{ config, inputs, lib, pkgs, ... }:

let
  user = config.attributes.mainUser.name;
  configBasePath = "/etc/nixos";
in rec {
  addBuildInputs = pkg: ins: pkg.overrideAttrs (attrs: { buildInputs = attrs.buildInputs ++ ins; });
  withPatches = pkg: patches: lib.overrideDerivation pkg (_: { inherit patches; });
  mkPythonScriptWithDeps = pname: packages: text: buildPythonScriptWithDeps pname false packages text;
  mkPythonScriptWithDepsAndConflicts = pname: packages: text: buildPythonScriptWithDeps pname true packages text;
  buildPythonScriptWithDeps = pname: allowConflicts: packages: text:
    pkgs.python3Packages.buildPythonPackage rec {
      inherit pname;
      version = "unstable";
      src = pkgs.writeTextFile {
        name = "${pname}.py";
        text = ''
          #!${pkgs.python3}/bin/python3
          ${text}'';
        executable = true;
      };
      format = "other";
      unpackPhase = "true";
      buildInputs = with pkgs; [ makeWrapper ];
      propagatedBuildInputs = with pkgs; packages;
      dontUsePythonCatchConflicts = allowConflicts;
      buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${pname}";
      installPhase = "true";
      postInstall = ''
        chmod a+x $out/bin/${pname}
      '';
    };
  mkShellScriptWithDeps = name: packages: contents:
    pkgs.stdenv.mkDerivation rec {
      inherit name;
      version = "unstable";
      src = pkgs.writeTextFile {
        name = "${name}.sh";
        text = ''
          #!${pkgs.stdenv.shell}
          ${contents}
        '';
        executable = true;
      };
      unpackPhase = "true";
      buildInputs = with pkgs; [ makeWrapper ];
      propagatedBuildInputs = with pkgs; packages;
      buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${name}";
      installPhase = ''
        mkdir -p $out/bin
        wrapProgram $out/bin/${name} \
          --prefix PATH : ${pkgs.lib.makeBinPath packages}
      '';
      postInstall = ''
        chmod a+x $out/bin/${name}
      '';
    };
  renderTimer = desc: boot: active: cal: {
    description = "${desc}";
    wantedBy = [ "timers.target" ];
    timerConfig = lib.optionalAttrs (boot != "") { OnBootSec = "${boot}"; }
      // lib.optionalAttrs (active != "") { OnUnitActiveSec = "${active}"; }
      // lib.optionalAttrs (cal != "") { OnCalendar = "${cal}"; };
  };
  mkIndent = width: with lib; (concatStrings (genList (const " ") width));
  mkNewlineAndIndent = width: with lib; "\n" + (concatStrings (genList (const " ") width));
  mapMimesToApp = mimes: app: lib.genAttrs mimes (_: [ app ]);
  homePrefix = suffix: "/home/${user}/" + suffix;
  xdgConfig = suffix: (homePrefix ".config") + suffix;
  secretsPrefix = suffix: configBasePath + "/machines/" + config.attributes.machine.name + "/secrets/" + suffix;
  assetsPrefix = suffix: configBasePath + "/machines/" + config.attributes.machine.name + "/assets/" + suffix;
  fromYAML = yaml:
    builtins.fromJSON (builtins.readFile (pkgs.runCommand "from-yaml" {
      inherit yaml;
      allowSubstitutes = false;
      preferLocalBuild = true;
    } ''
      ${pkgs.remarshal}/bin/remarshal  \
        -if yaml \
        -i <(echo "$yaml") \
        -of json \
        -o $out
    ''));
  toToml = attrs:
    builtins.readFile (pkgs.runCommand "to-toml" {
      # inherit attrs;
      allowSubstitutes = false;
      preferLocalBuild = true;
    } ''
      ${pkgs.remarshal}/bin/remarshal  \
        -if json \
        -of toml \
        < ${pkgs.writeText "attrs.json" (builtins.toJSON attrs)} \
        > $out
    '');
  maybeAttrIsBool = name: set: (builtins.hasAttr name set) && (set."${name}" == true);
  maybeAttrString = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else ph;
  maybeAttrList = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else [ ph ];
  emacsBoolToString = v: if v == true then "t" else "nil";
  wsRoot = key: lib.getAttrFromPath [ key ] config.navigation.bookmarks.workspaces.roots;
  wsRootAbs = key: homePrefix (wsRoot key);
  selectorFunction = lib.mkOptionType {
    name = "selectorFunction";
    description = "Function that takes an attribute set and returns a list"
      + " containing a selection of the values of the input set";
    check = lib.isFunction;
    merge = _loc: defs: as: lib.concatMap (select: select as) (lib.getValues defs);
  };
  # tmux plugin utils
  addRtpTmux = path: rtpFilePath: attrs: derivation:
    derivation // {
      rtp = "${derivation}/${path}/${rtpFilePath}";
    } // {
      overrideAttrs = f: mkDerivationTmux (attrs // f attrs);
    };
  mkDerivationTmux = a@{ pluginName, rtpFilePath ? (builtins.replaceStrings [ "-" ] [ "_" ] pluginName) + ".tmux"
    , namePrefix ? "tmuxplugin-", src, unpackPhase ? "", postPatch ? "", configurePhase ? ":", buildPhase ? ":"
    , addonInfo ? null, preInstall ? "", postInstall ? "", path ? lib.getName pluginName, dependencies ? [ ], ... }:
    let rtpPath = "share/tmux-plugins";
    in addRtpTmux "${rtpPath}/${path}" rtpFilePath a (pkgs.stdenv.mkDerivation (a // {
      name = namePrefix + pluginName;

      inherit pluginName unpackPhase postPatch configurePhase buildPhase addonInfo preInstall postInstall;

      installPhase = ''
        runHook preInstall

        target=$out/${rtpPath}/${path}
        mkdir -p $out/${rtpPath}
        cp -r . $target
        if [ -n "$addonInfo" ]; then
          echo "$addonInfo" > $target/addon-info.json
        fi

        runHook postInstall
      '';

      dependencies = [ pkgs.bash ] ++ dependencies;
    }));
  readSubstituted = subst: content:
    builtins.readFile (pkgs.substituteAll ((import subst { inherit config inputs lib pkgs; }) // { src = content; }));
  enabledLocals = bookmarks:
    lib.mapAttrs (_: meta: meta.local // lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { tags = meta.tags; })
      (lib.filterAttrs (_: meta:
        !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
        && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != "")) bookmarks);
  localEbooks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs (_: meta:
      !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
      && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != "")
      && (lib.hasAttrByPath [ "local" "ebooks" ] meta && meta.local.ebooks == true)) bookmarks);
  localEmacsBookmarks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs (_: meta:
      !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
      && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != "")) bookmarks);
  localBookmarksKVText = locals:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (id: meta: id + " : " + meta.path) locals);
  enabledRemotes = bookmarks:
    lib.mapAttrs (_: meta: meta.remote // lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { tags = meta.tags; })
      (lib.filterAttrs (_: meta:
        !(lib.hasAttrByPath [ "remote" "enable" ] meta && meta.remote.enable == false)
        && (lib.hasAttrByPath [ "remote" "url" ] meta && meta.remote.url != "")) bookmarks);
  mkBookmarkName = meta: sep: tagSep:
    lib.concatStringsSep sep
      (builtins.filter (e: e != "")
        ([ meta.url ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ] ++
         [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [] meta)) ]));
  mkBookmarkDest = meta: {
    url = meta.url + lib.optionalString (lib.hasAttrByPath [ "searchSuffix" ] meta) meta.searchSuffix;
  } // lib.optionalAttrs (lib.hasAttrByPath [ "vpn" ] meta) { vpn = meta.vpn; }
  // lib.optionalAttrs (lib.hasAttrByPath [ "browser" ] meta) { browser = meta.browser; };
  remoteWebjumps = remotes: sep: tagSep: lib.mapAttrs'
    (_: meta: lib.nameValuePair (mkBookmarkName meta sep tagSep) (mkBookmarkDest meta))
    (lib.filterAttrs (_: meta:
      (lib.hasAttrByPath [ "searchSuffix" ] meta && lib.hasSuffix "+" meta.searchSuffix)
      || (lib.hasAttrByPath [ "jump" ] meta && meta.jump == true) || !(lib.hasAttrByPath [ "searchSuffix" ] meta))
      remotes);
  remoteSearchEngines = remotes: sep: tagSep: lib.mapAttrs'
    (_: meta: lib.nameValuePair (mkBookmarkName meta sep tagSep) (mkBookmarkDest meta))
    (lib.filterAttrs (_: meta:
      (lib.hasAttrByPath [ "searchSuffix" ] meta)) remotes);
  concatStringListsQuoted = sep: ll: lib.concatStringsSep sep (lib.forEach (lib.flatten ll) (x: ''"'' + x + ''"''));
  takeLast = n: l: with lib;
    reverseList (take n (reverseList l));
}
