{ config, inputs, lib, pkgs, ... }:

# TODO: review https://github.com/ysndr/blog/blob/e4588f821ce6aee9ec3688ee9af3d2e61e143530/blog.nix#L14
# TODO: implement utils collection like nixpkgs/lib + flake handle

let
  windowRulePlaceholders = {
    "class" = ''^@$'';
    "title" = ''(?i).*@.*'';
    "role" = ''^@$'';
    "instance" = ''^@$'';
  };
in
rec {
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
      propagatedBuildInputs = with pkgs; (packages ++ [ glibc ]);
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
  renderTimer = desc: boot: active: cal: persistent: unit: {
    description = "${desc}";
    wantedBy = [ "timers.target" ];
    timerConfig = lib.optionalAttrs (boot != "") { OnBootSec = "${boot}"; }
      // lib.optionalAttrs (active != "") { OnUnitActiveSec = "${active}"; }
      // lib.optionalAttrs (cal != "") { OnCalendar = "${cal}"; }
      // lib.optionalAttrs (persistent) { Persistent = "true"; }
      // lib.optionalAttrs (unit != "") { Unit = "${unit}"; };
  };
  mkIndent = width: with lib; (concatStrings (genList (const " ") width));
  mkNewlineAndIndent = width: with lib; "\n" + (concatStrings (genList (const " ") width));
  mapMimesToApp = mimes: app: lib.genAttrs mimes (_: [ app ]);
  homePrefix = user: suffix: "/home/${user}/" + suffix;
  goBinPrefix = user: suffix: "/home/${user}/workspace/go/bin/" + suffix;
  xdgConfig = user: suffix: (homePrefix user ".config") + suffix;
  configPrefix = suffix:
    "${wsRoot "github"}/wiedzmin/nixos-config/" + suffix;
  secretsPrefix = suffix:
    configPrefix ("machines/" + config.attributes.machine.name + "/secrets/" + suffix);
  assetsPrefix = suffix:
    configPrefix ("machines/" + config.attributes.machine.name + "/assets/" + suffix);
  fromYAML = yaml:
    builtins.fromJSON (builtins.readFile (pkgs.runCommand "from-yaml"
      {
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
    builtins.readFile (pkgs.runCommand "to-toml"
      {
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
  wsRootAtHomedir = user: key: lib.removePrefix (homePrefix user "") key;
  mkGithubBookmark = user: repo: {
    local.path = "${wsRoot "github"}/${user}/${repo}";
    remote.url = "https://github.com/${user}/${repo}";
  };
  mkGithubBookmarkWithMyrepos = user: repo: {
    local.path = "${wsRoot "github"}/${user}/${repo}";
    remote.url = "https://github.com/${user}/${repo}";
    myrepos = {
      "${wsRoot "github"}/${user}/${repo}" = {
        checkout = [ "git clone 'https://github.com/${user}/${repo}.git' '${repo}'" ];
      };
    };
  };
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
  mkDerivationTmux =
    a@{ pluginName
    , rtpFilePath ? (builtins.replaceStrings [ "-" ] [ "_" ] pluginName) + ".tmux"
    , namePrefix ? "tmuxplugin-"
    , src
    , unpackPhase ? ""
    , postPatch ? ""
    , configurePhase ? ":"
    , buildPhase ? ":"
    , addonInfo ? null
    , preInstall ? ""
    , postInstall ? ""
    , path ? lib.getName pluginName
    , dependencies ? [ ]
    , ...
    }:
    let rtpPath = "share/tmux-plugins";
    in
    addRtpTmux "${rtpPath}/${path}" rtpFilePath a (pkgs.stdenv.mkDerivation (a // {
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
  readSubstituted = substs: entries:
    lib.concatStringsSep "\n" (lib.forEach entries
      (e: builtins.readFile (pkgs.substituteAll
        (lib.foldl (collector: subst: collector // ((import subst { inherit config inputs lib pkgs; }) // { src = e; }))
          {} substs))));
  enabledLocals = bookmarks:
    lib.mapAttrs
      (key: meta: meta.local // { key = key; } // (lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { tags = meta.tags; })
        // lib.optionalAttrs (lib.hasAttrByPath [ "desc" ] meta) { desc = meta.desc; })
      (lib.filterAttrs
        (_: meta:
          !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
          && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != ""))
        bookmarks);
  localEbooks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta:
        !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
        && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != "")
        && (lib.hasAttrByPath [ "local" "ebooks" ] meta && meta.local.ebooks == true))
      bookmarks);
  localDocs = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta:
        !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
        && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != "")
        && (lib.hasAttrByPath [ "local" "docs" ] meta && meta.local.docs == true))
      bookmarks);
  localEmacsBookmarks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta:
        !(lib.hasAttrByPath [ "local" "enable" ] meta && meta.local.enable == false)
        && (lib.hasAttrByPath [ "local" "path" ] meta && meta.local.path != ""))
      bookmarks);
  localBookmarksCommon = locals: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameLocal meta sep tagSep) (mkBookmarkDestLocal meta)) locals;
  localBookmarksKVText = locals:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (id: meta: id + " : " + meta.path) locals);
  enabledRemotes = bookmarks:
    lib.mapAttrs
      (_: meta: meta.remote // (lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { tags = meta.tags; })
        // lib.optionalAttrs (lib.hasAttrByPath [ "desc" ] meta) { desc = meta.desc; })
      (lib.filterAttrs
        (_: meta:
          !(lib.hasAttrByPath [ "remote" "enable" ] meta && meta.remote.enable == false)
          && (lib.hasAttrByPath [ "remote" "url" ] meta && meta.remote.url != ""))
        bookmarks);
  collectReposMetadata = bookmarks:
    (lib.mapAttrs'
      (_: meta:
        lib.nameValuePair (builtins.head (builtins.attrNames meta.myrepos))
          (builtins.head (builtins.attrValues meta.myrepos)))
      (lib.filterAttrs (_: meta: lib.hasAttrByPath [ "myrepos" ] meta && meta.myrepos != { }) bookmarks));
  mkBookmarkNameLocal = meta: sep: tagSep:
    lib.concatStringsSep sep (builtins.filter (e: e != "") ([ meta.key ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ]
      ++ [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [ ] meta)) ]));
  mkBookmarkDestLocal = meta:
    { path = meta.path; } // lib.optionalAttrs (lib.hasAttrByPath [ "shell" ] meta) { shell = meta.shell; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tmux" ] meta) { tmux = meta.tmux; };
  mkBookmarkNameRemote = meta: sep: tagSep:
    lib.concatStringsSep sep (builtins.filter (e: e != "") ([ meta.url ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ]
      ++ [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [ ] meta)) ]));
  mkBookmarkWebjumpDest = meta:
    { url = meta.url; } // lib.optionalAttrs (lib.hasAttrByPath [ "vpn" ] meta) { vpn = meta.vpn; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "browser" ] meta) { browser = meta.browser; };
  mkBookmarkSearchengineDest = meta:
    {
      url = meta.url + lib.optionalString (lib.hasAttrByPath [ "searchSuffix" ] meta) meta.searchSuffix;
    } // lib.optionalAttrs (lib.hasAttrByPath [ "vpn" ] meta) { vpn = meta.vpn; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "browser" ] meta) { browser = meta.browser; };
  remoteWebjumps = remotes: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameRemote meta sep tagSep) (mkBookmarkWebjumpDest meta))
      (lib.filterAttrs
        (_: meta:
          (lib.hasAttrByPath [ "jump" ] meta && meta.jump == true) || !(lib.hasAttrByPath [ "searchSuffix" ] meta))
        remotes);
  remoteSearchEngines = remotes: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameRemote meta sep tagSep) (mkBookmarkSearchengineDest meta))
      (lib.filterAttrs (_: meta: (lib.hasAttrByPath [ "searchSuffix" ] meta)) remotes);
  windowRulesFromBookmarks = bookmarks:
    lib.foldl (a: b: a ++ b) [ ]
      (lib.mapAttrsToList (_: meta: meta.windowRules)
        (lib.filterAttrs (_: meta: lib.hasAttrByPath [ "windowRules" ] meta) bookmarks));
  concatStringListsQuoted = sep: ll: lib.concatStringsSep sep (lib.forEach (lib.flatten ll) (x: ''"'' + x + ''"''));
  concatStringListsRaw = sep: ll: lib.concatStringsSep sep (lib.flatten ll);
  takeLast = n: l: with lib; reverseList (take n (reverseList l));
  mkWSMappingBrowsersRegexp =
    concatStringListsRaw "|" (with config.attributes.browser; [ default.windowClass fallback.windowClass ]);
  mkWSMappingEbookReadersRegexp =
    concatStringListsRaw "|" (with config.attributes.ebookreader; [ default.windowClass fallback.windowClass ]);
  mkWSMappingEbookReadersExtsRegexp = "(" + (concatStringListsRaw "|" config.content.ebooks.extensions.primary) + ")";
  # TODO: create function for ensuring non-prefix keys absence
  mkEmacsCustomKeymap = name: binding: ''
    (define-prefix-command '${name})
    (define-key global-map (kbd "${binding}") '${name})
  '';
  # TODO: consider adding keymap prompts
  genEmacsCustomKeymaps = meta:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: binding: mkEmacsCustomKeymap name binding) meta);
  emacsCmd = elisp:
    let emacsServerSocketPath = "/run/user/${config.attributes.mainUser.ID}/emacs/server";
    in "[ -f ${emacsServerSocketPath} ] && ${config.ide.emacs.core.package}/bin/emacsclient -s ${emacsServerSocketPath} -e '${elisp}'";
  mkArbttProgramTitleRule = windowClasses: titles: tag:
    lib.concatStringsSep "\n" (lib.forEach titles (t:
      "current window ($program == [${
        concatStringListsQuoted ", " windowClasses
      }] && $title =~ m!${t}!) ==> tag ${tag},"));
  mkArbttProgramRule = windowClasses: tag:
    "current window ($program == [${concatStringListsQuoted ", " windowClasses}]) ==> tag ${tag},";
  mkArbttTitleRule = titles: tag:
    lib.concatStringsSep "\n" (lib.forEach titles (t: "current window ($title =~ m!${t}!) ==> tag ${tag},"));
  mkArbttProgramMultipleTagsRule = program: tags:
    lib.concatStringsSep "\n" (lib.forEach tags (tag: ''current window $program == "${program}" ==> tag ${tag},''));
  mkArbttPrefixedTitlesRule = titles: prefix:
    lib.concatStringsSep "\n" (lib.forEach titles (t: "current window ($title =~ m!${t}!) ==> tag ${prefix}${t},"));
  mkArbttBrowserTitleRule = titles: tag:
    mkArbttProgramTitleRule (with config.attributes.browser; [ default.windowClass fallback.windowClass ]) titles tag;
  mkArbttProgramMapTitleRule = windowClasses: title2tag:
    lib.concatStringsSep "\n" (lib.mapAttrsToList
      (re: tag:
        "current window ($program == [${
        concatStringListsQuoted ", " windowClasses
      }] && $title =~ m!${re}!) ==> tag ${tag},")
      title2tag);
  mkArbttEmacsMapTitleRule = title2tag: # we could use similar helper functions for other IDEs
    lib.concatStringsSep "\n"
      (lib.mapAttrsToList (re: tag: "current window ($title =~ m!^emacs - [^ ]+\\.${re} .*$!) ==> tag ${tag},")
        title2tag);
  reAddWildcards = s: builtins.replaceStrings [ " " ] [ ".*" ] s;
  prepareWindowRule = rule:
    rule // (lib.mapAttrs (k: v: builtins.replaceStrings [ "@" ]
      [ (if k == "title" then reAddWildcards rule.${k} else rule.${k}) ] v)
        (lib.filterAttrs (k: _: builtins.hasAttr k rule) windowRulePlaceholders));
  getWorkspacesByType = wsdata: type: (lib.groupBy (x: x.snd.type) wsdata)."${type}";
  enumerateWorkspaces = wsdata: lib.zipLists (lib.imap1 (i: v: i) wsdata) wsdata;
  windowRuleClauses = rule:
    lib.filterAttrs (k: _: !builtins.elem k [ "activate" "debug" "desktop" "float" "key" "scratchpad" ]) rule;
  mkWMDebugScript = name: wmpkg: wmcmd:
    mkShellScriptWithDeps name
      (with pkgs; [
        coreutils
        gnugrep
        xorg.xorgserver.out
        xorg.xrandr
      ] ++ [ wmpkg ])
      ''
        if [ "$(xrandr | grep connected | grep -v dis | wc -l)" = "1" ]; then
          resolution=${config.attributes.hardware.monitors.internalHead.resolutionXephyr}
          echo "LVDS-only, using $resolution"
        else
          resolution=${config.attributes.hardware.monitors.internalHead.resolution}
          echo "dock-station, using $resolution"
        fi
        Xephyr -ac -br -noreset -screen $resolution :1 &
        sleep 1
        DISPLAY=:1.0 ${wmcmd}
      '';
}
