{ lib, ... }:

# TODO: review https://github.com/ysndr/blog/blob/e4588f821ce6aee9ec3688ee9af3d2e61e143530/blog.nix#L14
# TODO: implement utils collection like nixpkgs/lib + flake handle

rec {
  addBuildInputs = pkg: ins: pkg.overrideAttrs (attrs: { buildInputs = attrs.buildInputs ++ ins; });
  withPatches = pkg: patches: lib.overrideDerivation pkg (_: { inherit patches; });
  mkPythonScriptWithDeps = nixpkgs: pname: packages: text:
    nixpkgs.python3Packages.buildPythonPackage rec {
      inherit pname;
      version = "unstable";
      src = nixpkgs.writeTextFile {
        name = "${pname}.py";
        text = ''
          #!${nixpkgs.python3}/bin/python3
          ${text}'';
        executable = true;
      };
      format = "other";
      unpackPhase = "true";
      buildInputs = with nixpkgs; [ makeWrapper ];
      propagatedBuildInputs = with nixpkgs; (packages ++ [ glibc ]);
      buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${pname}";
      installPhase = "true";
      postInstall = ''
        chmod a+x $out/bin/${pname}
      '';
    };
  renderTimer = desc: boot: active: cal: persistent: unit: {
    description = "${desc}";
    wantedBy = [ "timers.target" ];
    timerConfig = lib.optionalAttrs (boot != "") { OnBootSec = "${boot}"; }
      // lib.optionalAttrs (active != "") { OnUnitActiveSec = "${active}"; }
      // lib.optionalAttrs (cal != "") { OnCalendar = "${cal}"; }
      // lib.optionalAttrs persistent { Persistent = "true"; }
      // lib.optionalAttrs (unit != "") { Unit = "${unit}"; };
  };
  mkIndent = width: with lib; (concatStrings (genList (const " ") width));
  mkNewlineAndIndent = width: with lib; "\n" + (concatStrings (genList (const " ") width));
  mapMimesToApp = mimes: app: lib.genAttrs mimes (_: [ app ]);
  homePrefix = user: suffix: "/home/${user}/" + suffix;
  goBinPrefix = user: suffix: "/home/${user}/workspace/go/bin/" + suffix;
  xdgConfig = user: suffix: (homePrefix user ".config") + suffix; # FIXME: deal with slashes seamlessly
  wsRoot = roots: key: lib.getAttrFromPath [ key ] roots;
  configPrefix = roots: suffix: "${wsRoot roots "github"}/wiedzmin/nixos-config/" + suffix;
  secretsPrefix = machine: suffix: roots: configPrefix roots ("machines/" + machine + "/secrets/" + suffix);
  assetsPrefix = machine: suffix: roots: configPrefix roots ("machines/" + machine + "/assets/" + suffix);
  maybeAttrIsBool = name: set: (builtins.hasAttr name set) && set."${name}";
  maybeAttrString = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else ph;
  maybeAttrList = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else [ ph ];
  emacsBoolToString = v: if v then "t" else "nil";
  wsRootAtHomedir = user: key: lib.removePrefix (homePrefix user "") key;
  mkGithubBookmark = user: repo: roots: {
    local.path = "${wsRoot roots "github"}/${user}/${repo}";
    remote.url = "https://github.com/${user}/${repo}";
  };
  mkGithubBookmarkWithMyrepos = user: repo: roots: {
    local.path = "${wsRoot roots "github"}/${user}/${repo}";
    remote.url = "https://github.com/${user}/${repo}";
    batchvcs = {
      "${wsRoot roots "github"}/${user}/${repo}" = {
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
  readSubstituted = config: inputs: pkgs: substs: entries:
    lib.concatStringsSep "\n" (lib.forEach entries
      (e: builtins.readFile (pkgs.substituteAll
        (lib.foldl (collector: subst: collector // ((import subst { inherit config inputs lib pkgs; }) // { src = e; }))
          { }
          substs))));
  emptyValueByPath = meta: path: (lib.attrByPath path "" meta) == "";
  nonEmptyValueByPath = meta: path: !(emptyValueByPath meta path);
  trueValueByPath = meta: path: (lib.attrByPath path true meta) == true;
  trueValueByPathStrict = meta: path: (lib.attrByPath path false meta) == true;
  falseValueByPath = meta: path: !(trueValueByPath meta path);
  checkedBookmarks = bookmarks: checkPath:
    (lib.filterAttrs
      (_: meta:
        trueValueByPath meta [ "enable" ] &&
        nonEmptyValueByPath meta checkPath)
      bookmarks);
  localFiles = type: bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta: trueValueByPathStrict meta [ "local" type ])
      (checkedBookmarks bookmarks [ "local" "path" ]));
  localEmacsBookmarks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (checkedBookmarks bookmarks [ "local" "path" ]);
  localBookmarksKeyMeta = bookmarks: sep: tagSep:
    lib.mapAttrs'
      (key: meta: lib.nameValuePair (mkBookmarkNameLocal (meta // { inherit key; }) sep tagSep)
        (mkBookmarkDestLocal meta.local))
      (checkedBookmarks bookmarks [ "local" "path" ]);
  localBookmarksKVText = bookmarks:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (id: meta: id + " : " + meta.local.path) (checkedBookmarks bookmarks [ "local" "path" ]));
  mkBookmarkNameLocal = meta: sep: tagSep:
    lib.concatStringsSep sep (builtins.filter (e: e != "") ([ meta.key ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ]
      ++ [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [ ] meta)) ]));
  mkBookmarkDestLocal = meta:
    { inherit (meta) path; } // lib.optionalAttrs (lib.hasAttrByPath [ "shell" ] meta) { inherit (meta) shell; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tmux" ] meta) { inherit (meta) tmux; };
  mkBookmarkNameRemote = meta: sep: tagSep:
    lib.concatStringsSep sep (builtins.filter (e: e != "") ([ meta.remote.url ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ]
      ++ [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [ ] meta)) ]));
  mkBookmarkWebjumpDest = meta:
    { inherit (meta.remote) url; } // lib.optionalAttrs (lib.hasAttrByPath [ "remote" "vpn" ] meta) { inherit (meta.remote) vpn; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "remote" "browser" ] meta) { inherit (meta.remote) browser; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { inherit (meta) tags; };
  mkBookmarkSearchengineDest = meta:
    {
      url = meta.remote.url + lib.optionalString (lib.hasAttrByPath [ "remote" "searchSuffix" ] meta) meta.remote.searchSuffix;
    } // lib.optionalAttrs (lib.hasAttrByPath [ "remote" "vpn" ] meta) { inherit (meta.remote) vpn; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "remote" "browser" ] meta) { inherit (meta.remote) browser; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { inherit (meta) tags; };
  remoteWebjumps = remotes: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameRemote meta sep tagSep) (mkBookmarkWebjumpDest meta))
      (lib.filterAttrs
        (_: meta: (trueValueByPath meta [ "remote" "jump" ] || falseValueByPath meta [ "remote" "searchSuffix" ]))
        (checkedBookmarks remotes [ "remote" "url" ]));
  remoteSearchEngines = remotes: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameRemote meta sep tagSep) (mkBookmarkSearchengineDest meta))
      (lib.filterAttrs
        (_: meta: lib.hasAttrByPath [ "remote" "searchSuffix" ] meta)
        (checkedBookmarks remotes [ "remote" "url" ]));
  concatStringListsQuoted = sep: ll: lib.concatStringsSep sep (lib.forEach (lib.flatten ll) (x: ''"'' + x + ''"''));
  concatStringListsRaw = sep: ll: lib.concatStringsSep sep (lib.flatten ll);
  takeLast = n: l: with lib; reverseList (take n (reverseList l));
  # TODO: create function for ensuring non-prefix keys absence
  mkEmacsCustomKeymap = name: binding: ''
    (define-prefix-command '${name})
    (define-key global-map (kbd "${binding}") '${name})
  '';
  # TODO: consider adding keymap prompts
  genEmacsCustomKeymaps = meta:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: binding: mkEmacsCustomKeymap name binding) meta);
  emacsCmd = uid: emacspkg: elisp:
    let emacsServerSocket = "/run/user/${uid}/emacs/server";
    in "[ -f ${emacsServerSocket} ] && ${emacspkg}/bin/emacsclient -s ${emacsServerSocket} -e '${elisp}'";
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
  mkArbttBrowserTitleRule = titles: tag: browser:
    mkArbttProgramTitleRule (with browser; [ default.windowClass fallback.windowClass ]) titles tag;
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
}
