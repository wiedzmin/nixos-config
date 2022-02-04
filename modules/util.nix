{ config, inputs, lib, pkgs, ... }:

# TODO: review https://github.com/ysndr/blog/blob/e4588f821ce6aee9ec3688ee9af3d2e61e143530/blog.nix#L14
# TODO: implement utils collection like nixpkgs/lib + flake handle

rec {
  addBuildInputs = pkg: ins: pkg.overrideAttrs (attrs: { buildInputs = attrs.buildInputs ++ ins; });
  withPatches = pkg: patches: lib.overrideDerivation pkg (_: { inherit patches; });
  mkPythonScriptWithDeps = pname: packages: text:
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
  configPrefix = suffix:
    "${wsRoot "github"}/wiedzmin/nixos-config/" + suffix;
  secretsPrefix = suffix:
    configPrefix ("machines/" + config.attributes.machine.name + "/secrets/" + suffix);
  assetsPrefix = suffix:
    configPrefix ("machines/" + config.attributes.machine.name + "/assets/" + suffix);
  maybeAttrIsBool = name: set: (builtins.hasAttr name set) && set."${name}";
  maybeAttrString = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else ph;
  maybeAttrList = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else [ ph ];
  emacsBoolToString = v: if v then "t" else "nil";
  wsRoot = key: lib.getAttrFromPath [ key ] config.navigation.bookmarks.workspaces.roots;
  wsRootAtHomedir = user: key: lib.removePrefix (homePrefix user "") key;
  mkGithubBookmark = user: repo: {
    local.path = "${wsRoot "github"}/${user}/${repo}";
    remote.url = "https://github.com/${user}/${repo}";
  };
  mkGithubBookmarkWithMyrepos = user: repo: {
    local.path = "${wsRoot "github"}/${user}/${repo}";
    remote.url = "https://github.com/${user}/${repo}";
    batchvcs = {
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
  readSubstituted = substs: entries:
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
  enabledLocals = bookmarks:
    lib.mapAttrs
      (key: meta: meta.local // { inherit key; } // (lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { inherit (meta) tags; })
        // lib.optionalAttrs (lib.hasAttrByPath [ "desc" ] meta) { inherit (meta) desc; })
      (lib.filterAttrs
        (_: meta: trueValueByPath meta [ "local" "enable" ] && nonEmptyValueByPath meta [ "local" "path" ])
        bookmarks);
  localEbooks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta:
        trueValueByPath meta [ "enable" ] &&
        nonEmptyValueByPath meta [ "local" "path" ] &&
        trueValueByPathStrict meta [ "local" "ebooks" ])
      bookmarks);
  localDocs = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta:
        trueValueByPath meta [ "enable" ] &&
        nonEmptyValueByPath meta [ "local" "path" ] &&
        trueValueByPathStrict meta [ "local" "docs" ])
      bookmarks);
  localEmacsBookmarks = bookmarks:
    lib.mapAttrsToList (_: meta: meta.local.path) (lib.filterAttrs
      (_: meta: trueValueByPath meta [ "local" "enable" ] && nonEmptyValueByPath meta [ "local" "path" ])
      bookmarks);
  localBookmarksCommon = locals: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameLocal meta sep tagSep) (mkBookmarkDestLocal meta)) locals;
  localBookmarksKVText = locals:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (id: meta: id + " : " + meta.path) locals);
  enabledRemotes = bookmarks:
    lib.mapAttrs
      (_: meta: meta.remote // (lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { inherit (meta) tags; })
        // lib.optionalAttrs (lib.hasAttrByPath [ "desc" ] meta) { inherit (meta) desc; })
      (lib.filterAttrs
        (_: meta: trueValueByPath meta [ "remote" "enable" ] && nonEmptyValueByPath meta [ "remote" "url" ])
        bookmarks);
  mkBookmarkNameLocal = meta: sep: tagSep:
    lib.concatStringsSep sep (builtins.filter (e: e != "") ([ meta.key ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ]
      ++ [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [ ] meta)) ]));
  mkBookmarkDestLocal = meta:
    { inherit (meta) path; } // lib.optionalAttrs (lib.hasAttrByPath [ "shell" ] meta) { inherit (meta) shell; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tmux" ] meta) { inherit (meta) tmux; };
  mkBookmarkNameRemote = meta: sep: tagSep:
    lib.concatStringsSep sep (builtins.filter (e: e != "") ([ meta.url ] ++ [ (lib.attrByPath [ "desc" ] "" meta) ]
      ++ [ (lib.concatStringsSep tagSep (lib.attrByPath [ "tags" ] [ ] meta)) ]));
  mkBookmarkWebjumpDest = meta:
    { inherit (meta) url; } // lib.optionalAttrs (lib.hasAttrByPath [ "vpn" ] meta) { inherit (meta) vpn; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "browser" ] meta) { inherit (meta) browser; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { inherit (meta) tags; };
  mkBookmarkSearchengineDest = meta:
    {
      url = meta.url + lib.optionalString (lib.hasAttrByPath [ "searchSuffix" ] meta) meta.searchSuffix;
    } // lib.optionalAttrs (lib.hasAttrByPath [ "vpn" ] meta) { inherit (meta) vpn; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "browser" ] meta) { inherit (meta) browser; }
    // lib.optionalAttrs (lib.hasAttrByPath [ "tags" ] meta) { inherit (meta) tags; };
  remoteWebjumps = remotes: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameRemote meta sep tagSep) (mkBookmarkWebjumpDest meta))
      (lib.filterAttrs
        (_: meta:
          (lib.hasAttrByPath [ "jump" ] meta && meta.jump) || !(lib.hasAttrByPath [ "searchSuffix" ] meta))
        remotes);
  remoteSearchEngines = remotes: sep: tagSep:
    lib.mapAttrs' (_: meta: lib.nameValuePair (mkBookmarkNameRemote meta sep tagSep) (mkBookmarkSearchengineDest meta))
      (lib.filterAttrs (_: meta: (lib.hasAttrByPath [ "searchSuffix" ] meta)) remotes);
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
}
