{ lib }:

# TODO: review https://github.com/ysndr/blog/blob/e4588f821ce6aee9ec3688ee9af3d2e61e143530/blog.nix#L14

let
  keySepI3 = "+";
  keySymsI3 = {
    "," = "comma";
    "." = "period";
    "Esc" = "Escape";
    "`" = "grave";
    "<" = "less";
    ">" = "more"; # ERROR: Could not translate string to key symbol: "more"
    "=" = "equal";
  };
  scratchpadModeToken = "scratchpad";
  keySepAwesome = "-";
  keySymsAwesomeEZ = {
    "Control" = "C";
    "Shift" = "S";
    "Alt" = "A";
    "Mod4" = "M";
    "Left" = "<Left>";
    "Right" = "<Right>";
    "Up" = "<Up>";
    "Down" = "<Down>";
    "XF86AudioPrev" = "<XF86AudioPrev>";
    "XF86AudioPlay" = "<XF86AudioPlay>";
    "XF86AudioNext" = "<XF86AudioNext>";
    "XF86AudioRaiseVolume" = "<XF86AudioRaiseVolume>";
    "XF86AudioLowerVolume" = "<XF86AudioLowerVolume>";
    "XF86AudioMute" = "<XF86AudioMute>";
    "XF86MonBrightnessUp" = "<XF86MonBrightnessUp>";
    "XF86MonBrightnessDown" = "<XF86MonBrightnessDown>";
    "Print" = "<Print>";
  };
  keysymsXmonad = {
    "F1" = "<F1>";
    "F2" = "<F2>";
    "F3" = "<F3>";
    "F4" = "<F4>";
    "F5" = "<F5>";
    "F6" = "<F6>";
    "F7" = "<F7>";
    "F8" = "<F8>";
    "F9" = "<F9>";
    "F10" = "<F10>";
    "F11" = "<F11>";
    "F12" = "<F12>";
  };
  windowRulePlaceholders = {
    "class" = ''^@$'';
    "title" = ''(?i).*@.*'';
    "role" = ''^@$'';
    "instance" = ''^@$'';
  };
  approvedDefaultBrowsersWC = [ # NOTE: is this the best location for this metadata?
    "Chromium-browser"
    "Firefox"
  ];
in
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
  maybeDefaultBrowserCmd = browserDefault: browserFallback:
    if builtins.elem (lib.last browserDefault.windowClass) approvedDefaultBrowsersWC then
      browserDefault.cmd
    else
      browserFallback.cmd;
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
  dockableWS = headscount: if headscount > 2 then "secondary" else "primary";
  ####################################################################################################################
  #                                                   WM utils                                                       #
  ####################################################################################################################
  windowRulesFromBookmarks = bookmarks:
    lib.foldl (a: b: a ++ b) [ ]
      (lib.mapAttrsToList (_: meta: meta.windowRules)
        (lib.filterAttrs (_: meta: lib.hasAttrByPath [ "windowRules" ] meta) bookmarks));
  mkWSMappingBrowsersRegexp = browser:
    concatStringListsRaw "|" (with browser; [ default.windowClass fallback.windowClass ]);
  mkWSMappingEbookReadersRegexp = ebookreader:
    concatStringListsRaw "|" (with ebookreader; [ default.windowClass fallback.windowClass ]);
  mkWSMappingEbookReadersExtsRegexp = primexts: "(" + (concatStringListsRaw "|" primexts) + ")";
  prepareWindowRule = rule:
    rule // (lib.mapAttrs
      (k: v: builtins.replaceStrings [ "@" ]
        [ (if k == "title" then reAddWildcards rule."${k}" else rule."${k}") ]
        v)
      (lib.filterAttrs (k: _: builtins.hasAttr k rule) windowRulePlaceholders));
  getWorkspacesByType = wsdata: type: (lib.groupBy (x: x.snd.type) wsdata)."${type}";
  enumerateWorkspaces = wsdata: lib.zipLists (lib.imap1 (i: _: i) wsdata) wsdata;
  windowRuleClauses = rule:
    lib.filterAttrs (k: _: !builtins.elem k [ "activate" "debug" "desktop" "float" "key" "scratchpad" ]) rule;
  mkWMDebugScript = nixpkgs: name: wmpkg: internalHead: wmcmd:
    nixpkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with nixpkgs; [
        coreutils
        gnugrep
        xorg.xorgserver.out
        xorg.xrandr
      ] ++ [ wmpkg ];
      text = ''
        if [ "$(xrandr | grep connected | grep -c dis)" = "1" ]; then
          resolution=${internalHead.resolutionXephyr}
          echo "LVDS-only, using $resolution"
        else
          resolution=${internalHead.resolution}
          echo "dock-station, using $resolution"
        fi
        Xephyr -ac -br -noreset -screen $resolution :1 &
        sleep 1
        DISPLAY=:1.0 ${wmcmd}
      '';
    };
  mkWindowRuleI3 = rule:
    "[${lib.concatStringsSep " " (lib.mapAttrsToList (k: v: ''${k}="${v}"'')
      (windowRuleClauses (prepareWindowRule rule)))}]";
  mkKeysymI3 = keys:
    lib.concatStringsSep keySepI3
      (lib.forEach keys (k: if builtins.hasAttr k keySymsI3 then builtins.getAttr k keySymsI3 else k));
  getWorkspaceByNameI3 = wsdata: name:
    let ws = builtins.head (builtins.filter (ws: ws.snd.name == name) (enumerateWorkspaces wsdata));
    in "${builtins.toString ws.fst}: ${ws.snd.name}";
  mkWorkspacesI3 = wsdata: mod:
    lib.concatStringsSep "\n" (lib.forEach (enumerateWorkspaces wsdata) (elt: ''
      bindsym ${mod}+${mkKeysymI3 elt.snd.key} workspace ${builtins.toString elt.fst}: ${elt.snd.name}
      bindsym ${mod}+Shift+${mkKeysymI3 elt.snd.key} move container to workspace ${
        builtins.toString elt.fst
      }: ${elt.snd.name}
    ''));
  mvWorkspacesCmdI3 = wsdata: type: head:
    "${lib.concatStringsSep " " (lib.forEach (getWorkspacesByType (enumerateWorkspaces wsdata) type) (ws:
      "workspace --no-auto-back-and-forth ${
        builtins.toString ws.fst
      }: ${ws.snd.name}; move workspace to output ${head}; "))}";
  setWorkspacesLayoutByTypeI3 = wsdata: type: layout:
    "${lib.concatStringsSep " " (lib.forEach (getWorkspacesByType (enumerateWorkspaces wsdata) type) (ws:
      "workspace --no-auto-back-and-forth ${
        builtins.toString ws.fst
      }: ${ws.snd.name}; layout ${layout}; "))}";
  mkCmdDebugAbsFilename = root: cmd:
    "${root}/${
      lib.last (lib.splitString "/"
        (builtins.head (lib.splitString " " cmd)))}-$(date +%Y-%m-%d-%H-%M-%S | tr -d '[:cntrl:]').log";
  mkKeybindingI3 = meta: desktops: logsroot:
    let
      debugEnabled = maybeAttrIsBool "debug" meta && !maybeAttrIsBool "raw" meta;
    in
    builtins.concatStringsSep " " ([ "bindsym" (mkKeysymI3 meta.key) ]
      ++ lib.optionals (maybeAttrIsBool "leaveFullscreen" meta) [ "fullscreen disable;" ]
      ++ lib.optionals (!maybeAttrIsBool "raw" meta) [ "exec" ]
      ++ lib.optionals (maybeAttrIsBool "transient" meta) [ "--no-startup-id" ] ++ [
      (builtins.concatStringsSep "; " (lib.optionals (builtins.hasAttr "cmd" meta)
        [
          ((lib.optionalString debugEnabled "DEBUG_MODE=1 ") + meta.cmd + lib.optionalString debugEnabled
            " > ${mkCmdDebugAbsFilename logsroot meta.cmd} 2>&1")
        ]
      ++ lib.optionals (builtins.hasAttr "desktop" meta)
        [ "workspace ${getWorkspaceByNameI3 desktops meta.desktop}" ]
      ++ lib.optionals (!maybeAttrIsBool "sticky" meta && meta.mode != "root") [ ''mode "default"'' ]))
    ]);
  bindkeysI3 = keys: modeBindings: exitBindings: desktops: logsroot:
    let
      prefixedModesMeta = lib.filterAttrs (k: _: k != "root" && k != scratchpadModeToken) (lib.groupBy (x: x.mode) keys);
      rootModeBindings = (lib.filterAttrs (k: _: k == "root") (lib.groupBy (x: x.mode) keys)).root;
    in
    ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (mode: bindings: ''
        mode "${mode}" {
          ${
            lib.concatStringsSep (mkNewlineAndIndent 2) (lib.forEach (bindings
              ++ lib.optionals (!lib.hasPrefix "Passthrough" mode) (map (b: {
                key = b;
                mode = "${mode}";
                raw = true;
              }) exitBindings)) (x: mkKeybindingI3 x desktops logsroot))
          }
        }
      '') prefixedModesMeta)}
      ${lib.concatStringsSep "\n" (lib.forEach rootModeBindings (x: mkKeybindingI3 x desktops logsroot))}

      ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (mode: key: ''bindsym ${mkKeysymI3 key} mode "${mode}"'') modeBindings)}
    '';
  bindkeysFocusI3 = rules:
    lib.concatStringsSep "\n" (lib.forEach (builtins.filter (r: builtins.hasAttr "focus" r) rules)
      (r: "bindsym ${mkKeysymI3 r.focus} ${mkWindowRuleI3 r} focus"));
  genWindowRulesFloatI3 = rules:
    lib.concatStringsSep "\n" (lib.forEach (builtins.filter (r: builtins.hasAttr "float" r) rules) (r:
      "for_window ${mkWindowRuleI3 r} floating ${
        if r.float then
          "toggle${
            if builtins.hasAttr "resize" r then
              "; ${lib.concatStringsSep "; " (lib.forEach r.resize (d: "resize ${d}"))}"
            else
              ""
          }"
        else
          "disable"
      }"));
  mkScratchpadToggleI3 = x:
    "";
  genScratchpadSettingsI3 = rules: keys: exitBindings: desktops: logsroot:
    let
      scratchpadBindings = (lib.filterAttrs (k: _: k == scratchpadModeToken)
        (lib.groupBy (x: x.mode) keys))."${scratchpadModeToken}";
      scratchpadRules = builtins.filter (r: builtins.hasAttr "scratchpad" r && builtins.hasAttr "key" r) rules;
    in
    lib.concatStringsSep "\n"
      (lib.forEach scratchpadRules
        (r: "for_window ${mkWindowRuleI3 r} move scratchpad")) +
    ''


        mode "${scratchpadModeToken}" {
          ${lib.concatStringsSep (mkNewlineAndIndent 2) (lib.forEach
            (map (r: {
              inherit (r) key;
              mode = "${scratchpadModeToken}";
              cmd = "${mkWindowRuleI3 r} scratchpad show";
              raw = true;
            }) scratchpadRules) (x: mkKeybindingI3 x desktops logsroot))
          }
          ${lib.concatStringsSep (mkNewlineAndIndent 2) (lib.forEach
            (scratchpadBindings ++ (map (b: {
              key = b;
              mode = "${scratchpadModeToken}";
              raw = true;
            }) exitBindings)) (x: mkKeybindingI3 x desktops logsroot))
          }
        }
      '';
  genPlacementRulesI3 = rules: wsdata:
    lib.concatStringsSep "\n" (lib.forEach (builtins.filter (r: builtins.hasAttr "desktop" r) rules) (r:
      "for_window ${mkWindowRuleI3 r} move to workspace ${getWorkspaceByNameI3 wsdata r.desktop}${
        if (builtins.hasAttr "activate" r && r.activate) then
          "; workspace ${getWorkspaceByNameI3 wsdata r.desktop}"
        else
          ""
      }"));
  mkWindowRuleAwesome = rule: width: ''
    ${mkIndent width}{
       ${mkIndent width}rule = { ${lib.concatStringsSep ", " (lib.mapAttrsToList (k: v: ''${k}="${v}"'')
         (windowRuleClauses (prepareWindowRule rule)))} },
       ${mkIndent width}properties = { ${lib.concatStringsSep ", "
         ([] ++ lib.optionals (builtins.hasAttr "desktop" rule) [ "tag = '${rule.desktop}'" ]
          ++ lib.optionals (builtins.hasAttr "activate" rule) [ "switchtotag = true" ])} }
    ${mkIndent width}}'';
  genPlacementRulesAwesomePatch = rules: width: ''
    awful.rules.rules = {
    ${(lib.concatStringsSep ",\n" (lib.forEach (builtins.filter (r: builtins.hasAttr "desktop" r) rules)
      (r: mkWindowRuleAwesome r width)))}
    }'';
  genPlacementRulesAwesomeList = rules: width: ''
    ${(lib.concatStringsSep ",\n" (lib.forEach (builtins.filter (r: builtins.hasAttr "desktop" r) rules)
      (r: mkWindowRuleAwesome r width)))}
  '';
  mkKeysymAwesome = keys:
    lib.concatStringsSep keySepI3
      (lib.forEach keys (k: if builtins.hasAttr k keySymsAwesomeEZ then builtins.getAttr k keySymsAwesomeEZ else k));
  # TODO: investigate if we need to handle "desktop" attribute here (or elsewhere), and other misc attributes as well
  mkKeybindingAwesome = meta: logsroot: _:
    let
      debugEnabled = maybeAttrIsBool "debug" meta && !maybeAttrIsBool "raw" meta;
    in
    ''
      ["${mkKeysymAwesome meta.key}"] = function()
        ${if maybeAttrIsBool "raw" meta then "${mkIndent 2}${meta.cmd}"
          else ''${mkIndent 2}awful.spawn("${lib.optionalString debugEnabled "DEBUG_MODE=1 "}${meta.cmd}")${
            lib.optionalString debugEnabled " > ${
              mkCmdDebugAbsFilename logsroot meta.cmd} 2>&1"}''}
      end
    '';
  # TODO: review floating property for window rules, regardless of WM being used
  toHaskellBool = v: builtins.replaceStrings [ "true" "false" ] [ "True" "False" ] (lib.trivial.boolToString v);
  mkKeysymXmonad = key: if builtins.hasAttr key keysymsXmonad then builtins.getAttr key keysymsXmonad else key;
  mkKeysXmonadSpawn = keys: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList (key: meta:
        ''
          "${key}" ~> spawn "${meta.cmd}"${
            if lib.hasAttrByPath [ "desktop" ] meta then " >> showWSOnProperScreen \"${meta.desktop}\"" else ""
          }'') keys)
    }${mkIndent indent}";
  mkKeysXmonadRaw = keys: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList (key: cmd: ''"${key}" ~> ${cmd}'') keys)
    }${mkIndent indent}";
  convertKeyXmonad = key: ""; # FIXME: fuse
  mkWorkspacesXmonad = wss: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList
        (ws: meta: ''("${ws}", Just "${convertKeyXmonad meta.key}", ${toHaskellBool meta.transient})'') wss)
    }${mkIndent indent}";
}
