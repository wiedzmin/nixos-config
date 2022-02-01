{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };

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
in
rec {
  windowRulesFromBookmarks = bookmarks:
    lib.foldl (a: b: a ++ b) [ ]
      (lib.mapAttrsToList (_: meta: meta.windowRules)
        (lib.filterAttrs (_: meta: lib.hasAttrByPath [ "windowRules" ] meta) bookmarks));
  mkWSMappingBrowsersRegexp =
    concatStringListsRaw "|" (with config.attributes.browser; [ default.windowClass fallback.windowClass ]);
  mkWSMappingEbookReadersRegexp =
    concatStringListsRaw "|" (with config.attributes.ebookreader; [ default.windowClass fallback.windowClass ]);
  mkWSMappingEbookReadersExtsRegexp = "(" + (concatStringListsRaw "|" config.content.ebooks.extensions.primary) + ")";
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
  mkWMDebugScript = name: wmpkg: wmcmd:
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs; [
        coreutils
        gnugrep
        xorg.xorgserver.out
        xorg.xrandr
      ] ++ [ wmpkg ];
      text = ''
        if [ "$(xrandr | grep connected | grep -c dis)" = "1" ]; then
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
  mkKeybindingI3 = meta: desktops:
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
            " > ${mkCmdDebugAbsFilename config.controlcenter.commandsDebugLogRoot meta.cmd} 2>&1")
        ]
      ++ lib.optionals (builtins.hasAttr "desktop" meta)
        [ "workspace ${getWorkspaceByNameI3 desktops meta.desktop}" ]
      ++ lib.optionals (!maybeAttrIsBool "sticky" meta && meta.mode != "root") [ ''mode "default"'' ]))
    ]);
  bindkeysI3 = keys: modeBindings: exitBindings: desktops:
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
              }) exitBindings)) (x: mkKeybindingI3 x desktops))
          }
        }
      '') prefixedModesMeta)}
      ${lib.concatStringsSep "\n" (lib.forEach rootModeBindings (x: mkKeybindingI3 x desktops))}

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
  genScratchpadSettingsI3 = rules: keys: exitBindings: desktops:
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
            }) scratchpadRules) (x: mkKeybindingI3 x desktops))
          }
          ${lib.concatStringsSep (mkNewlineAndIndent 2) (lib.forEach
            (scratchpadBindings ++ (map (b: {
              key = b;
              mode = "${scratchpadModeToken}";
              raw = true;
            }) exitBindings)) (x: mkKeybindingI3 x desktops))
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
  mkKeybindingAwesome = meta: _:
    let
      debugEnabled = maybeAttrIsBool "debug" meta && !maybeAttrIsBool "raw" meta;
    in
    ''
      ["${mkKeysymAwesome meta.key}"] = function()
        ${if maybeAttrIsBool "raw" meta then "${mkIndent 2}${meta.cmd}"
          else ''${mkIndent 2}awful.spawn("${lib.optionalString debugEnabled "DEBUG_MODE=1 "}${meta.cmd}")${
            lib.optionalString debugEnabled " > ${
              mkCmdDebugAbsFilename config.controlcenter.commandsDebugLogRoot meta.cmd} 2>&1"}''}
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
  mkWorkspacesXmonad = wss: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList
        (ws: meta: ''("${ws}", Just "${convertKeyXmonad meta.key}", ${toHaskellBool meta.transient})'') wss)
    }${mkIndent indent}";
}
