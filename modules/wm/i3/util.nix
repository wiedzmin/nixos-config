{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

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
in
rec {
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
      ++ lib.optionals (!maybeAttrIsBool "raw" meta) [ "exec" ]
      ++ lib.optionals (maybeAttrIsBool "transient" meta) [ "--no-startup-id" ] ++ [
      (builtins.concatStringsSep "; " (lib.optionals (builtins.hasAttr "cmd" meta)
        [
          ((lib.optionalString (debugEnabled) "DEBUG_MODE=1 ") + meta.cmd + lib.optionalString (debugEnabled)
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
        (lib.groupBy (x: x.mode) keys)).${scratchpadModeToken};
      scratchpadRules = builtins.filter (r: builtins.hasAttr "scratchpad" r && builtins.hasAttr "key" r) rules;
    in
    lib.concatStringsSep "\n"
      (lib.forEach scratchpadRules
        (r: "for_window ${mkWindowRuleI3 r} move scratchpad")) +
    ''


        mode "${scratchpadModeToken}" {
          ${lib.concatStringsSep (mkNewlineAndIndent 2) (lib.forEach
            (map (r: {
              key = r.key;
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
        if (builtins.hasAttr "activate" r && r.activate == true) then
          "; workspace ${getWorkspaceByNameI3 wsdata r.desktop}"
        else
          ""
      }"));
}
