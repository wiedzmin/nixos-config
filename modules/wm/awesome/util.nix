{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

let
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
in
rec {
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
  mkKeybindingAwesome = meta: desktops:
    let
      debugEnabled = maybeAttrIsBool "debug" meta && !maybeAttrIsBool "raw" meta;
    in
    ''
      ["${mkKeysymAwesome meta.key}"] = function()
        ${if maybeAttrIsBool "raw" meta then "${mkIndent 2}${meta.cmd}"
          else ''${mkIndent 2}awful.spawn("${lib.optionalString (debugEnabled) "DEBUG_MODE=1 "}${meta.cmd}")${
            lib.optionalString (debugEnabled) " > ${
              mkCmdDebugAbsFilename config.controlcenter.commandsDebugLogRoot meta.cmd} 2>&1"}''}
      end
    '';
  # TODO: review floating property for window rules, regardless of WM being used
}
