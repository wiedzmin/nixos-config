{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with import ./util.nix { inherit config inputs lib pkgs; };

rec {
  awesomeDebugPrint = "";
  defaultVTCmd = builtins.head config.attributes.defaultVTCommand;
  defaultVTExecCmd = config.attributes.defaultVTCommand;
  mainUserName = config.attributes.mainUser.name;
  placementRulesAwesomeList = genPlacementRulesAwesomeList config.wmCommon.wsMapping.rules 3;
  sloppyFocus = lib.boolToString config.wmCommon.focus.followsMouse;
  wmFontSimple = config.wmCommon.fonts.simple;
  wmPrefix = config.wmCommon.prefix;
  wmPrefixAlt = config.wmCommon.prefixAlt;
}
