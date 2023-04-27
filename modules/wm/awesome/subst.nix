{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  awesomeDebugPrint = "";
  defaultVTCmd = builtins.head config.attributes.vt.default.cmd;
  defaultVTExecCmd = config.attributes.vt.default.cmd;
  mainUserName = config.attributes.mainUser.name;
  placementRulesAwesomeList = genPlacementRulesAwesomeList config.wmCommon.wsMapping.rules 3;
  sloppyFocus = lib.boolToString config.wmCommon.focus.followsMouse;
  wmFontSimple = config.wmCommon.fonts.simple;
  wmPrefix = config.wmCommon.prefix;
  wmPrefixAlt = config.wmCommon.prefixAlt;
}
