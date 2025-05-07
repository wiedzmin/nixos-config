{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  placementRulesAwesomeList = genPlacementRulesAwesomeList config.wmCommon.wsMapping.rules 3;
  sloppyFocus = lib.boolToString config.wmCommon.focus.followsMouse;
}
