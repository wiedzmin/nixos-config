{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  defaultVTCmd = config.attributes.vt.default.traits.command.binary;
  defaultVTExecCmd = appCmdFull config.attributes.vt.default.traits;
}
