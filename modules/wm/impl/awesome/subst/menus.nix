{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  defaultVTCmd = config.attributes.vt.default.traits.command.binary;
  defaultVTExecCmd = appBareTerminalCmd config.attributes.vt.default.traits;
}
