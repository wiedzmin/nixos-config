{ config, lib, pkgs, ... }:
with lib;

let cfg = config.tools.analysis;
in {
  options = {
    tools.analysis = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable dev analysis tool.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      elfinfo
      flamegraph
      gdb
      gdbgui
      hopper
      patchelf
      patchutils
      radare2
      radare2-cutter
      sysdig
      valgrind
    ];
  };
}
