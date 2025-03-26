{ config, lib, pkgs, ... }:

let
  cfg = config.workstation.power.auto-cpufreq;
  cfgFilename = "auto-cpufreq.conf";
  cfgFile = format.generate cfgFilename cfg.settings;
  format = pkgs.formats.ini { };
in
{
  options = {
    workstation.power.auto-cpufreq = {
      enable = lib.mkEnableOption "auto-cpufreq daemon";

      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.auto-cpufreq;
        description = "auto-cpufreq package to use.";
      };

      settings = lib.mkOption {
        description = ''
          Configuration for `auto-cpufreq`.

          The available options can be found in [the example configuration file](https://github.com/AdnanHodzic/auto-cpufreq/blob/v${pkgs.auto-cpufreq.version}/auto-cpufreq.conf-example).
        '';

        default = { };
        type = lib.types.submodule { freeformType = format.type; };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    systemd = {
      packages = [ cfg.package ];
      services.auto-cpufreq = {
        # Workaround for https://github.com/NixOS/nixpkgs/issues/81138
        wantedBy = [ "multi-user.target" ];
        path = with pkgs; [
          bash
          coreutils
        ];

        serviceConfig.WorkingDirectory = "";
        serviceConfig.ExecStart = [
          ""
          "${lib.getExe cfg.package} --daemon --config ${cfgFile}"
        ];
      };
    };
  };

  # uses attributes of the linked package
  meta = {
    buildDocsInSandbox = false;
    maintainers = with lib.maintainers; [ nicoo ];
  };
}
