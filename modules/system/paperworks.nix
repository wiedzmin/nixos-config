{ config, lib, pkgs, ... }:
with lib;

let cfg = config.paperworks;
in {
  options = {
    paperworks = {
      printing.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable printing infrastructure.";
      };
      printing.drivers = mkOption {
        type = types.listOf types.path;
        default = [ ];
        example = [ pkgs.hplipWithPlugin ];
        description = "Printer drivers list.";
      };
      scanning.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable scanning infrastructure.";
      };
      scanning.extraBackends = mkOption {
        type = types.listOf types.path;
        default = [ ];
        example = [ pkgs.epkowa ];
        description = "Extra SANE backends list.";
      };
      scanning.snapscan.enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether we are using Epson Snapscan series scanner which is
          currently true for this configuration.
        '';
      };
      scanning.snapscan.firmware = mkOption {
        type = types.str;
        default = "";
        description = "Path to snapscan firmware file.";
      };
      scanning.enableXsane = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to add XSane to PATH.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.printing.enable {
      assertions = [
        {
          assertion = cfg.printing.enable && cfg.printing.drivers != [];
          message = "paperwork: must provide at least one printer driver package.";
        }
      ];
      services.printing = {
        enable = true;
        drivers = cfg.printing.drivers;
        browsing = true;
        defaultShared = true;
        webInterface = true;
      };
      users.users."${config.attributes.mainUser.name}".extraGroups = [ "lp" ];
    })
    (mkIf cfg.scanning.enable {
      assertions = [
        {
          assertion = cfg.scanning.snapscan.enable && cfg.scanning.snapscan.firmware != "";
          message = "paperwork: must provide firmware file if snapscan is enabled.";
        }
      ];
      hardware.sane = {
        enable = true;
        extraBackends = cfg.scanning.extraBackends;
      };

      services.saned.enable = true;

      nixpkgs.config = optionalAttrs cfg.scanning.snapscan.enable {
        sane.snapscanFirmware = cfg.scanning.snapscan.firmware;
      };
      environment.systemPackages = with pkgs; [
        deskew
        scantailor-advanced
        simple-scan
        utsushi
      ] ++ optional cfg.scanning.enableXsane [
        (xsane.override { gimpSupport = true; })
      ];
      users.users."${config.attributes.mainUser.name}".extraGroups = [ "scanner" ];
    })
  ];
}
