{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.custom.browsers.next;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.browsers.next = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Next'.
        '';
      };
      downloadPath = mkOption {
        type = types.str;
        default = homePrefix "Downloads";
        description = ''
          Downloads path'.
        '';
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Next should be the default browser.
        '';
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Next should be the fallback browser.
        '';
      };
      command = mkOption {
        type = types.str;
        default = "${pkgs.next}/bin/next";
        description = "Default command line to invoke";
      };
      staging.enableSettings = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable staging settings.";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ next ];
        xdg.configFile."next/init.lisp".text = readSubstituted ../../subst.nix ./init.lisp; # NOTE: actually absent
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      environment.sessionVariables = { BROWSER = cfg.command; };
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: next: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.chromium.isDefault && !config.custom.browsers.qutebrowser.isDefault
            && !config.custom.browsers.firefox.isDefault;
          message = "browsers: next: there should be exactly one default.";
        }
      ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "next.desktop";
      };
      attributes.browser.default = cfg.command;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: next: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.chromium.isFallback && !config.custom.browsers.qutebrowser.isFallback
            && !config.custom.browsers.firefox.isFallback;
          message = "browsers: next: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback = cfg.command;
    })
  ];
}
