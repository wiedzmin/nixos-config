{ config, lib, pkgs, ... }:
with lib;

let cfg = config.tools.security;
in {
  options = {
    # TODO: refine options
    tools.security = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable security tools.";
      };
    };
  };

  config = mkIf cfg.enable {
    home-manager.users."${config.attributes.mainUser.name}" = {
      home.packages = with pkgs; [
        # jd-gui
        (pass.withExtensions (ext: with ext; [ pass-audit pass-import pass-update ]))
        certigo
        dnsrecon
        gnupg
        gopass
        mkcert
        paperkey
        rofi-pass
        srm
        sslscan
        vulnix
      ];
    };
  };
}
