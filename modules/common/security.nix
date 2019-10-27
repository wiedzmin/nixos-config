{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.security;
  emacsSecuritySetup = ''
    (use-package auth-source
      :custom
      ;;TODO: investigate and setup ghub according to https://github.com/magit/ghub/blob/master/ghub.org#user-content-manually-creating-and-storing-a-token
      ;;TODO: check if it needed and resurrect .authinfo.gpg
      (auth-sources '("~/.authinfo.gpg")))

    (use-package auth-source-pass
      :ensure t
      :after auth-source
      :config
      ;FIXME: source not being added
      (auth-source-pass-enable))

    (use-package keychain-environment
      :ensure t
      :hook
      (after-init-hook . keychain-refresh-environment))

    (use-package password-cache
      :custom
      (password-cache-expiry nil)
      (password-cache t))

    (use-package pass
      :ensure t
      :bind
      (:prefix-map custom-pass-map
                   :prefix "<f6>"
                   ("p" . pass)
                   ("!" . ivy-pass))
      :config
      (use-package ivy-pass :ensure t))
  '';
  cache_job_vpn_status_up = pkgs.writeShellScriptBin "cache_job_vpn_status_up" ''
    ${pkgs.redis}/bin/redis-cli set job_vpn_status up
  '';
  cache_job_vpn_status_down = pkgs.writeShellScriptBin "cache_job_vpn_status_down" ''
    ${pkgs.redis}/bin/redis-cli set job_vpn_status down
  '';
  enforce_job_vpn_up = pkgs.writeShellScriptBin "enforce_job_vpn_up" ''
    if [[ $(${pkgs.redis}/bin/redis-cli get job_vpn_status) != "up" ]]; then
      ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "VPN is off, turn it on and retry"
      exit 1
    fi
  '';
in {
  options = {
    # TODO: refine options
    custom.security = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable security tools.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs security-related setup.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
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
          # ===
          enforce_job_vpn_up
        ];
        programs.gpg = {
          enable = true;
          settings = {
            keyserver = "hkp://keys.openpgp.org";
            require-cross-certification = true;
            use-agent = true;
          };
        };
        services.gpg-agent = {
          enable = true;
          defaultCacheTtl = 34560000;
          defaultCacheTtlSsh = 34560000;
          maxCacheTtl = 34560000;
          enableSshSupport = true;
          enableExtraSocket = true;
          extraConfig = ''
            allow-emacs-pinentry
            allow-loopback-pinentry
          '';
        };
      };
      # FIXME: think how to make plug point(s) for secrets with example of this use case
      services.openvpn.servers."${config.secrets.job.vpn.name}" = {
        up = ''
          ${cache_job_vpn_status_up}/bin/cache_job_vpn_status_up
        '';
        down = ''
          ${cache_job_vpn_status_down}/bin/cache_job_vpn_status_down
        '';
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [
          epkgs.auth-source-pass
          epkgs.ivy-pass
          epkgs.pass
        ];
      };
      ide.emacs.config = ''${emacsSecuritySetup}'';
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "<XF86ScreenSaver>" = ''spawn "${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off"'';
        "M-a q" = ''spawn "${pkgs.rofi-pass}/bin/rofi-pass"'';
        "M-s s <Down>" = ''spawn "${pkgs.systemd}/bin/systemctl stop openvpn-${config.secrets.network.vpn.name}.service"'';
        "M-s s <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart openvpn-${config.secrets.network.vpn.name}.service"'';
        "M-s v <Down>" = ''spawn "${pkgs.systemd}/bin/systemctl stop openvpn-${config.secrets.job.vpn.name}.service"'';
        "M-s v <Up>" = ''spawn "${pkgs.systemd}/bin/systemctl restart openvpn-${config.secrets.job.vpn.name}.service"'';
      };
    })
  ];
}

# TODO: review https://github.com/faulesocke/pass-dmenu/blob/master/pass-dmenu.py and try to make own script
