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
        ];
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
  ];
}
