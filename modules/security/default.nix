{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.security;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  stable = import inputs.stable {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    ext.security = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable security tools.";
      };
      pinentryFlavor = mkOption {
        type = types.enum pkgs.pinentry.flavors;
        example = "gnome3";
        description = "Pinentry flavor to use.";
      };
      polkit.silentAuth = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to allow special users to do some things without authentication.";
      };
      passwordStorePath = mkOption {
        description = "Default path to Pass password store";
        type = types.str;
        default = homePrefix user ".password-store";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs security-related setup.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = cfg.pinentryFlavor != null;
        message = "security: Must select exactly one pinentry flavor.";
      }];

      security = {
        sudo.wheelNeedsPassword = false;
        allowUserNamespaces = true;
        allowSimultaneousMultithreading = true;
        lockKernelModules = false;
      };

      nixpkgs.config.packageOverrides = _: rec {
        passctl = mkPythonScriptWithDeps pkgs "passctl"
          (with pkgs; [ python3Packages.pyfzf nurpkgs.pystdlib python3Packages.pygit2 python3Packages.redis xdotool ])
          (builtins.readFile ./scripts/passctl.py);
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gpg-tui mkpasswd paperkey passphrase2pgp senv ssh-to-pgp ];
        programs.password-store = {
          enable = true;
          package = stable.pass.withExtensions (ext: with ext; [ pass-audit pass-checkup pass-import pass-update ]);
          settings = {
            PASSWORD_STORE_CLIP_TIME = "60";
            PASSWORD_STORE_DIR = cfg.passwordStorePath;
          };
        };
        programs.gpg = {
          enable = true;
          settings = {
            auto-key-retrieve = true;
            keyserver = "hkp://keys.openpgp.org";
            require-cross-certification = true;
            use-agent = true;
          };
        };
        programs.zsh = {
          plugins = [
            {
              name = "pass-zsh-completion";
              file = "pass-zsh-completion.plugin.zsh";
              src = inputs.pass-zsh-completion;
            }
          ];
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
            no-allow-loopback-pinentry
          '';
          inherit (cfg) pinentryFlavor;
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.pass ];
      ide.emacs.core.customKeymaps = { "custom-pass-map" = "<f6>"; };
      ide.emacs.core.customPackages = {
        "selectrum-pass" = builtins.readFile ./emacs/selectrum-pass.el;
      };
      ide.emacs.core.config = builtins.readFile ./emacs/security.el;
    })
    (mkIf cfg.polkit.silentAuth {
      security.polkit.extraConfig = ''
        /* Allow users in wheel group to manage systemd units without authentication */
        polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.systemd1.manage-units" &&
                subject.isInGroup("wheel")) {
                return polkit.Result.YES;
            }
        });

        /* Allow users in wheel group to run programs with pkexec without authentication */
        polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.policykit.exec" &&
                subject.isInGroup("wheel")) {
                return polkit.Result.YES;
            }
        });
      '';
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "p" ];
        cmd = "${pkgs.passctl}/bin/passctl";
        mode = "select";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ passctl ]; };
    })
  ];
}
