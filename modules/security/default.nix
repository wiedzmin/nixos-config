{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.security;
  user = config.attributes.mainUser.name;
in
{
  options = {
    ext.security = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable security tools.";
      };
      pinentry.package = mkOption {
        type = types.nullOr types.package;
        example = "pkgs.pinentry-gnome3";
        description = "Pinentry package to use.";
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
        assertion = cfg.pinentry.package != null;
        message = "security: Must select exactly one pinentry package.";
      }];

      security = {
        sudo.wheelNeedsPassword = false;
        allowUserNamespaces = true;
        allowSimultaneousMultithreading = true;
        lockKernelModules = false;
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gpg-tui mkpasswd paperkey passphrase2pgp senv ssh-to-pgp ];
        programs.password-store = {
          enable = true;
          package = with pkgs; pass.withExtensions (ext: with ext; [ pass-audit pass-checkup pass-import pass-update ]);
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
        home.activation = {
          killGpgAgent = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = "${pkgs.gnupg}/bin/gpgconf --kill gpg-agent";
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
            no-allow-loopback-pinentry
          '';
          pinentry.package = cfg.pinentry.package;
        };
      };
      # TODO: generalize for multiple WMs
      wmCommon.wsMapping.rules = optionals (config.wm.i3.popupDuringFullscreen != "leave_fullscreen") [{
        class = ".*pinentry.*";
        fullscreen = true;
      }];
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.pass ];
      ide.emacs.core.customKeymaps = { "custom-pass-map" = "<f6>"; };
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/security.el ]);
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
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        security = {
          filter_class = "Emacs";
          matches = [
            {
              trigger = ":gpk";
              replace = "gpgconf --kill gpg-agent";
            }
            {
              trigger = ":gprec";
              replace = "gpg --list-only --no-default-keyring --secret-keyring /dev/null";
            }
            # TODO: play with "gpgconf --kill gpg-agent; # echo \"UPDATESTARTUPTTY\" | gpg-connect-agent > /dev/null 2>&1"
            {
              trigger = ":sslct";
              replace = "openssl s_client -showcerts -servername {{server.value}} -connect {{server.value}}:443 </dev/null | openssl x509 -inform pem -noout -text";
              vars = [
                {
                  name = "server";
                  type = "form";
                  params = { layout = "Server [[value]]"; };
                }
              ];
            }
            {
              trigger = ":sslcd";
              replace = "openssl s_client -connect {{server.value}}:443 2>/dev/null | openssl x509 -dates -noout";
              vars = [
                {
                  name = "server";
                  type = "form";
                  params = { layout = "Server [[value]]"; };
                }
              ];
            }
          ];
        };
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          openssl
        ];
      };
    })
  ];
}
