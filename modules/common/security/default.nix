let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-05_12_19  = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in
{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.security;
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
      assertions = [{
        assertion = cfg.pinentryFlavor != null;
        message = "security: Must select exactly one pinentry flavor.";
      }];
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          (pass.withExtensions (ext: with ext; [ pass-audit pass-checkup pass-import pass-update ]))
          gopass
          paperkey
          rofi-pass
          srm
          nixpkgs-pinned-05_12_19.vulnix
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
          pinentryFlavor = cfg.pinentryFlavor;
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
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./security.el; }));
    })
    (mkIf (cfg.polkit.silentAuth) {
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
# also https://github.com/Kwpolska/upass/blob/master/upass/__main__.py
