{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.misc;
  user = config.attributes.mainUser.name;
  nixpkgs-index-fm = import inputs.nixpkgs-index-fm {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    content.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable miscellanuous content setup.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-related Emacs infra";
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
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          android-file-transfer
          saldl # consider providing some (shell) automation
          # =======
          findimagedupes
          # =======
          frangipanni
          sftpman
        ];

        services.syncthing.enable = true; # TODO: consider separate option(s)
        programs.aria2 = {
          enable = true;
          settings = {
            seed-ratio = 1.0;
          };
        };
        programs.zsh.shellAliases = {
          yg = "${pkgs.you-get}/bin/you-get";
        };
        xdg.configFile."espanso/user/content_misc.yml".text = ''
          name: content_misc
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":mt"
              replace: "nix shell 'nixpkgs#monolith' -c monolith --isolate --base-url {{baseurl.value}} --output $|$"
              vars:
                - name: baseurl
                  type: form
                  params:
                    layout: |
                      URL: {{value}}
                # does not work for some reason
                # - name: outfile
                #   type: shell
                #   params:
                #     cmd: echo {{baseurl.value}}
        '';
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.elfeed
        epkgs.elfeed-dashboard
        epkgs.elfeed-goodies
        epkgs.elfeed-org
        epkgs.elfeed-score
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ../../pim/orgmode/subst.nix ] [ ./emacs/content.el ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "i" ];
          cmd = "${nixpkgs-index-fm.index-fm}/bin/index";
          mode = "run";
        }
      ];
      wmCommon.wsMapping.rules = [{
        class = "index";
        desktop = "tools";
        activate = true;
      }];
    })
  ];
}
