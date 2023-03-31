{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.misc;
  user = config.attributes.mainUser.name;
  yaml = pkgs.formats.yaml { };
  timeDurations = [
    {
      label = "1 day";
      id = "1day";
    }
    {
      label = "2 days";
      id = "2days";
    }
    {
      label = "3 days";
      id = "3days";
    }
    {
      label = "4 days";
      id = "4days";
    }
    {
      label = "5 days";
      id = "5days";
    }
    {
      label = "6 days";
      id = "6days";
    }
    {
      label = "1 week";
      id = "1sweek";
    }
    {
      label = "2 weeks";
      id = "2weeks";
    }
    {
      label = "3 weeks";
      id = "3weeks";
    }
    {
      label = "4 weeks";
      id = "4weeks";
    }
    {
      label = "1 month";
      id = "1month";
    }
    {
      label = "2 months";
      id = "2months";
    }
    {
      label = "3 months";
      id = "3months";
    }
  ];
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
      syncthing.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Syncthing service";
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
        programs.aria2 = {
          enable = true;
          settings = {
            seed-ratio = 1.0;
          };
        };
        programs.zsh.shellAliases = {
          yg = "${pkgs.you-get}/bin/you-get";
        };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/content_misc.yml".source = yaml.generate "espanso-content_misc.yml"
          {
            matches = [
              {
                trigger = ":fdelta";
                replace = "nix shell \"nixpkgs#fd\" \"nixpkgs#sad\" \"nixpkgs#delta\" -c fd -e {{fdregex.value}} | sad {{changefrom.value}} {{changeto.value}} | delta";
                vars = [
                  {
                    name = "fdregex";
                    type = "form";
                    params = { layout = "Search regexp: [[value]]"; };
                  }
                  {
                    name = "changefrom";
                    type = "form";
                    params = { layout = "change from: [[value]]"; };
                  }
                  {
                    name = "changeto";
                    type = "form";
                    params = { layout = "change to: [[value]]"; };
                  }
                ];
              }
              {
                trigger = ":colinks";
                replace = "nix shell \"nixpkgs#xidel\" -c xidel {{sourcefile.value}} --extract '//a/@href' --output-format {{outputformat.value}} > links.log";
                vars = [
                  {
                    name = "sourcefile";
                    type = "form";
                    params = { layout = "Search root: [[value]]"; };
                  }
                  {
                    name = "outputformat";
                    type = "choice";
                    params = { values = [ "adhoc" "bash" "json" ]; };
                  }
                ];
              }
              {
                trigger = ":fsame";
                replace = "find -L {{searchroot.value}} -samefile {{comparewith.value}}";
                vars = [
                  {
                    name = "searchroot";
                    type = "form";
                    params = { layout = "Search root: [[value]]"; };
                  }
                  {
                    name = "comparewith";
                    type = "form";
                    params = { layout = "Compare with: [[value]]"; };
                  }
                ];
              }
              {
                trigger = ":fdnew";
                replace = "nix shell \"nixpkgs#fd\" \"nixpkgs#ripgrep\" \"nixpkgs#fzf\" -c fd --change-newer-than {{changenewerthan.value}} -x rg -l \"\" '{}' | fzf";
                vars = [
                  {
                    name = "changenewerthan";
                    type = "choice";
                    params = { values = timeDurations; };
                  }
                ];
              }
              {
                trigger = ":fdold";
                replace = "nix shell \"nixpkgs#fd\" \"nixpkgs#ripgrep\" \"nixpkgs#fzf\" -c fd --change-older-than {{changeolderthan.value}} -x rg -l \"\" '{}' | fzf";
                vars = [
                  {
                    name = "changeolderthan";
                    type = "choice";
                    params = { values = timeDurations; };
                  }
                ];
              }
              {
                trigger = ":uis";
                replace = "{{result.value}}";
                vars = [
                  {
                    name = "result";
                    type = "choice";
                    params = {
                      values = [
                        {
                          label = "week";
                          id = "604800";
                        }
                        {
                          label = "day";
                          id = "86400";
                        }
                        {
                          label = "hour";
                          id = "3600";
                        }
                      ];
                    };
                  }
                ];
              }
              {
                trigger = ":uims";
                replace = "{{result.value}}";
                vars = [
                  {
                    name = "result";
                    type = "choice";
                    params = {
                      values = [
                        {
                          label = "week";
                          id = "604800000";
                        }
                        {
                          label = "day";
                          id = "86400000";
                        }
                        {
                          label = "hour";
                          id = "3600000";
                        }
                      ];
                    };
                  }
                ];
              }
              {
                trigger = ":isofd";
                replace = "sudo dd bs={{blocksize.value}} if={{isopath.value}} of={{devicepath.value}} conv=fdatasync";
                vars = [
                  {
                    name = "blocksize";
                    type = "choice";
                    params = {
                      values = [
                        "1M"
                        "2M"
                        "4M"
                      ];
                    };
                  }
                  {
                    name = "isopath";
                    type = "form";
                    params = { layout = "ISO path: [[value]]"; };
                  }
                  {
                    name = "devicepath";
                    type = "form";
                    params = { layout = "Device path: [[value]]"; };
                  }
                ];
              }
              {
                trigger = ":idu";
                replace = "nix shell 'nixpkgs#findimagedupes' -c findimagedupes .";
              }
              {
                trigger = ":mt";
                replace = "nix shell 'nixpkgs#monolith' -c monolith --isolate --base-url {{baseurl.value}} --output $|$";
                vars = [
                  {
                    name = "baseurl";
                    type = "form";
                    params = { layout = "URL: [[value]]"; };
                  }
                ];
              }
            ];
          } // optionalAttrs (config.shell.tmux.enable) {
          filter_title = "\".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*\"";
        };
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
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ../../pim/orgmode/subst.nix ] [ ./elisp/content.el ];
    })
    (mkIf (cfg.enable && cfg.syncthing.enable) {
      home-manager.users."${user}" = {
        services.syncthing.enable = false;
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [
        {
          key = [ "i" ];
          cmd = "${pkgs.index-fm}/bin/index";
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
