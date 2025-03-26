{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.misc;
  user = config.attributes.mainUser.name;
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
      mount.external = mkOption {
        type = types.str;
        default = homePrefix user "mnt/ext";
        description = "Standard path to mount external media (HDD, etc.) under";
      };
      mount.iso = mkOption {
        type = types.str;
        default = homePrefix user "mnt/iso";
        description = "Standard path to mount ISO images under";
      };
      iso.workdir = mkOption {
        type = types.str;
        default = homePrefix user "blobs/work/iso";
        description = "Path to working directory for ISO manipulations";
      };
      cdrom.dev = mkOption {
        type = types.str;
        default = "/dev/sr0";
        visible = false;
        internal = true;
        description = "CDROM device path";
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
        home.packages = with pkgs; [ recode rep-grep ren-find ]; # TODO: consider making some expansions/templates/whatever
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
      completion.expansions.espanso.matches = {
        content_misc = {
          matches = [
            {
              trigger = ":fdelta";
              replace = "fd -e {{fdregex.value}} | sad {{changefrom.value}} {{changeto.value}} | ${config.attributes.gitPager.cmd}";
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
              replace = "xidel {{sourcefile.value}} --extract '//a/@href' --output-format {{outputformat.value}} > links.log";
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
              trigger = ":ggr";
              replace = "cat `rg --files | grep -e \"\\.json\" | fzf` | gron | grep {{searchterm.value}} | gron --ungron";
              vars = [
                {
                  name = "searchterm";
                  type = "form";
                  params = { layout = "Search term: [[value]]"; };
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
              replace = "fd --change-newer-than {{changenewerthan.value}} -x rg -l \"\" '{}' | fzf";
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
              replace = "fd --change-older-than {{changeolderthan.value}} -x rg -l \"\" '{}' | fzf";
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
                        label = "10min";
                        id = "600000";
                      }
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
                      {
                        label = "72hours";
                        id = "259200000";
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
              trigger = ":mt";
              replace = "monolith --isolate --base-url {{baseurl.value}} --output $|$";
              vars = [
                {
                  name = "baseurl";
                  type = "form";
                  params = { layout = "URL: [[value]]"; };
                }
              ];
            }
            {
              trigger = ":gp";
              replace = "gpick";
            }
            {
              trigger = ":cdd";
              replace = "sudo dd if=${cfg.cdrom.dev} of=./{{isoname.value}} bs=2048 count=(isosize -d 2048 ${cfg.cdrom.dev}) status=progress";
              vars = [
                {
                  name = "isoname";
                  type = "form";
                  params = { layout = "ISO name: [[value]]"; };
                }
              ];
            }
            {
              trigger = ":mli";
              replace = "fd -e iso | fzf | tee /dev/tty | rargs sudo mount -o loop ./{1} ${cfg.mount.iso}";
            }
            {
              trigger = ":uli";
              replace = "sudo umount ${cfg.mount.iso}";
            }
            {
              trigger = ":mde";
              replace = "fd sd -p /dev -d 1 | fzf | tee /dev/tty | rargs sudo mount {1} ${cfg.mount.external}";
            }
            {
              trigger = ":ule";
              replace = "sudo umount ${cfg.mount.external}";
            }
          ];
        };
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          fd
          fzf
          gnugrep
          gpick
          gron
          monolith
          ripgrep
          sad
          ugrep
          xidel
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.elfeed
        epkgs.elfeed-goodies
        epkgs.elfeed-org
        epkgs.elfeed-score
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/content.el ];
      home-manager.users."${user}" = {
        home.file = {
          ".emacs.d/etc/elfeed/score/score.el".text = builtins.readFile ./assets/score.el;
        };
      };
    })
    (mkIf (cfg.enable && cfg.syncthing.enable) {
      home-manager.users."${user}" = {
        services.syncthing.enable = false;
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [
        {
          key = [ "i" ];
          cmd = "${pkgs.index-fm}/bin/index";
          mode = "run";
        }
      ];
      wmCommon.wsMapping.rules = [{
        class = "index";
        desktop = "tools"; # [ref:desktop_tools]
        activate = true;
      }];
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "iso-working-directory" = {
          desc = "ISO working directory";
          tags = [ "content" "misc" "iso" ];
          local.path = cfg.iso.workdir;
        };
        "mnt-iso" = {
          desc = "ISO well-known mount point";
          tags = [ "content" "misc" "iso" ];
          local.path = cfg.mount.iso;
        };
        "mnt-external" = {
          desc = "External HDD well-known mount point";
          tags = [ "content" "misc" "ext" ];
          local.path = cfg.mount.external;
        };
        "mnt-external-fhs" = {
          desc = "External HDD well-known mount point";
          tags = [ "content" "misc" "ext" "fhs" ];
          local.path = "/run/media/${user}";
        };
        "temp-dir" = {
          desc = "System-wide user's temporary directory";
          tags = [ "content" "misc" "temp" ];
          local.path = homePrefix user "workspace/temp";
        };
      };
    })
  ];
}
