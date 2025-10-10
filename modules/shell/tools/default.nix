{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# <[cli json]> - <consult-ripgrep "/home/alex3rd/workspace/repos/github.com/NixOS/nixpkgs/" "CLI JSON description">
# <[ssh shell]> - <consult-ripgrep "/home/alex3rd/workspace/repos/github.com/NixOS/nixpkgs/" "ssh shell description">
# npkg#x4

# nsp>broot|each|gron|htmlq|jc|jj|jp|jless|jqp|lfs|miller|moreutils|pipe-rename|sd|up|pup
# nsp>broot npkg#broot
# nsp>each npkg#each
# nsp>gron npkg#gron
# nsp>htmlq npkg#htmlq
# nsp>jc npkg#jc
# nsp>jj npkg#jj
# nsp>jp npkg#jp
# nsp>jless npkg#jless
# nsp>jqp npkg#jqp
# nsp>lfs npkg#lfs
# nsp>miller npkg#miller
# nsp>moreutils npkg#moreutils # NOTE: see https://joeyh.name/code/moreutils/ for reference
# nsp>pipe-rename npkg#pipe-rename
# nsp>sd npkg#sd # TODO: play with it in streaming use-cases
# nsp>up npkg#up
# nsp>pup npkg#pup

let
  cfg = config.shell.tools;
  user = config.attributes.mainUser.name;
in
{
  options = {
    shell.tools = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable successors of some traditional tools like find, sed, etc.";
      };
      pager = mkOption {
        type = types.enum [ "less" "moar" ];
        default = "less";
        description = "Pager tool to use";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        programs = {
          ripgrep-all = {
            enable = true;
          };
          lsd = {
            enable = true;
            settings = {
              classic = false;
              blocks = [
                "permission"
                "user"
                "group"
                "size"
                "date"
                "name"
              ];
              color = {
                when = "auto";
              };
              date = "+%a %Y %b %d %X";
              dereference = true;
              hyperlink = "always";
              icons = {
                when = "auto";
                theme = "fancy";
                separator = " ";
              };
              layout = "grid";
              size = "default";
              sorting = {
                column = "name";
                reverse = false;
                dir-grouping = "first";
              };
              no-symlink = false;
              symlink-arrow = "⇒";
            };
          };
          bat = {
            enable = true;
            config = {
              theme = "ansi";
              pager = "less -FR";
            };
          };
          jq = {
            enable = true;
            colors = {
              null = "1;30";
              false = "0;91";
              true = "0;92";
              numbers = "0;36";
              strings = "1;96";
              arrays = "1;94";
              objects = "1;33";
              objectKeys = "1;34";
            };
          };
        };
      };

      attributes.pager.cmd = optionalString (cfg.pager == "less") "${pkgs.less}/bin/less" +
        optionalString (cfg.pager == "moar") "${pkgs.moar}/bin/moar";
      shell.core.variables = [{ PAGER = config.attributes.pager.cmd; global = true; }];
    })
  ];
}
