{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        # TODO: play with automation around these tools
        home.packages = with pkgs; [
          broot
          each
          gron
          htmlq
          jc
          miller
          ripgrep-all
          sad # TODO: consider creating shell aliases
          sd # TODO: play with it in streaming use-cases
          up
          uq
        ];
        programs = {
          lsd = {
            enable = true;
            enableAliases = true;
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
            };
          };
        };
      };
      shell.core.variables = [{ GIT_PAGER = "${pkgs.delta}/bin/delta"; }];
    })
  ];
}
