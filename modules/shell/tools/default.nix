{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.tools;
  user = config.attributes.mainUser.name;
in {
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
        home.packages = with pkgs; [ broot choose each fd gron ijq ripgrep-all sd ugrep uq vgrep xlsxgrep ];
        programs = {
          lsd = {
            enable = true;
            enableAliases = true;
          };
          bat = {
            enable = true;
            config = {
              theme = "ansi-dark";
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
    })
  ];
}
