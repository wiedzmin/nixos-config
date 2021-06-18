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
          dasel
          each
          gron
          ripgrep-all
          up
          uq
        ];
        programs = {
          lsd = {
            enable = true;
            enableAliases = true;
          };
          bat = {
            enable = true;
            config = {
              theme = "ansi";
              pager = "less -FR";
            };
          };
        };
      };
    })
  ];
}
