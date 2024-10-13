{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.fish;
  user = config.attributes.mainUser.name;
in
{
  options = {
    shell.fish = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Fish shell";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.extraUsers."${user}".shell = pkgs.fish;
      programs.fish.enable = true;

      home-manager.users."${user}" = {
        programs.fish = {
          enable = true;
          shellAliases = {
            cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency
            catb = "${pkgs.bat}/bin/bat -A";

            j = "br -s";

            df = "${pkgs.duf}/bin/duf";
            du = "${pkgs.du-dust}/bin/dust";
          };
          interactiveShellInit = "set fish_greeting";
          functions = {
            __fish_command_not_found_handler = {
              body = "__fish_default_command_not_found_handler $argv[1]";
              onEvent = "fish_command_not_found";
            };
          };
          plugins = [{
            name = "fzf-marks";
            src = pkgs.fetchFromGitHub {
              owner = "urbainvaes";
              repo = "fzf-marks";
              rev = "f2e8844ce813f8ad35a1903eb8c680c4492e153b";
              sha256 = "0a8jlwc12m0xid2v4d7rxzci91w8qrc4x91jq4lv0lm62v2w4n1j";
            };
          }];
          shellInitLast = ''
            bind "[1;5P" fzm
          '';
        };
      };
    })
  ];
}
