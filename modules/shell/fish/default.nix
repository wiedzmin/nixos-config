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

            rm = "rm -r";
          };
          interactiveShellInit = "set fish_greeting";
          functions = {
            __fish_command_not_found_handler = {
              body = "__fish_default_command_not_found_handler $argv[1]";
              onEvent = "fish_command_not_found";
            };
            mkcd = {
              description = "Create [deeply] nested directory and change to it";
              body = ''
                mkdir -pv $argv;
                cd $argv;
              '';
            };
          };
          plugins = [
            {
              name = "fzf-marks";
              src = pkgs.fetchFromGitHub {
                owner = "urbainvaes";
                repo = "fzf-marks";
                rev = "f2e8844ce813f8ad35a1903eb8c680c4492e153b";
                sha256 = "0a8jlwc12m0xid2v4d7rxzci91w8qrc4x91jq4lv0lm62v2w4n1j";
              };
            }
            {
              name = "pisces"; # TODO: play and compare with https://github.com/jorgebucaran/autopair.fish
              src = pkgs.fetchFromGitHub {
                owner = "laughedelic";
                repo = "pisces";
                rev = "e45e0869855d089ba1e628b6248434b2dfa709c4";
                sha256 = "073wb83qcn0hfkywjcly64k6pf0d7z5nxxwls5sa80jdwchvd2rs";
              };
            }
            {
              name = "puffer-fish";
              src = pkgs.fetchFromGitHub {
                owner = "nickeb96";
                repo = "puffer-fish";
                rev = "12d062eae0ad24f4ec20593be845ac30cd4b5923";
                sha256 = "06g8pv68b0vyhhqzj469i9rcics67cq1kbhb8946azjb8f7rhy6s";
              };
            }
            {
              name = "fifc"; # FIXME: https://github.com/gazorby/fifc
              src = pkgs.fetchFromGitHub {
                owner = "gazorby";
                repo = "fifc";
                rev = "e953fcd521f34651d4eabedcc08cfdef7945b31d";
                sha256 = "0w05qlc0s926k0zdf10sk1qym9n3dwmnlbyzmzcv7zpr0mbrarsn";
              };
            }
            {
              name = "fzf"; # TODO: play and compare with https://github.com/PatrickF1/fzf.fish
              src = pkgs.fetchFromGitHub {
                owner = "jethrokuan";
                repo = "fzf";
                rev = "479fa67d7439b23095e01b64987ae79a91a4e283";
                sha256 = "0k6l21j192hrhy95092dm8029p52aakvzis7jiw48wnbckyidi6v";
              };
            }
            {
              name = "foreign-env";
              src = pkgs.fetchFromGitHub {
                owner = "oh-my-fish";
                repo = "plugin-foreign-env";
                rev = "dddd9213272a0ab848d474d0cbde12ad034e65bc";
                sha256 = "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs";
              };
            }
            {
              name = "colored_man_pages";
              src = pkgs.fetchFromGitHub {
                owner = "PatrickF1";
                repo = "colored_man_pages.fish";
                rev = "f885c2507128b70d6c41b043070a8f399988bc7a";
                sha256 = "0ifqdbaw09hd1ai0ykhxl8735fcsm0x2fwfzsk7my2z52ds60bwa";
              };
            }
            {
              name = "done";
              src = pkgs.fetchFromGitHub {
                owner = "franciscolourenco";
                repo = "done";
                rev = "eb32ade85c0f2c68cbfcff3036756bbf27a4f366";
                sha256 = "12l7m08bp8vfhl8dmi0bfpvx86i344zbg03v2bc7wfhm20li3hhc";
              };
            }
            {
              name = "fish-abbreviation-tips";
              src = pkgs.fetchFromGitHub {
                owner = "Gazorby";
                repo = "fish-abbreviation-tips";
                rev = "8ed76a62bb044ba4ad8e3e6832640178880df485";
                sha256 = "05b5qp7yly7mwsqykjlb79gl24bs6mbqzaj5b3xfn3v2b7apqnqp";
              };
            }
            {
              name = "upto";
              src = pkgs.fetchFromGitHub {
                owner = "Markcial";
                repo = "upto";
                rev = "2d1f35453fb55747d50da8c1cb1809840f99a646";
                sha256 = "12rbffk1z61j4bhfxdjrksbky2x4jlak08s5j44dkxdizns9gz9f";
              };
            }
            {
              name = "sponge"; # TODO: tweak, see https://github.com/meaningful-ooo/sponge for details
              src = pkgs.fetchFromGitHub {
                owner = "meaningful-ooo";
                repo = "sponge";
                rev = "384299545104d5256648cee9d8b117aaa9a6d7be";
                sha256 = "0p4vk6pq858h2v39c41irrgw1fbbcs7gd9cdr9i9fd3d6i81kmri";
              };
            }
            {
              name = "getopts";
              src = pkgs.fetchFromGitHub {
                owner = "jorgebucaran";
                repo = "getopts.fish";
                rev = "4b74206725c3e11d739675dc2bb84c77d893e901";
                sha256 = "1z5jvqip1hx59cggj9fyzaqqpz5rrsdjb3kv6ha042pbd034a57n";
              };
            }
          ];
          shellInitLast = ''
            bind "[1;5P" fzm
          '';
        };
      };
    })
  ];
}
