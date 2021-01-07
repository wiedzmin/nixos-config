{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.completion;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  prefix = config.wmCommon.prefix;
in {
  options = {
    shell.completion = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various shell completion helpers";
      };
      recent.backend = mkOption {
        type = types.enum [ "mcfly" "fzf" ];
        default = "mcfly";
        description = "Which tool to use to navigate recent commands history";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.shell.zsh.enable;
        message = "shell/completion: enable Zsh first.";
      }];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mmv-go cod ] ++ optionals (cfg.recent.backend == "mcfly") [ mcfly ];
        xdg.configFile."cod/config.toml".text = ''
          [[rule]]
          executable = "/run/current-system/sw/bin/dephell"
          policy = 'ignore'
        ''; # TODO: extract option
        programs.zsh = {
          initExtra = ''
            source <(cod init $$ zsh)
          '' + optionalString (cfg.recent.backend == "mcfly") ''
            source "${pkgs.mcfly}/share/mcfly/mcfly.zsh"
          '';
          sessionVariables = let dataHome = hm.xdg.dataHome;
          in {
            HISTFILE = "${dataHome}/.histfile";
            LESSHISTFILE = "${dataHome}/.lesshst";
            YSU_IGNORED_ALIASES = [ "g" "ll" ];
            YSU_MODE = "ALL";
          } // optionalAttrs (cfg.recent.backend == "mcfly") {
            MCFLY_FUZZY = "true";
            MCFLY_LIGHT = "TRUE";
          };
        };
      };
    })
  ];
}
