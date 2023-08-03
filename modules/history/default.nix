{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.history;
  user = config.attributes.mainUser.name;
  toml = pkgs.formats.toml { };
in
{
  options = {
    history = {
      shell.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell history helpers";
      };
      shell.backend = mkOption {
        type = types.enum [ "atuin" "mcfly" "fzf" ];
        default = "mcfly";
        description = "Which tool to use to navigate recent commands history";
      };
      shell.mcfly.fuzzySearch = mkOption {
        type = types.int;
        default = 2;
        description = ''
          Fuzzy Searching

          To enable fuzzy searching, set MCFLY_FUZZY to an integer.
          0 is off; higher numbers weight toward shorter matches.
          Values in the 2-5 range get good results so far;
        '';
      };
      shell.mcfly.resultsCount = mkOption {
        type = types.int;
        default = 10;
        description = "The maximum number of results shown";
      };
      shell.mcfly.interfaceView = mkOption {
        type = types.enum [ "TOP" "BOTTOM" ];
        default = "TOP";
        description = "Interface view";
      };
      shell.mcfly.disableMenu = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to disable the menu interface";
      };
      shell.mcfly.resultsSort = mkOption {
        type = types.enum [ "RANK" "LAST_RUN" ];
        default = "RANK";
        description = "Sorting options of shown results";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs history maintaining extensions";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.shell.enable) {
      # FIXME: why only zsh?! *** parameterize somehow ***
      assertions = [{
        assertion = config.shell.zsh.enable;
        message = "shell/completion: enable Zsh first.";
      }];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mmv-go ];
        programs.mcfly = optionalAttrs (cfg.shell.backend == "mcfly") {
          enable = true;
          enableZshIntegration = true;
          fuzzySearchFactor = cfg.shell.mcfly.fuzzySearch;
        };
        programs.atuin = optionalAttrs (cfg.shell.backend == "atuin") {
          enable = true;
          enableZshIntegration = true;
        };
        # NOTE: see below for references
        # https://github.com/ellie/atuin/blob/main/docs/config.md
        # https://github.com/ellie/atuin/blob/main/atuin-client/config.toml
        xdg.configFile."atuin/config.toml".source = toml.generate "config.toml" {
          search_mode = "fuzzy";
          style = "compact";
        };
      } // optionalAttrs (cfg.shell.backend == "mcfly") {
        home.sessionVariables.MCFLY_RESULTS = builtins.toString cfg.shell.mcfly.resultsCount;
        home.sessionVariables.MCFLY_RESULTS_SORT = cfg.shell.mcfly.resultsSort;
      } // optionalAttrs (cfg.shell.backend == "mcfly" && cfg.shell.mcfly.interfaceView != "TOP") {
        home.sessionVariables.MCFLY_INTERFACE_VIEW = cfg.shell.mcfly.interfaceView;
      } // optionalAttrs (cfg.shell.backend == "mcfly" && cfg.shell.mcfly.disableMenu) {
        home.sessionVariables.MCFLY_DISABLE_MENU = "TRUE";
      };
    })
    (mkIf (cfg.emacs.enable) {
      assertions = [{
        assertion = config.ide.emacs.core.enable;
        message = "emacs/history: core configuration must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [
        epkgs.backup-each-save
        epkgs.recentf-ext
        epkgs.savekill
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/history.el;
    })
  ];
}
