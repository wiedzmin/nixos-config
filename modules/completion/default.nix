{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.completion;
  user = config.attributes.mainUser.name;
  toml = pkgs.formats.toml { };
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    completion = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable completion setup";
      };
      expansions.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable expansions";
      };
      expansions.entries = mkOption {
        type = types.listOf types.attrs;
        description = "Various expandable text snippets, mostly for development automation.";
        default = [ ];
      };
      expansions.toggleKey = mkOption {
        type = types.enum [
          "ALT"
          "CTRL"
          "LEFT_ALT"
          "LEFT_CTRL"
          "LEFT_META"
          "LEFT_SHIFT"
          "META"
          "OFF"
          "RIGHT_ALT"
          "RIGHT_CTRL"
          "RIGHT_META"
          "RIGHT_SHIFT"
          "SHIFT"
        ];
        default = "RIGHT_SHIFT";
      };
      expansions.searchShortcut = mkOption {
        type = types.enum [
          "ALT+SPACE"
        ];
        default = "ALT+SPACE";
      };
      espanso.backend = mkOption {
        type = types.enum [ "Auto" "Clipboard" "Inject" ];
        default = "Auto";
      };
      espanso.config = mkOption {
        type = types.lines;
        default = ''
          toggle_key: ${cfg.expansions.toggleKey}
          search_shortcut: ${cfg.expansions.searchShortcut}
          auto_restart: false
          backend: ${cfg.espanso.backend}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Espanso main config";
      };
      espanso.package = mkOption {
        type = types.package;
        default = pkgs.espanso;
        visible = false;
        readOnly = true;
        internal = true;
        description = "Espanso package to use";
      };
      dev.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development-related completion setup";
      };
      shell.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various shell completion helpers";
      };
      shell.recent.backend = mkOption {
        type = types.enum [ "atuin" "mcfly" "fzf" ];
        default = "mcfly";
        description = "Which tool to use to navigate recent commands history";
      };
      shell.recent.mcfly.fuzzySearch = mkOption {
        type = types.int;
        default = 2;
        description = ''
          Fuzzy Searching

          To enable fuzzy searching, set MCFLY_FUZZY to an integer.
          0 is off; higher numbers weight toward shorter matches.
          Values in the 2-5 range get good results so far;
        '';
      };
      shell.recent.mcfly.resultsCount = mkOption {
        type = types.int;
        default = 10;
        description = "The maximum number of results shown";
      };
      shell.recent.mcfly.interfaceView = mkOption {
        type = types.enum [ "TOP" "BOTTOM" ];
        default = "TOP";
        description = "Interface view";
      };
      shell.recent.mcfly.disableMenu = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to disable the menu interface";
      };
      shell.recent.mcfly.resultsSort = mkOption {
        type = types.enum [ "RANK" "LAST_RUN" ];
        default = "RANK";
        description = "Sorting options of shown results";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs completion setup";
      };
      emacs.backend = mkOption {
        # TODO: consider extracting dedicated module
        type = types.enum [ "company" "corfu" ];
        default = "company";
        description = "Emacs completion UI to use. Currently, `company` and `corfu` are supported.";
      };
      emacs.snippets.backend = mkOption {
        type = types.enum [ "yasnippet" "tempel" ];
        default = "yasnippet";
        description = "Emacs snippets backend to use. Currently, `yasnippet` and `tempel` are supported.";
      };
      emacs.tempel.snippets = mkOption {
        type = types.lines;
        description = "Tempel templates contents";
        default = '''';
      };
      emacs.tempel.snippetsPath = mkOption {
        type = types.str;
        description = "Tempel templates contents file path";
        default = homePrefix user ".config/emacs/templates"; # TODO: search/use more specialized solution(s)
      };
      tabnine.config = mkOption {
        type = types.attrs;
        default = { };
        visible = false;
        internal = true;
        description = "TabNine configuration";
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
      assertions = [{
        assertion = config.workstation.systemtraits.enable;
        message = "navigation/completion: must enable systemtraits maintenance.";
      }];

      # NOTE: unzip is for tabnine binaries installation
      home-manager.users."${user}" = { home.packages = with pkgs; [ unzip ]; };
    })
    (mkIf (cfg.enable && cfg.dev.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ tabnine ]; # FIXME: install it to be consumable by company-tabnine
        xdg.configFile."TabNine/TabNine.toml".source = toml.generate "TabNine.toml" cfg.tabnine.config;
      };
    })
    (mkIf (cfg.enable && cfg.expansions.enable) {
      systemd.user.services.espanso-custom = {
        description = "Espanso daemon";
        path = [ pkgs.bash pkgs.curl ];
        serviceConfig = {
          ExecStart = "${cfg.espanso.package}/bin/espanso daemon";
          Restart = "on-failure";
        };
        wantedBy = [ "default.target" ];
      };
      environment.systemPackages = [ cfg.espanso.package ];
      # TODO: script(s) to store expansions in redis and show on demand (in case some useful expansions were forgotten)
      home-manager.users."${user}" = {
        home.activation = {
          populateEspansoConfig = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''mkdir -p /home/alex3rd/.config/espanso/config && echo "${cfg.espanso.config}" > /home/alex3rd/.config/espanso/config/default.yml'';
          };
          restartEspanso = {
            after = [ "linkGeneration" ];
            before = [ ];
            data = "${pkgs.systemd}/bin/systemctl --user restart espanso-custom.service";
          };
        };
        xdg.configFile."espanso/match/completion.yml".source = yaml.generate "espanso-completion.yml" {
          matches = [
            {
              trigger = ":tabns";
              replace = "TabNine::sem";
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.shell.enable) {
      assertions = [{
        assertion = config.shell.zsh.enable;
        message = "shell/completion: enable Zsh first.";
      }];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mmv-go ];
        programs.mcfly = optionalAttrs (cfg.shell.recent.backend == "mcfly") {
          enable = true;
          enableZshIntegration = true;
          fuzzySearchFactor = cfg.shell.recent.mcfly.fuzzySearch;
        };
        programs.atuin = optionalAttrs (cfg.shell.recent.backend == "atuin") {
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
      } // optionalAttrs (cfg.shell.recent.backend == "mcfly") {
        home.sessionVariables.MCFLY_RESULTS = builtins.toString cfg.shell.recent.mcfly.resultsCount;
        home.sessionVariables.MCFLY_RESULTS_SORT = cfg.shell.recent.mcfly.resultsSort;
      } // optionalAttrs (cfg.shell.recent.backend == "mcfly" && cfg.shell.recent.mcfly.interfaceView != "TOP") {
        home.sessionVariables.MCFLY_INTERFACE_VIEW = cfg.shell.recent.mcfly.interfaceView;
      } // optionalAttrs (cfg.shell.recent.backend == "mcfly" && cfg.shell.recent.mcfly.disableMenu) {
        home.sessionVariables.MCFLY_DISABLE_MENU = "TRUE";
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.all-the-icons-completion
        epkgs.pos-tip
      ] ++ optionals (cfg.emacs.backend == "company") [
        epkgs.company
        epkgs.company-box
        epkgs.company-fuzzy
        epkgs.company-quickhelp
        epkgs.company-restclient
        epkgs.company-statistics
        epkgs.company-tabnine
        epkgs.company-try-hard
      ] ++ optionals (cfg.emacs.backend == "corfu") [
        epkgs.cape
        epkgs.corfu
        epkgs.corfu-doc
        epkgs.kind-icon
      ] ++ optionals (cfg.emacs.snippets.backend == "yasnippet") [
        epkgs.yasnippet
      ] ++ optionals (cfg.emacs.snippets.backend == "yasnippet" && config.ide.emacs.navigation.collections.backend == "consult") [
        epkgs.consult-yasnippet
      ] ++ optionals (cfg.emacs.snippets.backend == "tempel") [
        epkgs.tempel
        epkgs.tempel-collection
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ]
        ([ ./elisp/completion.el ] ++ optionals (cfg.emacs.backend == "company") [ ./elisp/company.el ]
          ++ optionals (cfg.emacs.backend == "corfu") [ ./elisp/corfu.el ]
          ++ optionals (cfg.emacs.backend == "corfu" && config.ide.emacs.history.enable) [ ./elisp/corfu-history.el ]
          ++ optionals (cfg.emacs.snippets.backend == "yasnippet") [ ./elisp/yasnippet.el ]
          ++ optionals (cfg.emacs.snippets.backend == "yasnippet" && config.ide.emacs.navigation.collections.backend == "consult") [ ./elisp/consult-yasnippet.el ]
          ++ optionals (cfg.emacs.snippets.backend == "tempel") [ ./elisp/tempel.el ]);
      home-manager.users."${user}" = {
        home.file = optionalAttrs (cfg.emacs.snippets.backend == "tempel")
          {
            "${cfg.emacs.tempel.snippetsPath}".text = cfg.emacs.tempel.snippets;
          } // optionalAttrs (cfg.emacs.snippets.backend == "yasnippet") {
          ".emacs.d/resources/yasnippet" = {
            source = inputs.yasnippet-snippets;
            recursive = true;
          };
        };
      };
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = optionalAttrs (cfg.emacs.snippets.backend == "yasnippet")
        {
          yasnippet-snippets = {
            desc = "Yasnippet snippets collection";
            local.path = "${wsRoot roots "github"}/wiedzmin/yasnippet-snippets";
            remote = {
              url = "https://github.com/wiedzmin/yasnippet-snippets/";
              jump = true;
              searchSuffix = "search?q=";
            };
          };
        } // optionalAttrs (cfg.emacs.snippets.backend == "tempel") {
        tempel = {
          desc = "Tempel repo";
          local.path = "${wsRoot roots "github"}/wiedzmin/minad/tempel";
          remote = {
            url = "https://github.com/minad/tempel";
            jump = true;
            searchSuffix = "search?q=";
          };
        };
      };
    })
  ];
}
