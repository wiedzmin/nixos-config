{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.completion;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  toml = pkgs.formats.toml { };
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
      snippets.entries = mkOption {
        type = types.listOf types.attrs;
        description = "Various text snippets, mostly for development automation.";
        default = [ ];
      };
      expansions.entries = mkOption {
        type = types.listOf types.attrs;
        description = "Various text snippets, mostly for development automation.";
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
      espansoConfig = mkOption {
        type = types.lines;
        default = ''
          toggle_key: ${cfg.expansions.toggleKey}
          auto_restart: false
          backend: Clipboard
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Espanso main config";
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
        type = types.enum [ "mcfly" "fzf" ];
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
        message = "navigation/completion: must enable systemtraits maintainence.";
      }];

      nixpkgs.config.packageOverrides = _: rec {
        snippets = mkPythonScriptWithDeps pkgs "snippets" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis xsel ])
          (builtins.readFile ./scripts/snippets.py);
      };
      # NOTE: unzip is for tabnine binaries installation
      home-manager.users."${user}" = { home.packages = with pkgs; [ snippets unzip ]; };
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/snippets ${
          lib.strings.escapeNixString (builtins.toJSON (builtins.listToAttrs (forEach cfg.snippets.entries (s:
            nameValuePair
            "${lib.concatStringsSep ":" (maybeAttrList "tags" s "-")} | ${(maybeAttrString "description" s "-")} | ${
              (maybeAttrString "language" s "-")
            } | ${s.code}" s.code))))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.dev.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ tabnine ]; # FIXME: install it to be consumable by company-tabnine
        xdg.configFile."TabNine/TabNine.toml".source =
          toml.generate "TabNine.toml" { language.python = { command = "python-language-server"; }; };
      };
    })
    (mkIf (cfg.enable && cfg.expansions.enable) {
      services.espanso.enable = true;
      # FIXME: package auxillary `modulo` binary in `nur-packages`
      systemd.user.services.espanso.path = [ pkgs.bash nurpkgs.modulo pkgs.curl ];
      # TODO: script(s) to store expansions in redis and show on demand (in case some useful expansions were forgotten)
      home-manager.users."${user}" = {
        home.activation = {
          populateEspansoConfig = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''echo "${cfg.espansoConfig}" > /home/alex3rd/.config/espanso/default.yml'';
          };
          restartEspanso = {
            after = [ "linkGeneration" ];
            before = [ ];
            data = "${pkgs.systemd}/bin/systemctl --user restart espanso.service";
          };
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
        };
      } // optionalAttrs (cfg.shell.recent.backend == "mcfly") {
        home.sessionVariables.MCFLY_FUZZY = builtins.toString cfg.shell.recent.mcfly.fuzzySearch;
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
        epkgs.company
        epkgs.company-box
        epkgs.company-fuzzy
        epkgs.company-prescient
        epkgs.company-quickhelp
        epkgs.company-restclient
        epkgs.company-statistics
        epkgs.company-tabnine
        epkgs.company-try-hard
        epkgs.pos-tip
        epkgs.yasnippet
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./emacs/completion.el ];
      ide.emacs.core.customKeymaps = { "custom-yasnippet-map" = "<f5>"; };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "Shift" "s" ];
        cmd = "${pkgs.snippets}/bin/snippets";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ snippets ]; };
    })
  ];
}
