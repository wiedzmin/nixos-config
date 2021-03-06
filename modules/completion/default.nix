{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.completion;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
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
        snippets = mkPythonScriptWithDeps "snippets" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis xsel ])
          (builtins.readFile ./scripts/snippets.py);
      };
      # NOTE: unzip is for tabnine binaries installation
      home-manager.users.${user} = { home.packages = with pkgs; [ snippets unzip ]; };
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
      home-manager.users.${user} = {
        home.packages = with pkgs; [ tabnine ]; # FIXME: install it to be consumable by company-tabnine
        xdg.configFile."TabNine/TabNine.toml".text =
          toToml { language.python = { command = "python-language-server"; }; };
      };
    })
    (mkIf (cfg.enable && cfg.expansions.enable) {
      services.espanso.enable = true;
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

      shell.core.variables = [(optionalAttrs (cfg.shell.recent.backend == "mcfly") { MCFLY_FUZZY = "true"; })];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mmv-go ] ++ optionals (cfg.shell.recent.backend == "mcfly") [ mcfly ];
        programs.zsh = {
          initExtra = optionalString (cfg.shell.recent.backend == "mcfly") ''
            source "${pkgs.mcfly}/share/mcfly/mcfly.zsh"
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.company
        epkgs.company-box
        epkgs.company-fuzzy
        epkgs.company-quickhelp
        epkgs.company-restclient
        epkgs.company-statistics
        epkgs.company-tabnine
        epkgs.yasnippet
      ];
      ide.emacs.core.config = readSubstituted ../subst.nix ./emacs/completion.el;
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
      home-manager.users.${user} = { home.packages = with pkgs; [ snippets ]; };
    })
  ];
}
