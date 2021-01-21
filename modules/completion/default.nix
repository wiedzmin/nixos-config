{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.completion;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    completion = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable completion setup";
      };
      snippets = mkOption {
        type = types.listOf types.attrs;
        description = "Various text snippets, mostly for development automation.";
        default = [ ];
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
          (readSubstituted ../subst.nix ./scripts/snippets.py);
      };
      home-manager.users.${user} = { home.packages = with pkgs; [ snippets ]; };
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/snippets ${
          lib.strings.escapeNixString (builtins.toJSON (builtins.listToAttrs (forEach cfg.snippets (s:
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
    (mkIf (cfg.enable && cfg.shell.enable) {
      assertions = [{
        assertion = config.shell.zsh.enable;
        message = "shell/completion: enable Zsh first.";
      }];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mmv-go cod ] ++ optionals (cfg.shell.recent.backend == "mcfly") [ mcfly ];
        xdg.configFile."cod/config.toml".text = ''
          [[rule]]
          executable = "/run/current-system/sw/bin/dephell"
          policy = 'ignore'
        ''; # TODO: extract option
        programs.zsh = {
          initExtra = ''
            source <(cod init $$ zsh)
          '' + optionalString (cfg.shell.recent.backend == "mcfly") ''
            source "${pkgs.mcfly}/share/mcfly/mcfly.zsh"
          '';
          sessionVariables = let dataHome = hm.xdg.dataHome;
          in {
            HISTFILE = "${dataHome}/.histfile";
            LESSHISTFILE = "${dataHome}/.lesshst";
          } // optionalAttrs (cfg.shell.recent.backend == "mcfly") {
            MCFLY_FUZZY = "true";
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs:
        [
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
      ide.emacs.core.customKeymaps = {
        "custom-yasnippet-map" = "<f5>";
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "Shift" "s" ];
        cmd = "${pkgs.snippets}/bin/snippets";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ snippets ];
      };
    })
  ];
}
