{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.vcs;
  user = config.attributes.mainUser.name;
  # FIXME: check/unwire batchvcs functionality from bookmarks
  collectReposMetadata = bookmarks:
    (lib.mapAttrs'
      (_: meta:
        lib.nameValuePair (builtins.head (builtins.attrNames meta.batchvcs))
          (builtins.head (builtins.attrValues meta.batchvcs)))
      (lib.filterAttrs (_: meta: lib.hasAttrByPath [ "batchvcs" ] meta && meta.batchvcs != { }) bookmarks));
  formatMyreposCommands = entries: indent:
    lib.concatStringsSep "\n" (lib.mapAttrsToList
      (cmd: impl: ''
        ${cmd} =
        ${lib.concatStringsSep "\n" (lib.forEach impl (l: "${mkIndent indent}${l}"))}
      '')
      entries);
  formatMyreposRepoMeta = path: meta: ''
    [${path}]
    ${formatMyreposCommands meta 2}
  '';
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    dev.vcs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable common VCS configuration";
      };
      batch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable operations batching for various repos, currently using `myrepos`";
      };
      batch.commands = mkOption {
        type = types.attrsOf (types.listOf types.str);
        default = { };
        description = "Myrepos custom commands attrset.";
      };
      batch.configContent = mkOption {
        type = types.lines;
        default = ''
          [DEFAULT]
          ${formatMyreposCommands cfg.batch.commands 2}

          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (path: meta: formatMyreposRepoMeta path meta)
            (collectReposMetadata config.navigation.bookmarks.entries))}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Configuration data.";
      };
      ghq.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable ghq tooling.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.batch.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mr fossil ];
        home.file = { ".mrconfig".text = cfg.batch.configContent; };
      };
    })
    (mkIf (cfg.enable && cfg.ghq.enable) {
      environment.systemPackages = with pkgs; [ gitAndTools.ghq ];

      home-manager.users."${user}" = {
        programs.git.extraConfig = optionalAttrs config.navigation.bookmarks.enable {
          "ghq" = { root = config.navigation.bookmarks.workspaces.globalRoot; };
        };
        programs.zsh.shellAliases = {
          gg = "${pkgs.gitAndTools.ghq}/bin/ghq get";
          gfg = "${pkgs.gitAndTools.ghq}/bin/ghq get --vcs fossil";
        };
        programs.fish.shellAliases = {
          gg = "${pkgs.gitAndTools.ghq}/bin/ghq get";
          gfg = "${pkgs.gitAndTools.ghq}/bin/ghq get --vcs fossil";
        };

        xdg.configFile = optionalAttrs (config.shell.core.queueing.enable && config.completion.expansions.enable) {
          "espanso/match/git_navigation.yml".source = yaml.generate "espanso-git_navigation.yml" {
            matches = [
              {
                trigger = ":pgg";
                replace = "pueue add 'ghq get $|$'";
              }
            ];
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      assertions = [{
        assertion = config.ide.emacs.navigation.enable;
        message = "dev/vcs/emacs: ide/emacs/navigation must be enabled.";
      }];

      ide.emacs.core.extraPackages = epkgs: [ epkgs.diff-hl ]
        ++ optionals (config.ide.emacs.navigation.collections.backend == "consult" && cfg.ghq.enable) [ epkgs.consult-ghq ];
      ide.emacs.core.customPackages = optionalAttrs (cfg.ghq.enable) {
        "ghq-tap" = { text = builtins.readFile ./elisp/custom/ghq-tap.el; };
      };
      ide.emacs.core.customKeymaps = { "custom-vc-map" = "C-;"; };
      ide.emacs.core.config = builtins.readFile ./elisp/misc.el
        + optionalString (config.ide.emacs.navigation.collections.backend == "consult" && cfg.ghq.enable) (builtins.readFile ./elisp/consult.el)
        + optionalString (cfg.ghq.enable) ''
          (use-package ghq-tap)
        '';
    })
    (mkIf (cfg.enable && config.completion.expansions.enable && config.pim.core.enable) {
      completion.expansions.espanso.matches = {
        pim_core = {
          matches = [
            {
              trigger = ":gh";
              replace = "ghq#";
            }
            {
              trigger = ":np";
              replace = "npkg#";
            }
          ];
        };
      };
    })
  ];
}
