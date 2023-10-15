{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.vcs;
  user = config.attributes.mainUser.name;
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
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.diff-hl ];
      ide.emacs.core.config = builtins.readFile ./elisp/misc.el;
    })
    (mkIf (cfg.enable && cfg.batch.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mr ];
        home.file = { ".mrconfig".text = cfg.batch.configContent; };
      };
    })
  ];
}
