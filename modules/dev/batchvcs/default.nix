{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.batchvcs;
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
    dev.batchvcs = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable operations batching for various repos, currently using `myrepos`.";
      };
      commands = mkOption {
        type = types.attrsOf (types.listOf types.str);
        default = { };
        description = "Myrepos custom commands attrset.";
      };
      configContent = mkOption {
        type = types.lines;
        default = ''
          [DEFAULT]
          ${formatMyreposCommands cfg.commands 2}

          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (path: meta: formatMyreposRepoMeta path meta)
            (collectReposMetadata config.navigation.bookmarks.entries))}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Configuration data.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mr ];
        home.file = { ".mrconfig".text = cfg.configContent; };
      };
    })
  ];
}
