{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.batch;
  user = config.attributes.mainUser.name;
  formatMyreposCommands = entries: indent:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (cmd: impl: ''
      ${cmd} =
      ${lib.concatStringsSep "\n" (lib.forEach impl (l: "${mkIndent indent}${l}"))}
    '') entries);
  formatMyreposRepoMeta = path: meta: indent: ''
    [${path}]
    ${formatMyreposCommands meta 2}
  '';
in {
  options = {
    dev.git.batch = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable operations batching for Git repos, currently using `myrepos`.";
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

          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (path: meta: formatMyreposRepoMeta path meta 2)
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
      home-manager.users.${user} = {
        home.packages = with pkgs; [ mr ];
        home.file = { ".mrconfig".text = cfg.configContent; };
      };
    })
  ];
}
