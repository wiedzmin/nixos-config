{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.bookmarks;
  user = config.attributes.mainUser.name;
in
{
  options = {
    shell.bookmarks = {
      enable = mkOption {
        type = types.bool;
        description = "Whether to enable shell bookmarks";
        default = false;
      };
      path = mkOption {
        type = types.str;
        description = "Where to store shell bookmarks, relative to $HOME";
        default = ".bookmarks";
      };
      order = mkOption {
        type = types.bool;
        default = false;
        description = "Keep order of bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.activation = {
          populateShellBookmarks = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''
              echo "${localBookmarksKVText config.navigation.bookmarks.entries}" > ${
                homePrefix user cfg.path
              }'';
          };
        };
      };
    })
  ];
}
