{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.content.core;
  user = config.attributes.mainUser.name;
in
{
  options = {
    content.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core content setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      boot.kernel.sysctl = {
        "fs.inotify.max_user_instances" = 1024;
        "fs.inotify.max_user_watches" = 1048576;
        "fs.inotify.max_queued_events" = 32768;
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ archiver archivemount pbzip2 pigz ];
        home.activation.ensureMimeappsList = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          data = "rm -f ${homePrefix ".config/mimeapps.list"} ";
        };

        xdg = {
          dataHome = homePrefix ".local/share";
          userDirs = {
            enable = true;
            desktop = homePrefix "Desktop";
            documents = homePrefix "docs/inbox";
            download = homePrefix "Downloads";
            music = homePrefix "blobs/music";
            pictures = homePrefix "blobs/pics";
            videos = homePrefix "blobs/video";
          };
          mimeApps.enable = true;
        };
      };
    })
  ];
}
