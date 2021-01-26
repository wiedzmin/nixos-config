{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.content.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    content.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable miscellanuous content setup.";
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
      nixpkgs.config.packageOverrides = _: rec {
        collect_links_on_page = mkPythonScriptWithDeps "collect_links_on_page"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.beautifulsoup4 xsel ])
          (readSubstituted ../../subst.nix ./scripts/collect_links_on_page.py);
        paste_to_ix = mkPythonScriptWithDeps "paste_to_ix" (with pkgs; [ ix xsel ])
          (readSubstituted ../../subst.nix ./scripts/paste_to_ix.sh);
      };

      boot.kernel.sysctl = {
        "fs.inotify.max_user_instances" = 1024;
        "fs.inotify.max_user_watches" = 1048576;
        "fs.inotify.max_queued_events" = 32768;
      };

      home-manager.users.${user} = {
        home.activation.ensureMimeappsList = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          data = "rm -f ${homePrefix ".config/mimeapps.list"} ";
        };

        xdg.dataHome = homePrefix ".local/share";
        xdg.userDirs = {
          enable = true;
          desktop = homePrefix "Desktop";
          documents = homePrefix "docs/inbox";
          download = homePrefix "Downloads";
          music = homePrefix "blobs/music";
          pictures = homePrefix "blobs/pics";
          videos = homePrefix "blobs/video";
        };

        # TODO: consider desktop files locating automation
        xdg.mimeApps.defaultApplications = (mapMimesToApp config.attributes.mimetypes.images "feh.desktop")
          // (mapMimesToApp config.attributes.mimetypes.video "mpv.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.docs "writer.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.spreadsheets "calc.desktop");

        home.packages = with pkgs; [ # TODO: rethink section
          android-file-transfer
          jmtpfs # consider providing some (shell) automation
          saldl # consider providing some (shell) automation
          you-get
          # =======
          archiver
          # =======
          exiv2
          mediainfo
          # =======
          paste_to_ix

          monolith
          tartube
        ];
        services.syncthing.enable = true; # TODO: consider separate option(s)
        xdg.mimeApps.enable = true;
        programs.aria2.enable = true;
        programs.zsh.shellAliases = { yg = "${pkgs.you-get}/bin/you-get"; };
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "p" ];
          cmd = "${pkgs.paste_to_ix}/bin/paste_to_ix";
          mode = "window";
        }
        {
          key = [ "c" ];
          cmd = "${pkgs.collect_links_on_page}/bin/collect_links_on_page";
          mode = "browser";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ collect_links_on_page paste_to_ix ];
      };
    })
  ];
}
