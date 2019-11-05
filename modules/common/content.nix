{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.content;
  buku_add = writePythonScriptWithPythonPackages "buku_add" [
    pkgs.python3Packages.dmenu-python
    pkgs.python3Packages.notify2
  ] ''
    import os
    import re
    import subprocess
    import sys
    import time

    import dmenu
    import notify2
    from notify2 import URGENCY_NORMAL, URGENCY_CRITICAL

    URL_REGEX = "(https?|ftp|file)://[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]"


    def is_valid_url(url):
        return re.search(URL_REGEX, url) is not None


    def fetch_tags_cloud():
        tags_cloud_task = subprocess.Popen("${pkgs.bukuWorking.buku}/bin/buku --np --st",
                                      shell=True, stdout=subprocess.PIPE)
        result = [" ".join(tag.strip().split(" ")[1:-1])
                  for tag in tags_cloud_task.stdout.read().decode().split("\n") if tag]
        assert tags_cloud_task.wait() == 0
        return result


    notify2.init("buku_add")

    bookmark_text_task = subprocess.Popen("${pkgs.xsel}/bin/xsel -o -b",
                                          shell=True, stdout=subprocess.PIPE)
    bookmark_text = bookmark_text_task.stdout.read().decode().strip()
    assert bookmark_text_task.wait() == 0
    if bookmark_text is not None:
        if not is_valid_url(bookmark_text):
            n = notify2.Notification("error", "URL is not valid")
            n.set_urgency(URGENCY_CRITICAL)
            n.set_timeout(5000)
            n.show()
            sys.exit(1)
        result = dmenu.show([bookmark_text], prompt='bookmark')
        if not result:
            n = notify2.Notification("OK", "Aborted adding bookmark")
            n.set_urgency(URGENCY_NORMAL)
            n.set_timeout(5000)
            n.show()
            sys.exit(1)
        tags_cloud = fetch_tags_cloud()
        bookmark_tags = []
        while True:
            tag = dmenu.show(tags_cloud, prompt='add tag', lines=15)
            if tag:
               bookmark_tags.append(tag)
               tags_cloud.remove(tag)
            else:
               break
        if bookmark_tags:
            os.system("${pkgs.bukuWorking.buku}/bin/buku -a {0} {1}".format(
                      bookmark_text,
                      ",".join(bookmark_tags)))
        else:
            os.system("${pkgs.bukuWorking.buku}/bin/buku -a {0}".format(
                      bookmark_text))
        n = notify2.Notification("Success", "Bookmark added: {0} ({1})".format(
                                 bookmark_text,
                                 ",".join(bookmark_tags)))
        n.set_urgency(URGENCY_NORMAL)
        n.set_timeout(2000)
        n.show()
    else:
        n = notify2.Notification("Error", "No text in clipboard")
        n.set_urgency(URGENCY_CRITICAL)
        n.set_timeout(2000)
        n.show()
        sys.exit(1)
  '';
  buku_search_tag = let
    buku_batch_open_treshold = 20;
  in pkgs.writeScriptBin "buku_search_tag" ''
    collect_tags () {
        taglist=()
        sep=''${1:-,}
        tagcloud=$(${pkgs.bukuWorking.buku}/bin/buku --np --st | \
                   ${pkgs.gawk}/bin/awk '{$NF=""; print $0}' | \
                   ${pkgs.coreutils}/bin/cut -d ' ' -f2 | sort -u )
        while true; do
            tag=$(echo $tagcloud | tr ' ' '\n' | ${pkgs.dmenu}/bin/dmenu -p '> ' -mesg "Add tag" -custom)
            keep_going=$?
            if [[ $keep_going -ne 0 ]]; then
                break
            fi
            tag=$(echo "$tag" | tr -d '[:space:]')
            taglist+=("$tag$sep")
            tagcloud=( "''${tagcloud[@]/$tag}" )
        done
    }

    declare -A MODES

    MODES=(
      ["urls"]="${pkgs.bukuWorking.buku}/bin/buku -f 1 --np --st"
      ["titles"]="${pkgs.bukuWorking.buku}/bin/buku -f 3 --np --st"
    )
    DEFAULT_MODE=urls

    ask_for_mode() {
        for i in "''${!MODES[@]}"
        do
            echo "$i"
        done
    }

    list_search_results() {
        for i in "''${SEARCH_RESULTS[@]}"
        do
            echo "$i"
        done
    }

    OPEN_ALL="Alt+0"
    HELP_COLOR="#774477"

    main() {
        collect_tags ","
        MODE=$( (ask_for_mode) | ${pkgs.dmenu}/bin/dmenu -i -p "Mode" )
        if [ -z $MODE ]; then
            MODE=$DEFAULT_MODE
        fi
        BUKU_CMD=''${MODES[$MODE]}
        if [[ $(echo "''${taglist}" | wc -l) -eq 0 ]]; then
            exit 1
        fi
        BUKU_CMD="$BUKU_CMD ''${taglist[@]}"
        BUKU_CMD=''${BUKU_CMD%?}
        SEARCH_RESULTS="$($BUKU_CMD)"
        LEGEND="Select an entry or use <span color='$HELP_COLOR'>$OPEN_ALL</span> to open all bookmarks. You could open maximum ${
          builtins.toString buku_batch_open_treshold
        } bookmarks at once."
        SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | \
                     ${pkgs.dmenu}/bin/dmenu -p '> ' -mesg "''${LEGEND}" -kb-custom-10 "''${OPEN_ALL}")
        ROFI_EXIT=$?
        if [[ $ROFI_EXIT -eq 10 ]]; then
            if [[ $(echo "''${taglist}" | wc -l) -gt ${builtins.toString buku_batch_open_treshold} ]]; then
                exit 1
            else
                SELECTION=$SEARCH_RESULTS
            fi
        fi

        SELECTION=$( (list_search_results) | ${pkgs.gawk}/bin/awk '{print $1}' )
        ${pkgs.bukuWorking.buku}/bin/buku -o $SELECTION
    }

    main

    exit 0
  '';
  buku_search_url = pkgs.writeScriptBin "buku_search_url" ''
    main() {
        SEARCH_RESULTS="$(${pkgs.bukuWorking.buku}/bin/buku -f 1 --nc -p)"
        SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | ${pkgs.dmenu}/bin/dmenu -p '> ')
        if [ -n "$SELECTION" ]; then
            ${pkgs.bukuWorking.buku}/bin/buku -o $SELECTION
        fi
    }

    main

    exit 0
  '';
in {
  options = {
    custom.content = {
      consumers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-consumering tools.";
      };
      compression.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable (de)compression tools.";
      };
      imageTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools for working with images.";
      };
      videoTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools for working with videos.";
      };
      orderingTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various ordering/deduplication tools.";
      };
      players.deltaSeconds = mkOption {
        type = types.int;
        default = 10;
        description = "Player rewinding delta in seconds";
      };
      mpd.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MPD";
      };
      ympd.enable = mkOption { # TODO: check how ympd relates to user-level mpd service
        type = types.bool;
        default = false;
        description = "Whether to enable YMPD";
      };
      ympd.port = mkOption {
        type = types.str;
        default = "9090";
        description = "Port for YMPD to listen on";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.consumers.enable {
      # transmission + service https://transmissionbt.com/ + stig
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          android-file-transfer
          aria2
          gallery-dl
          jmtpfs # consider providing some (shell) automation
          qbittorrent
          uget
          wayback_machine_downloader
          you-get
          ytcc
        ] ++ lib.optionals config.attributes.staging.enable [
        ];
        services.syncthing.enable = true; # TODO: consider separate option(s)
      };
    })
    (mkIf cfg.compression.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          archiver
          pbzip2
          pigz
          unar
        ];
      };
    })
    (mkIf cfg.imageTools.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          exif
          exiftool
          exiv2
          gimp
          jhead
          mediainfo
          mediainfo-gui
          rawtherapee
        ];
      };
    })
    (mkIf cfg.videoTools.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          ( # FIXME: make closure for last working version
            mpv-with-scripts.override (
              {
                scripts = [ mpvScripts.mpris ];
              }
            )
          )
          ccextractor
          ffmpeg
          clipgrab
        ];
      };
    })
    (mkIf cfg.videoTools.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          dupd
          jdupes
          rmlint
          fpart
        ];
      };
    })
    (mkIf cfg.mpd.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        services.mpd = { # TODO: check options' values
          enable = true;
          musicDirectory = "/home/${config.attributes.mainUser.name}/blobs/music";
        };
      };
    })
    (mkIf cfg.ympd.enable {
      assertions = [
        {
          assertion = cfg.ympd.enable && cfg.mpd.enable;
          message = "content: enabling YMPD makes no sense without enabling MPD beforehand.";
        }
      ];

      services.ympd = {
        enable = true;
        webPort = cfg.ympd.port;
      };
    })
    (mkIf cfg.xmonad.enable {
      wm.xmonad.keybindings = {
        "M-y" = ''spawn "${buku_add}/bin/buku_add"'';
      };
    })
  ];
}
