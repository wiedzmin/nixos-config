{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.content;
  bookshelf = pkgs.writeScriptBin "bookshelf" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i python3 -p python3 python3Packages.dmenu-python
    import os
    import subprocess

    import dmenu

    books = []

    books_task = subprocess.Popen("fd --full-path /home/alex3rd/bookshelf -e pdf -e djvu",
                                  shell=True, stdout=subprocess.PIPE)
    books.extend([book for book in books_task.stdout.read().decode().split("\n")])
    assert books_task.wait() == 0

    result = dmenu.show(books, prompt='book', lines=30)
    if result:
        os.system("zathura {0}".format(result))
  '';
  buku_add = pkgs.writeScriptBin "buku_add" ''
    is_url () {
        url_regex='(https?|ftp|file)://[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]'
        url_candidate=$1
        if [[ $url_candidate =~ $url_regex ]]
        then
            return 0
        fi
        return 1
    }

    collect_tags () {
        taglist=()
        sep=''${1:-,}
        tagcloud=$(${pkgs.buku}/bin/buku --np --st | \
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

    sleep_sec=''${1:-1}

    add_mark () {
        inserturl=$(echo -e "$(${pkgs.xsel}/bin/xsel -o -b)" | ${pkgs.dmenu}/bin/dmenu -p '> ' -mesg "Use URL below or type manually")
        if [[ $? -ne 0 ]]; then
            exit
        fi
        is_url $inserturl
        if [[ $? -ne 0 ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "URL is not valid, exiting"
            exit
        fi

        add_tags
    }

    add_tags () {
        collect_tags ","
        if [[ $(echo "''${taglist}" | wc -l) -gt 0 ]]; then
            ${pkgs.buku}/bin/buku -a ''${inserturl} ''${taglist[@]}
        else
            ${pkgs.buku}/bin/buku -a ''${inserturl}
        fi
    }

    main() {
        sleep $sleep_sec
        add_mark
        ${pkgs.dunst}/bin/dunstify -t 5000 "Bookmark added: $inserturl"
    }

    main

    exit 0
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
          waon
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
        "M-S-b" = ''spawn "${bookshelf}/bin/bookshelf"'';
        "M-y" = ''spawn "${buku_add}/bin/buku_add"'';
      };
    })
  ];
}
