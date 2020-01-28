let
  deps = import ../../nix/sources.nix;
  nixpkgs-pinned-05_12_19  = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in
{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.content;
  # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
  paste_to_ix = pkgs.writeShellScriptBin "paste_to_ix" ''
    # FIXME: check if there is actually something in clipboard
    ix_url=$(${pkgs.xsel}/bin/xsel -ob | ${pkgs.ix}/bin/ix)
    echo -n "$ix_url" | ${pkgs.xsel}/bin/xsel -ib
  '';
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
        tags_cloud_task = subprocess.Popen("${nixpkgs-pinned-05_12_19.buku}/bin/buku --np --st",
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
            os.system("${nixpkgs-pinned-05_12_19.buku}/bin/buku -a {0} {1}".format(
                      bookmark_text,
                      ",".join(bookmark_tags)))
        else:
            os.system("${nixpkgs-pinned-05_12_19.buku}/bin/buku -a {0}".format(
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
        tagcloud=$(${nixpkgs-pinned-05_12_19.buku}/bin/buku --np --st | \
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
      ["urls"]="${nixpkgs-pinned-05_12_19.buku}/bin/buku -f 1 --np --st"
      ["titles"]="${nixpkgs-pinned-05_12_19.buku}/bin/buku -f 3 --np --st"
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
        ${nixpkgs-pinned-05_12_19.buku}/bin/buku -o $SELECTION
    }

    main

    exit 0
  '';
  buku_search_url = pkgs.writeScriptBin "buku_search_url" ''
    main() {
        SEARCH_RESULTS="$(${nixpkgs-pinned-05_12_19.buku}/bin/buku -f 1 --nc -p)"
        SELECTION=$( echo "$SEARCH_RESULTS" | tr ' ' '\n' | ${pkgs.dmenu}/bin/dmenu -p '> ')
        if [ -n "$SELECTION" ]; then
            ${nixpkgs-pinned-05_12_19.buku}/bin/buku -o $SELECTION
        fi
    }

    main

    exit 0
  '';
  screenshot_active_window = pkgs.writeShellScriptBin "screenshot_active_window" ''
    ${pkgs.maim}/bin/maim -o -i $(${pkgs.xdotool}/bin/xdotool getactivewindow) --format png /dev/stdout | \
        ${pkgs.coreutils}/bin/tee ${cfg.screenshots.baseDir}/screenshot-$(date ${cfg.screenshots.dateFormat}.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
        ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
  '';
  screenshot_full = pkgs.writeShellScriptBin "screenshot_full" ''
    ${pkgs.maim}/bin/maim -o --format png /dev/stdout | \
        ${pkgs.coreutils}/bin/tee ${cfg.screenshots.baseDir}/screenshot-$(date ${cfg.screenshots.dateFormat}.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
        ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
  '';
  screenshot_region = pkgs.writeShellScriptBin "screenshot_region" ''
    ${pkgs.maim}/bin/maim -o -s --format png /dev/stdout | \
        ${pkgs.coreutils}/bin/tee ${cfg.screenshots.baseDir}/screenshot-$(date ${cfg.screenshots.dateFormat}.png | ${pkgs.coreutils}/bin/tr -d '[:cntrl:]') | \
        ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
  '';
in {
  options = {
    custom.content = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-related tools.";
      };
      players.deltaSeconds = mkOption {
        type = types.int;
        default = 10;
        description = "Player rewinding delta in seconds";
      };
      screenshots.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable screenshots functionality.";
      };
      screenshots.baseDir = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Screenshots base directory";
      };
      screenshots.dateFormat = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "screenshot date suffix format";
      };
      warmup.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable pulling some highly used data into memory.";
      };
      warmup.paths = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "List of paths to warm up.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          android-file-transfer
          aria2
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
        ];
        services.syncthing.enable = true; # TODO: consider separate option(s)
        programs.mpv = {
          enable = true;
          scripts = with pkgs.mpvScripts; [
            mpris
          ];
        };
      };
    })
    (mkIf cfg.screenshots.enable {
      assertions = [
        {
          assertion = cfg.screenshots.baseDir != null;
          message = "Must provide path to screenshots dir.";
        }
        {
          assertion = cfg.screenshots.dateFormat != null;
          message = "Must provide date format.";
        }
      ];

      environment.systemPackages = [
        screenshot_active_window
        screenshot_full
        screenshot_region
      ];
    })
    (mkIf (cfg.warmup.enable && cfg.warmup.paths != []) {
      systemd.user.services."warmup" = {
        description = "Warm up paths";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.vmtouch}/bin/vmtouch -t ${lib.concatStringsSep " " cfg.warmup.paths}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
        partOf = [ "multi-user.target" ]; # FIXME: does not autostart
        wantedBy = [ "multi-user.target" ];
      };
    })
    (mkIf cfg.xmonad.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          buku_search_tag
          buku_search_url
        ];
      };
      wm.xmonad.keybindings = {
        "M-y" = ''spawn "${buku_add}/bin/buku_add"'';
        "M-i" = ''spawn "${paste_to_ix}/bin/paste_to_ix"'';
      };
    })
    (mkIf (cfg.xmonad.enable && cfg.screenshots.enable) {
      wm.xmonad.keybindings = {
        "<Print>" = ''spawn "screenshot_active_window"'';
        "C-<Print>" = ''spawn "screenshot_full"'';
        "M-<Print>" = ''spawn "screenshot_region"'';
      };
    })
  ];
}
