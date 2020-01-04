{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.nas;
  mount_nas_volume = pkgs.writeShellScriptBin "mount_nas_volume" ''
    function show_list() {
        contents=("$@")
        for i in "''${contents[@]}";
        do
            echo "$i"
        done
    }

    function ensure_nas_online() {
        if [ -z "$$(${pkgs.netcat}/bin/nc -z ${cfg.hostName} 22 2 -w 2 2>&1)" ]; then
            ${pkgs.dunst}/bin/dunstify -t 7000 -u critical "Cannot access NAS, network error"
            exit 1
        fi
    }

    function ensure_volume_already_mounted() {
        if [[ ! -z $(cat /etc/mtab | ${pkgs.gnugrep}/bin/grep ${cfg.hostName} | ${pkgs.coreutils}/bin/cut -d ' '  -f 1 | ${pkgs.gnugrep}/bin/grep $VOLUME) ]]; then
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Volume '$VOLUME' already mounted"
            exit 1
        fi
    }

    function mount_volume() {
        VOLUME=$1
        mkdir -p ${cfg.localMountBase}/$VOLUME
        ${pkgs.afpfs-ng}/bin/mount_afp afp://${cfg.primaryUser}:${cfg.primaryUserPassword}@${cfg.hostName}/$VOLUME \
            ${cfg.localMountBase}/$VOLUME
        if [[ $? -eq 0 ]]; then
            ${pkgs.dunst}/bin/dunstify -t 3000 "Volume '$VOLUME' succesfully mounted"
        else
            ${pkgs.dunst}/bin/dunstify -t 5000 -u critical "Error mounting volume '$VOLUME'"
        fi
    }

    nas_volumes=(${builtins.concatStringsSep " " cfg.volumes})

    main() {
        ensure_nas_online
        ensure_volume_already_mounted

        selected_volume=$( (show_list "''${nas_volumes[@]}") | ${pkgs.dmenu}/bin/dmenu -i -p "Mount: " -l 15)
        if [ -n "$selected_volume" ]; then
            mount_volume "$selected_volume"
        fi
    }

    main

    exit 0
  '';
  unmount_nas_volume = pkgs.writeShellScriptBin "unmount_nas_volume" ''
    function show_list() {
        contents=("$@")
        for i in "''${contents[@]}";
        do
            echo "$i"
        done
    }

    function unmount_volume() {
        VOLUME=$1
        YET_MOUNTED=$(cat /etc/mtab | ${pkgs.gnugrep}/bin/grep ${cfg.hostName} | ${pkgs.coreutils}/bin/cut -d ' '  -f 1 | ${pkgs.gnugrep}/bin/grep $VOLUME)
        if [[ ! -z $YET_MOUNTED ]]; then
            fusermount -u ${cfg.localMountBase}/$VOLUME
            ${pkgs.dunst}/bin/dunstify -t 3000 "Volume $VOLUME succesfully unmounted!"
        else
            ${pkgs.dunst}/bin/dunstify -t 7000 "Volume '$VOLUME' already unmounted!"
        fi
    }

    main() {
        mounted_nas_volumes=$(cat /etc/mtab | ${pkgs.gnugrep}/bin/grep ${cfg.hostName} | ${pkgs.coreutils}/bin/cut -d ' '  -f 1)
        selected_volume=$( (show_list "''${mounted_nas_volumes[@]}") | ${pkgs.dmenu}/bin/dmenu -i -p "Unmount: " -l 15)
        if [ -n "$selected_volume" ]; then
            unmount_volume "$selected_volume"
        fi
    }

    main

    exit 0
  '';
  force_unmount_nas = pkgs.writeShellScriptBin "force_unmount_nas" ''
    function unmount_volume() {
        VOLUME=$1
        YET_MOUNTED=$(cat /etc/mtab | ${pkgs.gnugrep}/bin/grep ${cfg.hostName} | ${pkgs.coreutils}/bin/cut -d ' '  -f 1 | ${pkgs.gnugrep}/bin/grep $VOLUME)
        if [[ ! -z $YET_MOUNTED ]]; then
            fusermount -u ${cfg.localMountBase}/$VOLUME
            ${pkgs.dunst}/bin/dunstify -t 3000 "Volume $VOLUME succesfully unmounted!"
        else
            ${pkgs.dunst}/bin/dunstify -t 7000 "Volume '$VOLUME' already unmounted!"
        fi
    }

    mounted_nas_volumes=$(cat /etc/mtab | ${pkgs.gnugrep}/bin/grep ${cfg.hostName} | ${pkgs.coreutils}/bin/cut -d ' '  -f 1)
    for i in "''${mounted_nas_volumes[@]}"
    do
        unmount_volume "$i"
    done
  '';
in {
  options = {
    custom.nas = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable NAS connectivity";
      };
      primaryUser = mkOption {
        type = types.str;
        default = "";
        description = "Primary user to login at NAS";
      };
      primaryUserPassword = mkOption {
        type = types.str;
        default = "";
        description = "Primary user's password";
      };
      localMountBase = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/nas";
        description = "Where to mount remote volumes under";
      };
      hostName = mkOption {
        type = types.str;
        default = "";
        description = "NAS hostname on LAN";
      };
      volumes = mkOption {
        type = types.list;
        default = [ ];
        description = "NAS' volumes to access";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.primaryUser != "";
          message = "NAS: must provide primary user's name.";
        }
        {
          assertion = cfg.primaryUserPassword != "";
          message = "NAS: must provide primary user's password.";
        }
        {
          assertion = cfg.localMountBase != "";
          message = "NAS: must provide local base path for remote volumes mounting.";
        }
        {
          assertion = cfg.hostName != "";
          message = "NAS: must provide hostname.";
        }
      ];

      security = {
        wrappers = {
          # dmenu-pmount
          pmount.source = "${pkgs.pmount}/bin/pmount";
          pumount.source = "${pkgs.pmount}/bin/pumount";
        };
      };

      environment.systemPackages = with pkgs; [
        mount_nas_volume
        unmount_nas_volume
        force_unmount_nas
      ];
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      # TODO: try udisksctl
      wm.xmonad.keybindings = {
        "M-C-m" = ''spawn "${mount_nas_volume}/bin/mount_nas_volume"'';
        "M-C-u" = ''spawn "${unmount_nas_volume}/bin/unmount_nas_volume"'';
      };
    })
  ];
}
