{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.tools.dbms;
  dbms = pkgs.writeShellScriptBin "dbms" ''
    ${config.secrets.job.enforceJobVpnHunkSh}

    enforce_job_vpn

    declare -A DBMS_TRAITS

    DBMS_TRAITS=(
    ${builtins.concatStringsSep "\n" (lib.mapAttrsToList
      (alias: meta: "  [\"${alias}\"]=\"${meta.ip} ${meta.command} ${meta.user} ${meta.passwordPassPath}\"")
      (config.secrets.job.infra.dbmsMeta))}
    )

    MYCLI_BINARY=${pkgs.mypgWorking.mycli}/bin/mycli
    PGCLI_BINARY=${pkgs.mypgWorking.pgcli}/bin/pgcli

    list_dbms_traits() {
        for i in "''${!DBMS_TRAITS[@]}"
        do
            echo "$i"
        done
    }

    main() {
        DBMS_META="''${DBMS_TRAITS[$( (list_dbms_traits) | ${pkgs.rofi}/bin/rofi -dmenu -p "Connect" )]}"
        if [ -n "$DBMS_META" ]; then
            DBMS_IP=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f1 -d\ )
            DBMS_COMMAND=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f2 -d\ )
            DBMS_USER=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f3 -d\ )
            DBMS_PASSWORD_PASS_PATH=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f4 -d\ )
            DBMS_PASSWORD=$(${pkgs.pass}/bin/pass $DBMS_PASSWORD_PASS_PATH)
            CLI_BINARY_VARNAME="''${DBMS_COMMAND^^}_BINARY"
            CLI_EXECUTABLE="''${!CLI_BINARY_VARNAME}"
            if [ "$DBMS_COMMAND" == "mycli" ]; then
                ${pkgs.tmux}/bin/tmux new-window "$CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER --password $DBMS_PASSWORD"
            elif [ "$DBMS_COMMAND" == "pgcli" ]; then
                ${pkgs.tmux}/bin/tmux new-window "PGPASSWORD=$DBMS_PASSWORD $CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER"
            fi
        fi
    }

    main

    exit 0
  '';
in {
  options = {
    tools.dbms = {
      postgresql.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable PostgreSQL helper tools.";
      };
      mysql.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MySQL helper tools.";
      };
      sqlite.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Sqlite helper tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc helper tools.";
      };
      jobDbms.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable job dbms connectivity.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.postgresql.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          pgcenter
          mypgWorking.pgcli
        ];
      };
    })
    (mkIf cfg.mysql.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          mypgWorking.mycli
        ];
      };
    })
    (mkIf cfg.sqlite.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          sqlitebrowser
          litecliWorking.litecli # TODO: shell automation: skim for selecting db file, you get the idea
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nodePackages.elasticdump
          redis-tui
        ];
      };
    })
    (mkIf (cfg.jobDbms.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-C-y" = ''spawn "${dbms}/bin/dbms" >> showWSOnProperScreen "shell"'';
      };
    })
  ];
}
