let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-02_08_19 = import deps.nixpkgs-pinned-02_08_19 { config.allowUnfree = true; };
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.tools.dbms;
in {
  options = {
    tools.dbms = {
      postgresql.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable PostgreSQL helper tools.";
      };
      postgresql.logPath = mkOption {
        type = types.str;
        default = homePrefix ".config/pgcli/log";
        description = "PgCLI log file location.";
      };
      postgresql.historyPath = mkOption {
        type = types.str;
        default = homePrefix ".config/pgcli/history";
        description = "PgCLI historylog file location.";
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
      cli.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable job dbms connectivity.";
      };
      cli.meta = mkOption {
        type = types.attrsOf types.attrs;
        default = { };
        description = "Job dbms metadata.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.postgresql.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ pgcenter nixpkgs-pinned-05_12_19.pgcli ];
        xdg.configFile."pgcli/config".text = lib.generators.toINI { } {
          main = {
            smart_completion = "True";
            wider_completion_menu = "False";
            multi_line = "False";
            multi_line_mode = "psql";
            destructive_warning = "True";
            expand = "True";
            auto_expand = "False";
            generate_aliases = "False";
            log_file = cfg.postgresql.logPath;
            keyword_casing = "auto";
            casing_file = "default";
            generate_casing_file = "False";
            case_column_headers = "True";
            history_file = cfg.postgresql.historyPath;
            log_level = "INFO";
            asterisk_column_order = "table_order";
            qualify_columns = "if_more_than_one_table";
            search_path_filter = "False";
            timing = "True";
            table_format = "psql";
            syntax_style = "emacs";
            vi = "False";
            on_error = "STOP";
            row_limit = "1000";
            less_chatty = "False";
            min_num_menu_lines = "10";
            multiline_continuation_char = "''";
            null_string = "'<null>'";
            enable_pager = "True";
            keyring = "False";
          };
          colors = {
            "completion-menu.completion.current" = "'bg:#ffffff #000000'";
            "completion-menu.completion" = "'bg:#008888 #ffffff'";
            "completion-menu.meta.completion.current" = "'bg:#44aaaa #000000'";
            "completion-menu.meta.completion" = "'bg:#448888 #ffffff'";
            "completion-menu.multi-column-meta" = "'bg:#aaffff #000000'";
            "scrollbar.arrow" = "'bg:#003333'";
            "scrollbar" = "'bg:#00aaaa'";
            "selected" = "'#ffffff bg:#6666aa'";
            "search" = "'#ffffff bg:#4444aa'";
            "search.current" = "'#ffffff bg:#44aa44'";
            "bottom-toolbar" = "'bg:#222222 #aaaaaa'";
            "bottom-toolbar.off" = "'bg:#222222 #888888'";
            "bottom-toolbar.on" = "'bg:#222222 #ffffff'";
            "search-toolbar" = "'noinherit bold'";
            "search-toolbar.text" = "'nobold'";
            "system-toolbar" = "'noinherit bold'";
            "arg-toolbar" = "'noinherit bold'";
            "arg-toolbar.text" = "'nobold'";
            "bottom-toolbar.transaction.valid" = "'bg:#222222 #00ff5f bold'";
            "bottom-toolbar.transaction.failed" = "'bg:#222222 #ff005f bold'";
            "output.header" = "'#00ff5f bold'";
            "output.odd-row" = "";
            "output.even-row" = "";
          };
          "named queries" = { };
          "alias_dsn" = { };
          "data_formats" = {
            decimal = "";
            float = "";
          };
        };
      };
    })
    (mkIf cfg.mysql.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nixpkgs-pinned-05_12_19.mycli ];
      };
    })
    (mkIf cfg.sqlite.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          sqlitebrowser
          nixpkgs-pinned-02_08_19.litecli # TODO: shell automation: skim for selecting db file, you get the idea
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nixpkgs-pinned-05_12_19.nodePackages.elasticdump ];
      };
    })
    (mkIf (cfg.cli.enable && cfg.wm.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        dbms = writePythonScriptWithPythonPackages "dbms" [
          pkgs.pass
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.notify2
          pkgs.python3Packages.redis
          pkgs.tmux
          pkgs.vpnctl
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./dbms.py; })));
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set misc/dbms_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.cli.meta)}
      '';
      wmCommon.keys = [{
        key = "d";
        cmd = "${pkgs.dbms}/bin/dbms";
        mode = "run";
        desktop = "shell";
      }];
    })
  ];
}
