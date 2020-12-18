{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.tools.dbms;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  nixpkgs-litecli = import inputs.nixpkgs-03_12_20 ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });

  dataHome = hm.xdg.dataHome;
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
        default = "${dataHome}/pgcli/log";
        description = "PgCLI log file location.";
      };
      postgresql.historyPath = mkOption {
        type = types.str;
        default = "${dataHome}/pgcli/history";
        description = "PgCLI history file location.";
      };
      mysql.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MySQL helper tools.";
      };
      mysql.logPath = mkOption {
        type = types.str;
        default = "${dataHome}/mycli/log";
        description = "MyCLI log file location.";
      };
      mysql.historyPath = mkOption {
        type = types.str;
        default = "${dataHome}/mycli/history";
        description = "MyCLI history file location.";
      };
      mysql.auditLogPath = mkOption {
        type = types.str;
        default = "${dataHome}/mycli/audit";
        description = "MyCLI audit log file location.";
      };
      mysql.prompt = mkOption {
        type = types.str;
        default = "\\t \\u@\\h:\\d> ";
        description = ''
          MySQL prompt
          \D - The full current date
          \d - Database name
          \h - Hostname of the server
          \m - Minutes of the current time
          \n - Newline
          \P - AM/PM
          \p - Port
          \R - The current time, in 24-hour military time (0–23)
          \r - The current time, standard 12-hour time (1–12)
          \s - Seconds of the current time
          \t - Product type (Percona, MySQL, MariaDB)
          \u - Username
        '';
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
      home-manager.users.${user} = {
        home.packages = with pkgs; [ pgcenter pgcli ];
        xdg.configFile.".pgclirc".text = lib.generators.toINI { } {
          main = {
            asterisk_column_order = "table_order";
            auto_expand = "False";
            case_column_headers = "True";
            casing_file = "default";
            destructive_warning = "True";
            enable_pager = "True";
            expand = "True";
            generate_aliases = "False";
            generate_casing_file = "False";
            history_file = cfg.postgresql.historyPath;
            keyring = "False";
            keyword_casing = "auto";
            less_chatty = "False";
            log_file = cfg.postgresql.logPath;
            log_level = "INFO";
            min_num_menu_lines = "10";
            multi_line = "False";
            multi_line_mode = "psql";
            multiline_continuation_char = "''";
            null_string = "'<null>'";
            on_error = "STOP";
            qualify_columns = "if_more_than_one_table";
            row_limit = "1000";
            search_path_filter = "False";
            smart_completion = "True";
            syntax_style = "emacs";
            table_format = "psql";
            timing = "True";
            vi = "False";
            wider_completion_menu = "False";
          };
          colors = {
            "arg-toolbar" = "'noinherit bold'";
            "arg-toolbar.text" = "'nobold'";
            "bottom-toolbar" = "'bg:#222222 #aaaaaa'";
            "bottom-toolbar.off" = "'bg:#222222 #888888'";
            "bottom-toolbar.on" = "'bg:#222222 #ffffff'";
            "bottom-toolbar.transaction.failed" = "'bg:#222222 #ff005f bold'";
            "bottom-toolbar.transaction.valid" = "'bg:#222222 #00ff5f bold'";
            "completion-menu.completion" = "'bg:#008888 #ffffff'";
            "completion-menu.completion.current" = "'bg:#ffffff #000000'";
            "completion-menu.meta.completion" = "'bg:#448888 #ffffff'";
            "completion-menu.meta.completion.current" = "'bg:#44aaaa #000000'";
            "completion-menu.multi-column-meta" = "'bg:#aaffff #000000'";
            "output.even-row" = "";
            "output.header" = "'#00ff5f bold'";
            "output.odd-row" = "";
            "scrollbar" = "'bg:#00aaaa'";
            "scrollbar.arrow" = "'bg:#003333'";
            "search" = "'#ffffff bg:#4444aa'";
            "search-toolbar" = "'noinherit bold'";
            "search-toolbar.text" = "'nobold'";
            "search.current" = "'#ffffff bg:#44aa44'";
            "selected" = "'#ffffff bg:#6666aa'";
            "system-toolbar" = "'noinherit bold'";
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
      environment.variables.MYCLI_HISTFILE = cfg.mysql.historyPath;
      home-manager.users.${user} = {
        home.packages = with pkgs; [ mycli ];
        xdg.configFile.".myclirc".text = lib.generators.toINI { } {
          main = {
            audit_log = cfg.mysql.auditLogPath;
            auto_vertical_output = "True";
            destructive_warning = "True";
            enable_pager = "True";
            key_bindings = "emacs";
            keyword_casing = "auto";
            less_chatty = "False";
            log_file = cfg.mysql.logPath;
            log_level = "INFO";
            login_path_as_host = "False";
            multi_line = "True";
            null_string = "'<null>'";
            prompt = cfg.mysql.prompt;
            prompt_continuation = "-> ";
            smart_completion = "True";
            syntax_style = "emacs";
            table_format = "fancy_grid";
            timing = "True";
            wider_completion_menu = "False";
          };
          colors = {
            "Token.Menu.Completions.Completion.Current" = "bg:#00aaaa #000000";
            "Token.Menu.Completions.Completion" = "bg:#008888 #ffffff";
            "Token.Menu.Completions.MultiColumnMeta" = "bg:#aaffff #000000";
            "Token.Menu.Completions.ProgressButton" = "bg:#003333";
            "Token.Menu.Completions.ProgressBar" = "bg:#00aaaa";
            "Token.Output.Header" = "'bold'";
            "Token.Output.OddRow" = "";
            "Token.Output.EvenRow" = "";
            "Token.SelectedText" = "#ffffff bg:#6666aa";
            "Token.SearchMatch" = "'#ffffff bg:#4444aa'";
            "Token.SearchMatch.Current" = "#ffffff bg:#44aa44";
            "Token.Toolbar" = "bg:#222222 #aaaaaa";
            "Token.Toolbar.Off" = "bg:#222222 #888888";
            "Token.Toolbar.On" = "bg:#222222 #ffffff";
            "Token.Toolbar.Search" = "noinherit bold";
            "Token.Toolbar.Search.Text" = "nobold";
            "Token.Toolbar.System" = "noinherit bold";
            "Token.Toolbar.Arg" = "noinherit bold";
            "Token.Toolbar.Arg.Text" = "nobold";
          };
          "favorite_queries" = { };
          "alias_dsn" = { };
        };
      };
    })
    (mkIf cfg.sqlite.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          sqlitebrowser
          nixpkgs-litecli.litecli # TODO: shell automation: fzf for selecting db file, you get the idea
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users.${user} = { home.packages = with pkgs; [ nodePackages.elasticdump ]; };
    })
    (mkIf (cfg.cli.enable && cfg.wm.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        dbms = mkPythonScriptWithDeps "dbms" (with pkgs; [ pass nurpkgs.pystdlib python3Packages.redis tmux vpnctl ])
          (readSubstituted ../../subst.nix ./scripts/dbms.py);
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set misc/dbms_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.cli.meta)}
      '';
      wmCommon.keys = [{
        key = [ "d" ];
        cmd = "${pkgs.dbms}/bin/dbms";
        mode = "dev";
        desktop = "shell";
      }];
    })
    (mkIf (config.attributes.debug.scripts) { home-manager.users.${user} = { home.packages = with pkgs; [ dbms ]; }; })
  ];
}
