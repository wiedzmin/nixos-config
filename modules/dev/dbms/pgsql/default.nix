{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dbms.pgsql;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  inherit (hm.xdg) dataHome;
  stable = import inputs.stable {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
in
{
  options = {
    dbms.pgsql = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable PostgreSQL helper tools.";
      };
      pgcli.package = mkOption {
        type = types.package;
        default = stable.pgcli;
        description = "PgCLI log file location.";
      };
      pgcli.logPath = mkOption {
        type = types.str;
        default = "${dataHome}/pgcli/log";
        description = "PgCLI log file location.";
      };
      pgcli.historyPath = mkOption {
        type = types.str;
        default = "${dataHome}/pgcli/history";
        description = "PgCLI history file location.";
      };
      pgcli.namedQueries = mkOption {
        type = types.attrs;
        default = { };
        description = "PgCLI named queries inventory.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ pgcenter cfg.pgcli.package ];
        xdg.configFile.".pgclirc".text = generators.toINI { } {
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
            history_file = cfg.pgcli.historyPath;
            keyring = "False";
            keyword_casing = "auto";
            less_chatty = "False";
            log_file = cfg.pgcli.logPath;
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
          "alias_dsn" = { };
          "data_formats" = {
            decimal = "";
            float = "";
          };
          "named queries" = cfg.pgcli.namedQueries;
        };
      };
    })
  ];
}
