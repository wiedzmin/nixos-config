{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dbms.mysql;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  stable = import inputs.stable {
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  };
  inherit (hm.xdg) dataHome;
in {
  options = {
    dbms.mysql = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MySQL helper tools.";
      };
      mycli.logPath = mkOption {
        type = types.str;
        default = "${dataHome}/mycli/log";
        description = "MyCLI log file location.";
      };
      mycli.historyPath = mkOption {
        type = types.str;
        default = "${dataHome}/mycli/history";
        description = "MyCLI history file location.";
      };
      mycli.auditLogPath = mkOption {
        type = types.str;
        default = "${dataHome}/mycli/audit";
        description = "MyCLI audit log file location.";
      };
      mycli.favouriteQueries = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "MyCLI favourite queries inventory.";
      };
      mycli.prompt = mkOption {
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
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.core.variables = [{ MYCLI_HISTFILE = cfg.mycli.historyPath; global = true; }];
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ stable.mycli ];
        xdg.configFile.".myclirc".text = lib.generators.toINI { } {
          main = {
            audit_log = cfg.mycli.auditLogPath;
            auto_vertical_output = "True";
            destructive_warning = "True";
            enable_pager = "True";
            key_bindings = "emacs";
            keyword_casing = "auto";
            less_chatty = "False";
            log_file = cfg.mycli.logPath;
            log_level = "INFO";
            login_path_as_host = "False";
            multi_line = "True";
            null_string = "'<null>'";
            prompt_continuation = "-> ";
            smart_completion = "True";
            syntax_style = "emacs";
            table_format = "fancy_grid";
            timing = "True";
            wider_completion_menu = "False";
            inherit (cfg.mycli) prompt;
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
          "alias_dsn" = { };
          "favorite_queries" = cfg.mycli.favouriteQueries;
        };
      };
    })
  ];
}
