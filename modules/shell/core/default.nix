{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

# nsp>bashate|gdu|shellcheck
# nsp>gdu npkg#gdu
# nsp>bashate npkg#bashate # linter
# nsp>shellcheck npkg#shellcheck

let
  cfg = config.shell.core;
  user = config.attributes.mainUser.name;
in
{
  options = {
    shell.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable core shell setup";
      };
      variables = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Metadata-augmented environment variables registry";
      };
      queueing.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell commands queueing, using `pueue` machinery";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related Emacs infra";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      console.useXkbConfig = true;

      environment.variables = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "global" e && e.global) cfg.variables);
      environment.sessionVariables = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "global" e && e.global) cfg.variables);
      ide.emacs.core.environment = foldl (a: b: a // (envVars b)) { }
        (builtins.filter (e: builtins.hasAttr "emacs" e && e.emacs) cfg.variables);

      home-manager.users."${user}" = {
        programs.readline = {
          enable = true;
          extraConfig = ''
            set echo-control-characters off
          '';
        };
        home.sessionVariables = foldl (a: b: a // (envVars b)) { } cfg.variables;
        programs.television = {
          enable = true;
          settings = {
            tick_rate = 50;
            default_channel = "files";
            history_size = 200;
            global_history = false;
            ui = {
              ui_scale = 100;
              orientation = "landscape";
              theme = "default";
              input_bar = {
                position = "top";
                prompt = ">";
                border_type = "rounded";
              };
              status_bar = {
                separator_open = "";
                separator_close = "";
                hidden = false;
              };
              results_panel = {
                border_type = "rounded";
              };
              preview_panel = {
                size = 50;
                scrollbar = true;
                border_type = "rounded";
                hidden = false;
              };
              help_panel = {
                show_categories = true;
                hidden = true;
              };
              remote_control = {
                show_channel_descriptions = true;
                sort_alphabetically = true;
              };
            };
            keybindings = {
              "esc" = "quit";
              "ctrl-c" = "quit";
              "down" = "select_next_entry";
              "ctrl-n" = "select_next_entry";
              "ctrl-j" = "select_next_entry";
              "up" = "select_prev_entry";
              "ctrl-p" = "select_prev_entry";
              "ctrl-k" = "select_prev_entry";
              "ctrl-up" = "select_prev_history";
              "ctrl-down" = "select_next_history";
              "tab" = "toggle_selection_down";
              "backtab" = "toggle_selection_up";
              "enter" = "confirm_selection";
              "pagedown" = "scroll_preview_half_page_down";
              "pageup" = "scroll_preview_half_page_up";
              "ctrl-y" = "copy_entry_to_clipboard";
              "ctrl-r" = "reload_source";
              "ctrl-s" = "cycle_sources";
              "ctrl-t" = "toggle_remote_control";
              "ctrl-o" = "toggle_preview";
              "ctrl-h" = "toggle_help";
              "f12" = "toggle_status_bar";
              "backspace" = "delete_prev_char";
              "ctrl-w" = "delete_prev_word";
              "ctrl-u" = "delete_line";
              "delete" = "delete_next_char";
              "left" = "go_to_prev_char";
              "right" = "go_to_next_char";
              "home" = "go_to_input_start";
              "ctrl-a" = "go_to_input_start";
              "end" = "go_to_input_end";
              "ctrl-e" = "go_to_input_end";
            };
            events = {
              "mouse-scroll-up" = "scroll_preview_up";
              "mouse-scroll-down" = "scroll_preview_down";
            };
            shell_integration = {
              fallback_channel = "files";
              channel_triggers = {
                "alias" = [
                  "alias"
                  "unalias"
                ];
                "env" = [
                  "export"
                  "unset"
                ];
                "dirs" = [
                  "cd"
                  "ls"
                  "rmdir"
                ];
                "files" = [
                  "cat"
                  "less"
                  "head"
                  "tail"
                  "vim"
                  "nano"
                  "bat"
                  "cp"
                  "mv"
                  "rm"
                  "touch"
                  "chmod"
                  "chown"
                  "ln"
                  "tar"
                  "zip"
                  "unzip"
                  "gzip"
                  "gunzip"
                  "xz"
                ];
                "git-diff" = [
                  "git add"
                  "git restore"
                ];
                "git-branch" = [
                  "git checkout"
                  "git branch"
                  "git merge"
                  "git rebase"
                  "git pull"
                  "git push"
                ];
                "git-log" = [
                  "git log"
                  "git show"
                ];
                "git-repos" = [
                  "nvim"
                  "code"
                  "hx"
                  "git clone"
                ];
              };
              keybindings = {
                "smart_autocomplete" = "ctrl-t";
                "command_history" = "ctrl-r";
              };
            };
          };
        };
        programs.command-not-found = {
          enable = true;
          dbPath = configPrefix roots "modules/shell/core/assets/programs.sqlite";
        };
        home.packages = with pkgs; [
          perl # for plugins
          dtach
        ];
      };
    })
    (mkIf (cfg.enable && cfg.queueing.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ pueue ];
      };
      systemd.user.services."pueued" = {
        description = "Pueue daemon";
        path = [ pkgs.bash ];
        serviceConfig = {
          ExecStart = "${pkgs.pueue}/bin/pueued";
          ExecReload = "${pkgs.pueue}/bin/pueued";
          Restart = "no";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
        wantedBy = [ "multi-user.target" ];
      };
      navigation.bookmarks.entries = {
        pueue-wiki = {
          desc = "Pueue github project wiki";
          url = "https://github.com/Nukesor/pueue/wiki";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        emacs-pueue-repo = {
          desc = "Pueue emacs frontend project repo";
          url = "https://github.com/xFA25E/pueue";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        television-homepage = {
          desc = "Television homepage";
          url = "https://alexpasmantier.github.io/television";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        television-templates-reference = {
          desc = "Television template system reference";
          url = "https://alexpasmantier.github.io/television/advanced/template-system";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        television-string-pipeline = {
          desc = "Docs for Rust crate used to refine Television output";
          url = "https://docs.rs/string_pipeline/latest/string_pipeline";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };

        television-reference-actions = {
          desc = "Television actions reference";
          url = "https://alexpasmantier.github.io/television/reference/actions";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        television-channel-spec = {
          desc = "Television channel specification";
          url = "https://alexpasmantier.github.io/television/reference/channel-spec";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
      };
    })
    (mkIf (cfg.enable && cfg.queueing.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile = {
          "pueue/status-output.jq".text = ''
            .tasks | to_entries | sort_by(.key) | .[] | "\(.value.id) | \(.value.status.Done.result) | \(.value.command) | \(.value.path) | \(.value.status.Done.start | split(".")[0] | strptime("%Y-%m-%dT%H:%M:%S") | strftime("%Y-%m-%d %H:%M:%S")) | \(.value.status.Done.end | split(".")[0] | strptime("%Y-%m-%dT%H:%M:%S") | strftime("%Y-%m-%d %H:%M:%S"))"
          '';
        };
        programs.television = {
          settings.shell_integration.channel_triggers = {
            "pueue-tasks" = [
              "pueue status"
            ];
          };
          channels = {
            pueue-tasks = {
              metadata = {
                name = "pueue-tasks";
                description = "A channel to select Pueue tasks";
                requirements = [ "pueue" "jq" ];
              };
              source = {
                command = "pueue status --json | jq -r -f ${xdgConfig user "/pueue/status-output.jq"}";
              };
              preview = {
                command = "pueue log {split: \| :0}";
              };
            };
          };
        };
      };
      completion.expansions.espanso.matches = {
        shell_core_queueing = {
          matches = [
            {
              trigger = ":pus";
              replace = "pueue status";
            }
            {
              trigger = ":pul";
              replace = "pueue log";
            }
            {
              trigger = ":puc";
              replace = "pueue clean";
            }
            {
              trigger = ":pur";
              replace = "pueue restart $|$";
            }
            {
              trigger = ":pupr";
              replace = "pueue restart --in-place $|$";
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        shell_core = {
          matches = [
            {
              trigger = ":ptim";
              replace = "ps -ef | tv | tr -s ' ' | cut -d' ' -f2 | xargs ps -o pid,lstart,etime -p"; # nsp>television
            }
            {
              trigger = ":ts";
              replace = "$|$ | rtss"; # nsp>rtss npkg#rtss
            }
            {
              trigger = ":ets";
              replace = "ets '$|$'"; # nsp>ets npkg#ets
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ bash-language-server checkbashisms ]; };
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.detached
        epkgs.flycheck-checkbashisms
        epkgs.pueue
      ];
      ide.emacs.core.config = lib.optionalString (!config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst/non-ts.nix ] [ ./elisp/non-ts.el ]) +
        lib.optionalString (config.ide.emacs.core.treesitter.enable) (readSubstituted config inputs pkgs [ ./subst/ts.nix ] [ ./elisp/ts.el ]) +
        (builtins.readFile ./elisp/common.el);
      ide.emacs.core.treesitter.grammars = {
        bash = "https://github.com/tree-sitter/tree-sitter-bash";
      };
      ide.emacs.core.treesitter.modeRemappings = {
        bash-mode = "bash-ts-mode";
        sh-mode = "bash-ts-mode";
      };
    })
  ];
}
