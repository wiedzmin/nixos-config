{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.shell.tmux;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    shell.tmux = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tmux.";
      };
      defaultSession = mkOption {
        description = "Default tmux predefined session name to be used in automation scripts";
        type = types.str;
        default = "main";
      };
      historyDepth = mkOption {
        type = types.int;
        default = 10000;
        description = "Tmux pane's lines count to search (for fzf-tmux-url at the moment)";
      };
      theme.package = mkPackageOption pkgs "tmuxPlugins.gruvbox" { };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ xsel ]; # for `fingers` plugin
        home.file = {
          ".tmuxp/main.yml".text = ''
            session_name: ${cfg.defaultSession}
            windows:
              - window_name: files
                panes:
                  - mc
              - window_name: nixos
                panes:
                  - shell_command:
                    - cd ${configPrefix roots ""}
          '';
        };
      };
      shell.core.variables = [{
        TB_TMUX_SESSION = cfg.defaultSession;
        TB_VT_ORG_TOOL = "tmux";
        global = true;
      }];
      workstation.video.transparency.opacityRules = [ ''65:name *?= "${cfg.defaultSession}" && class_g = "Alacritty"'' ];
      ext.programs.tmux = {
        enable = true;
        baseIndex = 1;
        clock24 = true;
        escapeTime = 0;
        borderStyle = {
          active = "fg=yellow,bg=default";
          inactive = "fg=yellow,bg=default";
        };
        hooks = {
          "after-select-pane" = ''
            "run-shell 'tmux set -g window-active-style bg=brightblack && sleep .05 && tmux set -g window-active-style none'"
          '';
        };
        bindings = {
          root = {
            "C-left" = "prev";
            "C-right" = "next";
            "S-left" = "swap-window -t -1";
            "S-right" = "swap-window -t +1";
            "C-y" = ''run -b "exec </dev/null; ${pkgs.xsel}/bin/xsel -o -b | tmux load-buffer - ; tmux paste-buffer"'';
          };
          prefixed = {
            "*" = "list-clients";
            "l" = "refresh-client";
            "m" = "select-pane -m";
            "|" = ''split-window -h -c "#{pane_current_path}"'';
            "'\\'" = ''split-window -fh -c "#{pane_current_path}"'';
            "-" = ''split-window -v -c "#{pane_current_path}"'';
            "_" = ''split-window -fv -c "#{pane_current_path}"'';
            "'#'" = ''split-window -h -c "#{pane_current_path}"'';
            "@" = ''split-window -v -c "#{pane_current_path}"'';
            "BSpace" = "last-window";
            "r" = ''source-file ~/.tmux.conf \; display "  Config reloaded..."'';
            "y" = "set-window-option synchronize-panes";
            "T" = ''neww -n "Tmux manual" "exec man tmux"'';
            "s" = ''
              split-window -v "tmux list-sessions | cut -d: -f1 | \
                                                     grep -v $(tmux display-message -p '#S') | \
                                                     ${pkgs.fzf}/bin/fzf --reverse | xargs tmux switch-client -t"'';
          };
        };
        extraConfig = ''
          set -g renumber-windows on

          set -g bell-action any
          set -g visual-activity off
          set -g visual-bell off
          set -g visual-silence off
          setw -g monitor-activity on

          set -g set-titles on
          set -g set-titles-string "#S #W #{pane_title}"

          set -g default-shell "/run/current-system/sw/bin/zsh"
        '';
        historyLimit = 102400;
        keyMode = "emacs";
        nestedShortcut = "C-x";
        sensibleOnTop = false;
        shortcut = "M-x";
        terminal = "screen-256color";
        secureSocket = false;
        tmuxp.enable = true;
        plugins = with pkgs.tmuxPlugins; [
          {
            plugin = fzf-tmux-url;
            extraConfig = ''
              set -g @fzf-url-history-limit '${builtins.toString cfg.historyDepth}'
              set -g @fzf-url-bind 'o'
            '';
          }
          copycat
          {
            plugin = extrakto;
            extraConfig = ''
              set -g @extrakto_filter_order "line all word" # FIXME: strangely, defaults are unchangeable now, find a way to patch sources
              EDITOR="${config.ide.emacs.core.package}/bin/emacsclient -c -s /run/user/${config.attributes.mainUser.ID}/emacs/server"
            '';
          }
          fpp
          {
            plugin = fingers;
            extraConfig = "set -g @fingers-hint-format '#[fg=green,bold]%s'";
          }
          logging
          mode-indicator
          prefix-highlight
          sessionist
          {
            plugin = tmux-fzf;
            extraConfig = ''
              TMUX_FZF_LAUNCH_KEY="z"
            '';
          }
          cfg.theme.package
        ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [{
        key = [ "t" ];
        cmd = "${nurpkgs.toolbox}/bin/tmuxctl";
        mode = "select";
        debug = true;
      }];
    })
  ];
}
