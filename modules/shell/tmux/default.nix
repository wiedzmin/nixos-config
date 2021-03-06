{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.tmux;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  fzf-tmux-url-with-history = mkDerivationTmux {
    pluginName = "fzf-tmux-url";
    rtpFilePath = "fzf-url.tmux";
    src = pkgs.fetchgit {
      url = "https://github.com/wfxr/tmux-fzf-url";
      rev = "ecd518eec1067234598c01e655b048ff9d06ef2f";
      sha256 = "0png8hdv91y2nivq5vdii2192mb2qcrkwwn69lzxrdnbfa27qrgv";
    };
    postPatch = ''
      substituteInPlace fzf-url.sh --replace "capture-pane -J -p" "capture-pane -S -${
        builtins.toString cfg.historyDepth
      } -J -p"
    '';
  };
  tmux-fzf-fixed = mkDerivationTmux {
    pluginName = "tmux-fzf";
    rtpFilePath = "main.tmux";
    version = "unstable-2020-11-23";
    src = pkgs.fetchFromGitHub {
      owner = "sainnhe";
      repo = "tmux-fzf";
      rev = "312685b2a7747b61f1f4a96bd807819f1450479d";
      sha256 = "1z0zmsf8asxs9wbwvkiyd81h93wb2ikl8nxxc26sdpi6l333q5s9";
    };
    postInstall = ''
      find $target -type f -print0 | xargs -0 sed -i -e 's|fzf |${pkgs.fzf}/bin/fzf |g'
      find $target -type f -print0 | xargs -0 sed -i -e 's|sed |${pkgs.gnused}/bin/sed |g'
      find $target -type f -print0 | xargs -0 sed -i -e 's|tput |${pkgs.ncurses}/bin/tput |g'
    '';
    meta = {
      homepage = "https://github.com/sainnhe/tmux-fzf";
      description = "Use fzf to manage your tmux work environment! ";
      longDescription = ''
        Features:
        * Manage sessions (attach, detach*, rename, kill*).
        * Manage windows (switch, link, move, swap, rename, kill*).
        * Manage panes (switch, break, join*, swap, layout, kill*, resize).
        * Multiple selection (support for actions marked by *).
        * Search commands and append to command prompt.
        * Search key bindings and execute.
        * User menu.
        * Popup window support.
      '';
    };
  };
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
        # we should have access not only to visible pane's content
        type = types.int;
        default = 10000;
        description = "Tmux pane's lines count to search (for fzf-tmux-url at the moment)";
      };
      theme.package = mkOption {
        type = types.package;
        default = pkgs.tmuxPlugins.gruvbox;
        description = "Tmux theme package";
      };
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
        home.file = {
          ".tmuxp/main.yml".text = ''
            session_name: ${cfg.defaultSession}
            windows:
              - window_name: repl
                panes:
                  - shell_command:
                    - cd ${wsRoot "github"}/wiedzmin/nixos-config
                    - nix repl ./flake-repl.nix
              - window_name: files
                panes:
                  - mc
              - window_name: nixos
                panes:
                  - shell_command:
                    - cd ${wsRoot "github"}/wiedzmin/nixos-config
          '';
        };
      };
      shell.core.variables = [{ TB_TMUX_SESSION = cfg.defaultSession; global = true; }];
      workstation.video.transparency.opacityRule = [ ''65:name *?= "${cfg.defaultSession}" && class_g = "Alacritty"'' ];
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
            plugin = fzf-tmux-url-with-history;
            extraConfig = "set -g @fzf-url-bind 'o'";
          }
          copycat
          extrakto
          fpp
          {
            plugin = fingers;
            extraConfig = "set -g @fingers-hint-format '#[fg=green,bold]%s'";
          }
          logging
          prefix-highlight
          sessionist
          {
            plugin = tmux-fzf-fixed;
            extraConfig = "set -g @tmux-fzf-launch-key 'w'";
          }
          cfg.theme.package
        ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      home-manager.users."${user}" = {
        # NOTE: temp, until publishing upstream
        home.packages = with pkgs; [ nurpkgs.dmenu-ng rofi ];
      };
      wmCommon.keys = [{
        key = [ "t" ];
        cmd = "${goBinPrefix "tmuxctl"}";
        mode = "select";
        debug = true;
      }];
    })
  ];
}
