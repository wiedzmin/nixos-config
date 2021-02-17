{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.tmux;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  thumbs-bin = pkgs.rustPlatform.buildRustPackage rec {
    pname = "thumbs-bin";
    version = "unstable-2020-09-07";

    src = pkgs.fetchFromGitHub {
      owner = "fcsonline";
      repo = "tmux-thumbs";
      rev = "fc6d5833a32e7a4d73c85be6712512a6b47cea69";
      sha256 = "1n1qp9z574hrx2p3xx316vwfkjjg37kskpmsigfpz26sdnxldk7r";
      fetchSubmodules = true;
    };
    cargoSha256 = "13z469xgz7478kfgw457fyc8lbf32wjvan1dxhf7kfzfjy15mlf4";
  };
  thumbs = mkDerivationTmux { # FIXME: broken at the moment, fixes pending
    pluginName = "thumbs";
    version = "unstable-2020-09-07";
    rtpFilePath = "tmux-thumbs.tmux";
    src = pkgs.fetchFromGitHub {
      owner = "fcsonline";
      repo = "tmux-thumbs";
      rev = "fc6d5833a32e7a4d73c85be6712512a6b47cea69";
      sha256 = "1n1qp9z574hrx2p3xx316vwfkjjg37kskpmsigfpz26sdnxldk7r";
      fetchSubmodules = true;
    };
    postInstall = ''
      sed -i -e '/BINARY/,$d' $target/tmux-thumbs.tmux
      sed -i -e 's|''${CURRENT_DIR}/target/release/tmux-thumbs|${thumbs-bin}/bin/thumbs|g' $target/tmux-thumbs.sh
      sed -i -e 's|PARAMS=(--dir "''${CURRENT_DIR}")|PARAMS=(-u -r)|g' $target/tmux-thumbs.sh
    '';
  };
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
in {
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
      historyDepth = mkOption { # we should have access not only to visible pane's content
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
      nixpkgs.config.packageOverrides = _: rec {
        tmuxp_sessions = mkPythonScriptWithDeps "tmuxp_sessions" (with pkgs; [ nurpkgs.pystdlib ])
          (readSubstituted ../../subst.nix ./scripts/tmuxp_sessions.py);
      };

      home-manager.users."${user}" = {
        home.file = {
          ".tmuxp/main.yml".text = ''
            session_name: main
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
      workstation.video.transparency.opacityRule = [ ''65:name *?= "${cfg.defaultSession}" && class_g = "Alacritty"'' ];
      custom.programs.tmux = {
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
          fpp
          logging
          prefix-highlight
          sessionist
          {
            plugin = thumbs;
            extraConfig = "set -g @thumbs-key 't'";
          }
          {
            plugin = tmux-fzf-fixed;
            extraConfig = "set -g @tmux-fzf-launch-key 'w'";
          }
          cfg.theme.package
        ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "t" ];
        cmd = "${pkgs.tmuxp_sessions}/bin/tmuxp_sessions";
        mode = "select";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ tmuxp_sessions ]; };
    })
  ];
}
