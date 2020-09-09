let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-02_06_20 = import deps.nixpkgs-pinned-02_06_20 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.custom.shell;
in {
  options = {
    custom.shell = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell tooling.";
      };
      alacritty.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Alacritty.";
      };
      terminal = mkOption {
        description = "Default terminal";
        type = types.str;
        visible = false;
        internal = true;
        default = "";
      };
      tmux.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tmux.";
      };
      tmux.defaultSession = mkOption {
        description = "Default tmux predefined session name to be used in automation scripts";
        type = types.str;
        default = "main";
      };
      toolsng.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable successors of some traditional tools like find, sed, etc.";
      };
      ohMyZshPrompt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable oh-my-zsh theming.";
      };
      ohMyZshPrompt.theme = mkOption {
        type = types.str;
        default = "";
        description = "Oh-my-zsh prompt theme name.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        description = "Whether to enable shell bookmarks.";
        default = false;
      };
      bookmarks.path = mkOption {
        type = types.str;
        description = "Where to store shell bookmarks, relative to $HOME.";
        default = ".bookmarks";
      };
      bookmarks.order = mkOption {
        type = types.bool;
        default = false;
        description = "Keep order of Bookmarks.";
      };
      liquidPrompt.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable liquidprompt.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related Emacs infra.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
      staging.packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of staging settings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = (cfg.ohMyZshPrompt.enable && !cfg.liquidPrompt.enable)
            || (!cfg.ohMyZshPrompt.enable && cfg.liquidPrompt.enable)
            || (!cfg.ohMyZshPrompt.enable && !cfg.liquidPrompt.enable);
          message = "shell: exactly one or no theming should be used.";
        }
        {
          assertion = cfg.liquidPrompt.enable || (cfg.ohMyZshPrompt.enable && cfg.ohMyZshPrompt.theme != "");
          message = "shell: oh-my-zsh prompt theming enabled but no theme name provided.";
        }
      ];

      nixpkgs.config.packageOverrides = _: rec {
        tmuxp_sessions = mkPythonScriptWithDeps "tmuxp_sessions" (with pkgs; [ pystdlib ]) (builtins.readFile
          (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/tmuxp_sessions.py; })));
      };

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [ checkbashism libnotify wmctrl xdotool seturgent shellcheck ]
          ++ lib.optionals (cfg.staging.packages != [ ]) cfg.staging.packages;
        home.file = {
          ".tmuxp/main.yml".text = ''
            session_name: main
            windows:
              - window_name: repl
                panes:
                  - nix repl '<nixpkgs/nixos>'
              - window_name: files
                panes:
                  - mc
          '';
        } // lib.optionalAttrs (cfg.liquidPrompt.enable) {
          ".lp.ps1".text = builtins.readFile ./misc/.lp.ps1;
          ".liquidpromptrc".text = builtins.readFile ./misc/.liquidpromptrc;
        };
        programs.readline = {
          enable = true;
          extraConfig = ''
            set echo-control-characters off
          '';
        };
        programs.command-not-found = {
          enable = true;
          dbPath = ../../../assets/blobs/programs.sqlite;
        };
        programs.zsh = {
          enable = true;
          oh-my-zsh = {
            enable = true;
            plugins = [ "colored-man-pages" "urltools" ];
          } // lib.optionalAttrs (cfg.ohMyZshPrompt.enable) { theme = cfg.ohMyZshPrompt.theme; };
          enableAutosuggestions = true;
          enableCompletion = true;
          history = {
            size = 10000;
            save = 10000;
            path = ".histfile";
            ignoreDups = true;
            expireDuplicatesFirst = true;
            extended = true;
            share = true;
          };
          initExtra = ''
            setopt APPEND_HISTORY
            setopt BRACE_CCL
            setopt HIST_FIND_NO_DUPS
            setopt HIST_IGNORE_ALL_DUPS
            setopt HIST_IGNORE_SPACE
            setopt HIST_NO_STORE
            setopt HIST_SAVE_NO_DUPS
            setopt AUTO_CD
            setopt EXTENDED_GLOB
            setopt INC_APPEND_HISTORY
            setopt MENU_COMPLETE

            ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

            ${lib.concatMapStrings (opt: ''
              setopt ${opt}
            '') [ "braceccl" "extendedglob" "menucomplete" ]}

            bindkey '^P' fuzzy-search-and-edit
          '';
          sessionVariables = let dataHome = config.home-manager.users."${config.attributes.mainUser.name}".xdg.dataHome;
          in {
            HISTFILE = "${dataHome}/.histfile";
            LESSHISTFILE = "${dataHome}/.lesshst";
            YSU_IGNORED_ALIASES = [ "g" "ll" ];
            YSU_MODE = "ALL";
          } // lib.optionalAttrs (!cfg.liquidPrompt.enable) { ZSH_COMMAND_TIME_COLOR = "cyan"; };
          shellAliases = {
            cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency

            df = "${pkgs.dfc}/bin/dfc";
            du = "${pkgs.dua}/bin/dua";

            zr = ". ~/.zshrc";

            vg = "EDITOR=emacsclient ${pkgs.vgrep}/bin/vgrep --interactive --show c5";
          };
          plugins = [
            {
              name = "zsh-notify";
              file = "notify.plugin.zsh";
              src = deps.zsh-notify;
            }
            {
              name = "zsh-nix-shell";
              file = "nix-shell.plugin.zsh";
              src = deps.zsh-nix-shell;
            }
            {
              name = "you-should-use";
              file = "you-should-use.plugin.zsh";
              src = deps.zsh-you-should-use;
            }
            {
              name = "pass-zsh-completion";
              file = "pass-zsh-completion.plugin.zsh";
              src = deps.pass-zsh-completion;
            }
            {
              name = "zsh-async";
              file = "async.plugin.zsh";
              src = deps.zsh-async;
            }
            {
              name = "git-extra-commands";
              file = "git-extra-commands.plugin.zsh";
              src = deps.git-extra-commands;
            }
            {
              name = "zsh-reentry-hook";
              file = "zsh-reentry-hook.plugin.zsh";
              src = deps.zsh-reentry-hook;
            }
            {
              name = "zsh-fuzzy-search-and-edit";
              file = "plugin.zsh";
              src = deps.zsh-fuzzy-search-and-edit;
            }
          ] ++ lib.optionals (!cfg.liquidPrompt.enable) [{
            name = "zsh-command-time";
            file = "command-time.plugin.zsh";
            src = deps.zsh-command-time;
          }] ++ lib.optionals (cfg.liquidPrompt.enable) [{
            name = "liquidprompt";
            file = "liquidprompt.plugin.zsh";
            src = deps.liquidprompt;
          }] ++ [{
            # NOTE: should be last in the list
            name = "zsh-syntax-highlighting";
            file = "zsh-syntax-highlighting.plugin.zsh";
            src = deps.zsh-syntax-highlighting;
          }];
        };
      };
    })
    (mkIf (cfg.enable && cfg.alacritty.enable) {
      environment.sessionVariables.TERMINAL = [ "${pkgs.alacritty}/bin/alacritty" ];
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.alacritty = {
          enable = true;
          settings = {
            env = {
              LESS = "-SRXF";
              TERM = "xterm-256color";
            };
            window = {
              padding = {
                x = 2;
                y = 2;
              };
              decorations = "full";
              dynamic_title = true;
            };
            draw_bold_text_with_bright_colors = true;
            bell = {
              animation = "EaseOutExpo";
              duration = 0;
            };
            mouse_bindings = [{
              mouse = "Middle";
              action = "PasteSelection";
            }];
            background_opacity = 0.5;
            selection = { semantic_escape_chars = '',│`|:"' ()[]{}<>''; };
            cursor = { style = "Beam"; };
            live_config_reload = true;
          };
        };
      };
      custom.shell.terminal = "${pkgs.alacritty}/bin/alacritty";
      custom.xinput.xkeysnail.rc = ''
        define_keymap(re.compile("Alacritty"), {
            K("C-x"): {
                K("k"): K("C-d"),
            },
        }, "Alacritty")
      '';
    })
    (mkIf (cfg.enable && cfg.tmux.enable) {
      custom.programs.tmux = {
        enable = true;
        baseIndex = 1;
        clock24 = true;
        escapeTime = 0;
        status = {
          currentWindowFormat = "#[bg=blue,fg=cyan,bold]#I#[bg=blue,fg=cyan]:#[fg=colour230]#T#[fg=dim]#F";
          windowFormat = "#[fg=cyan,dim]#I#[fg=blue]:#[default]#W#[fg=grey,dim]#F";
          leftFormat = "#{prefix_highlight}#[fg=green](#S) #(whoami)@#H";
          rightFormat = "#[fg=blue,bright]%k:%M %d/%m/%Y";
          style = "fg=white,bg=default,default";
          windowStyle = "fg=cyan,bg=default,dim";
          currentWindowStyle = "fg=colour166,bg=red,bright";
          messageStyle = "fg=white,bg=black,bright";
        };
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
          copyMode = { "M-n" = ''run-shell "org-capture ns"''; };
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
            plugin = pkgs.fzf-tmux-url-with-history; # patched version, see overlays
            extraConfig = "set -g @fzf-url-bind 'o'";
          }
          copycat
          # fingers # broken at the moment, retry later
          fpp
          logging
          prefix-highlight
          sessionist
        ];
      };
    })
    (mkIf cfg.toolsng.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ fd gron nixpkgs-pinned-02_06_20.ripgrep-all sd uq vgrep ];
        programs = {
          lsd = {
            enable = true;
            enableAliases = true;
          };
          bat = {
            enable = true;
            config = {
              theme = "TwoDark";
              pager = "less -FR";
            };
          };
          jq = {
            enable = true;
            colors = {
              null = "1;30";
              false = "0;91";
              true = "0;92";
              numbers = "0;36";
              strings = "1;96";
              arrays = "1;94";
              objects = "1;33";
            };
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          "${cfg.bookmarks.path}".text = lib.concatStringsSep "\n"
            (lib.mapAttrsToList (name: path: name + " : " + path) config.custom.navigation.bookmarks.entries);
        };
        programs.fzf.enable = true;
        programs.zsh = {
          sessionVariables = {
            FZF_MARKS_FILE = "$HOME/${cfg.bookmarks.path}";
            FZF_MARKS_JUMP = "^[[1;5P";
          } // lib.optionalAttrs (cfg.bookmarks.order) { FZF_MARKS_KEEP_ORDER = "1"; };
          plugins = [{
            name = "fzf-marks";
            file = "fzf-marks.plugin.zsh";
            src = pkgs.fetchFromGitHub {
              owner = "urbainvaes";
              repo = "fzf-marks";
              rev = "f2e8844ce813f8ad35a1903eb8c680c4492e153b";
              sha256 = "0a8jlwc12m0xid2v4d7rxzci91w8qrc4x91jq4lv0lm62v2w4n1j";
            };
          }];
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ checkbashisms nodePackages.bash-language-server ];
      };
      ide.emacs.extraPackages = epkgs: [ epkgs.flycheck-checkbashisms ];
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./emacs/shell.el; }));

    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "t" ];
        cmd = "${pkgs.tmuxp_sessions}/bin/tmuxp_sessions";
        mode = "run";
      }];
    })
  ];
}
