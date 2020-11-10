{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.shell;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
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
        builtins.toString cfg.tmux.historyDepth
      } -J -p"
    '';
  };
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
      tmux.historyDepth = mkOption { # we should have access not only to visible pane's content
        type = types.int;
        default = 10000;
        description = "Tmux pane's lines count to search (for fzf-tmux-url at the moment)";
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

      fonts = { fonts = with pkgs; [ powerline-fonts ]; };

      nixpkgs.config.packageOverrides = _: rec {
        tmuxp_sessions = mkPythonScriptWithDeps "tmuxp_sessions" (with pkgs; [ nurpkgs.pystdlib ])
          (readSubstituted ../subst.nix ./scripts/tmuxp_sessions.py);
      };

      home-manager.users."${user}" = {
        xdg.configFile."cod/config.toml".text = ''
          [[rule]]
          executable = "/run/current-system/sw/bin/dephell"
          policy = 'ignore'
        '';
        home.packages = with pkgs;
          [ checkbashism libnotify wmctrl xdotool seturgent shellcheck perl cod thumbs-bin ]
          ++ lib.optionals (cfg.staging.packages != [ ]) cfg.staging.packages;
        home.file = {
          ".tmuxp/main.yml".text = ''
            session_name: main
            windows:
              - window_name: repl
                panes:
                  - shell_command:
                    - cd /etc/nixos
                    - nix repl ./flake-repl.nix
              - window_name: files
                panes:
                  - mc
              - window_name: nixos
                panes:
                  - shell_command:
                    - cd /etc/nixos
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
            source <(cod init $$ zsh)

            ${lib.concatMapStrings (opt: ''
              setopt ${opt}
            '') [ "braceccl" "extendedglob" "menucomplete" ]}

            bindkey '^P' fuzzy-search-and-edit
          '';
          sessionVariables = let dataHome = hm.xdg.dataHome;
          in {
            HISTFILE = "${dataHome}/.histfile";
            LESSHISTFILE = "${dataHome}/.lesshst";
            YSU_IGNORED_ALIASES = [ "g" "ll" ];
            YSU_MODE = "ALL";
          } // lib.optionalAttrs (!cfg.liquidPrompt.enable) { ZSH_COMMAND_TIME_COLOR = "cyan"; };
          shellAliases = {
            cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency
            catb = "${pkgs.bat}/bin/bat -A";

            df = "${pkgs.duf}/bin/duf";
            du = "${pkgs.dua}/bin/dua";

            zr = ". ~/.zshrc";

            vg = "EDITOR=emacsclient ${pkgs.vgrep}/bin/vgrep --interactive --show c5";
          };
          plugins = [
            {
              name = "zsh-notify";
              file = "notify.plugin.zsh";
              src = inputs.zsh-notify;
            }
            {
              name = "zsh-nix-shell";
              file = "nix-shell.plugin.zsh";
              src = inputs.zsh-nix-shell;
            }
            {
              name = "you-should-use";
              file = "you-should-use.plugin.zsh";
              src = inputs.zsh-you-should-use;
            }
            {
              name = "pass-zsh-completion";
              file = "pass-zsh-completion.plugin.zsh";
              src = inputs.pass-zsh-completion;
            }
            {
              name = "zsh-async";
              file = "async.plugin.zsh";
              src = inputs.zsh-async;
            }
            {
              name = "git-extra-commands";
              file = "git-extra-commands.plugin.zsh";
              src = inputs.git-extra-commands;
            }
            {
              name = "zsh-reentry-hook";
              file = "zsh-reentry-hook.plugin.zsh";
              src = inputs.zsh-reentry-hook;
            }
            {
              name = "zsh-fuzzy-search-and-edit";
              file = "plugin.zsh";
              src = inputs.zsh-fuzzy-search-and-edit;
            }
          ] ++ lib.optionals (!cfg.liquidPrompt.enable) [{
            name = "zsh-command-time";
            file = "command-time.plugin.zsh";
            src = inputs.zsh-command-time;
          }] ++ lib.optionals (cfg.liquidPrompt.enable) [{
            name = "liquidprompt";
            file = "liquidprompt.plugin.zsh";
            src = inputs.liquidprompt;
          }] ++ [{
            # NOTE: should be last in the list
            name = "zsh-syntax-highlighting";
            file = "zsh-syntax-highlighting.plugin.zsh";
            src = inputs.zsh-syntax-highlighting;
          }];
        };
      };
    })
    (mkIf (cfg.enable && cfg.alacritty.enable) {
      environment.sessionVariables.TERMINAL = [ "${pkgs.alacritty}/bin/alacritty" ];
      home-manager.users."${user}" = {
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
          copyMode = { "M-n" = ''run-shell "${pkgs.org-capture}/bin/org-capture ns"''; };
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
          gruvbox
          logging
          prefix-highlight
          sessionist
          {
            plugin = thumbs;
            extraConfig = "set -g @thumbs-key 't'";
          }
        ];
      };
    })
    (mkIf cfg.toolsng.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          choose
          fd
          gron
          inputs.nixpkgs-02_06_20.legacyPackages.x86_64-linux.ripgrep-all
          sd
          uq
          vgrep
        ];
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
      home-manager.users."${user}" = {
        home.file = {
          "${cfg.bookmarks.path}".text = localBookmarksKVText (enabledLocals config.custom.navigation.bookmarks.entries);
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
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ checkbashisms nodePackages.bash-language-server ];
      };
      ide.emacs.extraPackages = epkgs: [ epkgs.flycheck-checkbashisms ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/shell.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "t" ];
        cmd = "${pkgs.tmuxp_sessions}/bin/tmuxp_sessions";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ tmuxp_sessions ]; };
    })
  ];
}
