{config, pkgs, ...}:
{
    imports = [
        <home-manager/nixos>
        ../contrib/custom-scripts.nix
    ];

    users.extraUsers = {
        alex3rd = {
            isNormalUser = true;
            uid = 1000;
            description = "Alex Ermolov";
            shell = pkgs.zsh;
            extraGroups = [ "audio" "docker" "lp" "networkmanager" "scanner" "vboxusers" "video" "wheel" ];
        };
    };

    nix.trustedUsers = [ "alex3rd" ];

    programs.bash.enableCompletion = true;

    home-manager.users.alex3rd = {
        home.packages = with pkgs; [
            # base
            file
            glibcLocales

            # common
            fbreader

            # email
            notmuch
            msmtp

            alacritty-tmux
            tmux-sessions
        ];
        home.file = {
            ".zsh/functions.zsh".source = ../dotfiles/shell/functions.zsh;
            "common_settings".source = ../dotfiles/shell/common_settings;
            ".Xresources".source = ../dotfiles/x11/Xresources;
            ".sbclrc".text = ''
                #-quicklisp
                (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                                       (user-homedir-pathname))))
                  (when (probe-file quicklisp-init)
                    (load quicklisp-init)))

                (ql:quickload :cffi)
                (pushnew #P"/home/alex3rd/.nix-profile/lib/" ;; TODO: parameterize username
                    cffi:*foreign-library-directories*)
            '';
            "git-assets/git-commit-template".source = ../dotfiles/dev/git-assets/git-commit-template;
            "git-assets/.gitignore".source = ../dotfiles/dev/git-assets/gitignore;
            "git-assets/templates/hooks/pre-push".source = ../dotfiles/dev/git-assets/templates/hooks/pre-push;
            ".pylintrc".source = ../dotfiles/dev/python/pylintrc;
            ".isort.cfg".source = ../dotfiles/dev/python/isort.cfg;
            ".config/flake8".source = ../dotfiles/dev/python/flake8;
            ".config/pycodestyle".source = ../dotfiles/dev/python/pycodestyle;
            ".arbtt/categorize.cfg".source = ../dotfiles/x11/categorize.cfg;
            ".tmux.conf".text = ''
                set -g base-index 1             # first window index
                set -g renumber-windows on
                set -g history-limit 102400
                setw -g pane-base-index 1
                setw -g automatic-rename on
                setw -g aggressive-resize on

                set-option -g default-shell ${pkgs.zsh}/bin/zsh

                set-option -sg escape-time 0 # faster functioning for Esc-bound apps (ex. Vim)
                set-option -g repeat-time 500 # Repeat time limit (ms)

                # set -g update-environment "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"

                # window titles
                set -g set-titles on
                set -g set-titles-string "#h.#I.#T[#W]"

                # activity
                set -g bell-action any
                set -g visual-activity off
                set-option -g visual-bell off
                set-option -g visual-silence off
                setw -g monitor-activity on

                # kill confirmations
                bind k confirm kill-window
                bind K confirm kill-server

                # statusbar
                set -g display-time 2000
                set -g display-panes-time 2000
                set -g status on
                set -g status-interval 1
                set -g status-justify centre
                set -g status-left-length 30
                set -g status-right-length 140
                setw -g window-status-format '#[fg=cyan,dim]#I#[fg=blue]:#[default]#W#[fg=grey,dim]#F'
                setw -g window-status-current-format '#[bg=blue,fg=cyan,bold]#I#[bg=blue,fg=cyan]:#[fg=colour230]#T#[fg=dim]#F'
                set -g status-left '#[fg=green](#S) #(whoami)@#H'
                set -g status-right '#[fg=yellow,dim,bg=default]#(${pkgs.status_uptime}/bin/status_uptime) #[fg=white,bg=default]%a %k:%M:%S %p#[default] #[fg=blue,bright]%Y-%m-%d #[fg=red,bg=default,bright]#(${pkgs.status_bat_info}/bin/status_bat_info)'
                setw -g clock-mode-style 24

                # colors
                set -g status-fg white
                set -g status-bg default
                set -g status-attr default
                setw -g window-status-fg cyan
                setw -g window-status-bg default
                setw -g window-status-attr dim
                setw -g window-status-current-fg colour166 # TODO was white, check if it works somewhere else
                setw -g window-status-current-bg default
                setw -g window-status-current-attr bright
                set -g message-fg white
                set -g message-bg black
                set -g message-attr bright
                setw -g window-status-current-bg red
                set -g pane-border-fg colour235 #base02
                set -g pane-active-border-fg colour240 #base01
                set -g pane-border-bg default
                set -g pane-active-border-bg default
                setw -g clock-mode-colour green #green
                set -g default-terminal "screen-256color"
                set-option -ga terminal-overrides 'xterm*:smcup@:rmcup@,xterm-256color:Tc'

                # keybindings
                unbind C-b
                unbind %
                unbind '"'
                unbind l
                set-option -g prefix M-x
                set -sg escape-time 0
                set -g status-keys emacs
                setw -g mode-keys emacs
                bind -n C-left prev
                bind -n C-right next
                bind -n S-left swap-window -t -1
                bind -n S-right swap-window -t +1
                bind X next-layout
                bind Z previous-layout
                bind b set-option status
                bind '#' split-window -h
                bind '@' split-window -v
                bind -n M-Left select-pane -L
                bind -n M-Right select-pane -R
                bind -n M-Up select-pane -U
                bind -n M-Down select-pane -D
                bind -r H resize-pane -L 5
                bind -r J resize-pane -D 5
                bind -r K resize-pane -U 5
                bind -r L resize-pane -R 5
                bind L choose-session
                # TODO: investigate how session names may be templated in Nix
                bind M-w switch -t work
                bind M-h switch -t housekeeping
                bind M-r switch -t remote
                bind l refresh-client
                bind N command-prompt -p "New session name:" "rename-session %%"
                bind A command-prompt -p "rename-window %%"
                bind '~' split-window "exec ${pkgs.htop}/bin/htop"
                bind '!' split-window "exec sudo ${pkgs.iotop}/bin/iotop"
                bind '#' split-window "exec ${pkgs.networkmanager}/bin/nmtui"
                bind '-' split-window "exec ${pkgs.bc}/bin/bc"
                bind -n C-x send-prefix     # prefix commands for nested tmux sessions
                bind C-m command-prompt -p "Open man page for:" "new-window 'exec man %%'" # open %% man page
                bind T neww -n "Tmux manual" "exec man tmux"
                bind * list-clients
                bind t set status

                # pane/window movement
                bind j command-prompt -p "join pane from:" "join-pane -s '%%'"
                bind b break-pane
                bind s choose-window "join-pane -h -t '%%'"
                bind R move-window -t remote
                bind S command-prompt -p "swap window with:" "swap-window -t ':%%'"
                bind m command-prompt -p "move window to:" "move-window -t ':%%'"

                bind-key BSpace last-window

                # bind-key -t emacs-copy -n M-w copy-pipe "${pkgs.xclip}/bin/xclip -i -selection clipboard" # FIXME fails
                bind C-y run "tmux set-buffer \"$(${pkgs.xclip}/bin/xclip -o -b -sel)\"; tmux paste-buffer"

                bind r source-file ~/.tmux.conf \; display "  Config reloaded..."

                bind-key y set-window-option synchronize-panes

                # set -g mouse-utf8 on
                # set -g mouse on
                # bind -n WheelUpPane   select-pane -t= \; copy-mode -e \; send-keys -M
                # set -g @plugin 'tmux-plugins/tmux-copycat'
                # set -g @plugin 'tmux-plugins/tmux-yank'
                # set -g @plugin 'tmux-plugins/tmux-open'
                # set -g @plugin 'tmux-plugins/tmux-sessionist'
                # set -g @plugin 'tmux-plugins/tmux-resurrect'
                # set -g @plugin 'tmux-plugins/tmux-continuum'
                # set -g @plugin 'tmux-plugins/tmux-urlview'
                # set -g @plugin 'tmux-plugins/tpm'
                # run '~/.tmux/plugins/tpm/tpm'
            '';
            ".config/alacritty/alacritty.yml".text = ''
                # use https://github.com/jwilm/alacritty/blob/master/alacritty.yml for reference
                env:
                  TERM: xterm-256color
                window:
                  dimensions:
                    columns: 80
                    lines: 24
                  padding:
                    x: 2
                    y: 2
                  decorations: true
                tabspaces: 8
                draw_bold_text_with_bright_colors: true
                font:
                  normal:
                    family: Iosevka
                    style: Bold
                  bold:
                    family: Iosevka
                    style: Bold
                  italic:
                    family: Iosevka
                    style: Italic
                  size: 11.0
                render_timer: false
                custom_cursor_colors: false
                # Colors (Oxide)
                colors:
                  primary:
                    background: '0x212121'
                    foreground: '0xC0C5CE'
                  cursor:
                    text: '0x212121'
                    cursor: '0xC0C5CE'
                  normal:
                    black: '0x212121'
                    red: '0xE57373'
                    green: '0xA6BC69'
                    yellow: '0xFAC863'
                    blue: '0x6699CC'
                    magenta: '0xC594C5'
                    cyan: '0x5FB3B3'
                    white: '0xC0C5CE'
                  bright:
                    black: '0x65737E'
                    red: '0xE57373'
                    green: '0xA6BC69'
                    yellow: '0xFAC863'
                    blue: '0x6699CC'
                    magenta: '0xC594C5'
                    cyan: '0x5FB3B3'
                    white: '0xD8DEE9'
                  # TODO: provide dim colors
                visual_bell:
                  animation: EaseOutExpo
                  duration: 1
                background_opacity: 0.8
                mouse_bindings:
                  - { mouse: Middle, action: PasteSelection }
                mouse:
                  double_click: { threshold: 300 }
                  triple_click: { threshold: 300 }
                  faux_scrollback_lines: 1
                selection:
                  semantic_escape_chars: ",│`|:\"' ()[]{}<>"
                dynamic_title: true
                hide_cursor_when_typing: false
                cursor_style: Beam
                live_config_reload: true
                key_bindings:
                  - { key: V,        mods: Control|Shift,    action: Paste               }
                  - { key: C,        mods: Control|Shift,    action: Copy                }
                  - { key: Q,        mods: Command, action: Quit                         }
                  - { key: W,        mods: Command, action: Quit                         }
                  - { key: Insert,   mods: Shift,   action: PasteSelection               }
                  - { key: Key0,     mods: Control, action: ResetFontSize                }
                  - { key: Equals,   mods: Control, action: IncreaseFontSize             }
                  - { key: Subtract, mods: Control, action: DecreaseFontSize             }
                  - { key: Home,                    chars: "\x1bOH",   mode: AppCursor   }
                  - { key: Home,                    chars: "\x1b[H",   mode: ~AppCursor  }
                  - { key: End,                     chars: "\x1bOF",   mode: AppCursor   }
                  - { key: End,                     chars: "\x1b[F",   mode: ~AppCursor  }
                  - { key: PageUp,   mods: Shift,   chars: "\x1b[5;2~"                   }
                  - { key: PageUp,   mods: Control, chars: "\x1b[5;5~"                   }
                  - { key: PageUp,                  chars: "\x1b[5~"                     }
                  - { key: PageDown, mods: Shift,   chars: "\x1b[6;2~"                   }
                  - { key: PageDown, mods: Control, chars: "\x1b[6;5~"                   }
                  - { key: PageDown,                chars: "\x1b[6~"                     }
                  - { key: Tab,      mods: Shift,   chars: "\x1b[Z"                      }
                  - { key: Back,                    chars: "\x7f"                        }
                  - { key: Back,     mods: Alt,     chars: "\x1b\x7f"                    }
                  - { key: Insert,                  chars: "\x1b[2~"                     }
                  - { key: Delete,                  chars: "\x1b[3~"                     }
                  - { key: Left,     mods: Shift,   chars: "\x1b[1;2D"                   }
                  - { key: Left,     mods: Control, chars: "\x1b[1;5D"                   }
                  - { key: Left,     mods: Alt,     chars: "\x1b[1;3D"                   }
                  - { key: Left,                    chars: "\x1b[D",   mode: ~AppCursor  }
                  - { key: Left,                    chars: "\x1bOD",   mode: AppCursor   }
                  - { key: Right,    mods: Shift,   chars: "\x1b[1;2C"                   }
                  - { key: Right,    mods: Control, chars: "\x1b[1;5C"                   }
                  - { key: Right,    mods: Alt,     chars: "\x1b[1;3C"                   }
                  - { key: Right,                   chars: "\x1b[C",   mode: ~AppCursor  }
                  - { key: Right,                   chars: "\x1bOC",   mode: AppCursor   }
                  - { key: Up,       mods: Shift,   chars: "\x1b[1;2A"                   }
                  - { key: Up,       mods: Control, chars: "\x1b[1;5A"                   }
                  - { key: Up,       mods: Alt,     chars: "\x1b[1;3A"                   }
                  - { key: Up,                      chars: "\x1b[A",   mode: ~AppCursor  }
                  - { key: Up,                      chars: "\x1bOA",   mode: AppCursor   }
                  - { key: Down,     mods: Shift,   chars: "\x1b[1;2B"                   }
                  - { key: Down,     mods: Control, chars: "\x1b[1;5B"                   }
                  - { key: Down,     mods: Alt,     chars: "\x1b[1;3B"                   }
                  - { key: Down,                    chars: "\x1b[B",   mode: ~AppCursor  }
                  - { key: Down,                    chars: "\x1bOB",   mode: AppCursor   }
                  - { key: F1,                      chars: "\x1bOP"                      }
                  - { key: F2,                      chars: "\x1bOQ"                      }
                  - { key: F3,                      chars: "\x1bOR"                      }
                  - { key: F4,                      chars: "\x1bOS"                      }
                  - { key: F5,                      chars: "\x1b[15~"                    }
                  - { key: F6,                      chars: "\x1b[17~"                    }
                  - { key: F7,                      chars: "\x1b[18~"                    }
                  - { key: F8,                      chars: "\x1b[19~"                    }
                  - { key: F9,                      chars: "\x1b[20~"                    }
                  - { key: F10,                     chars: "\x1b[21~"                    }
                  - { key: F11,                     chars: "\x1b[23~"                    }
                  - { key: F12,                     chars: "\x1b[24~"                    }
                  - { key: F1,       mods: Shift,   chars: "\x1b[1;2P"                   }
                  - { key: F2,       mods: Shift,   chars: "\x1b[1;2Q"                   }
                  - { key: F3,       mods: Shift,   chars: "\x1b[1;2R"                   }
                  - { key: F4,       mods: Shift,   chars: "\x1b[1;2S"                   }
                  - { key: F5,       mods: Shift,   chars: "\x1b[15;2~"                  }
                  - { key: F6,       mods: Shift,   chars: "\x1b[17;2~"                  }
                  - { key: F7,       mods: Shift,   chars: "\x1b[18;2~"                  }
                  - { key: F8,       mods: Shift,   chars: "\x1b[19;2~"                  }
                  - { key: F9,       mods: Shift,   chars: "\x1b[20;2~"                  }
                  - { key: F10,      mods: Shift,   chars: "\x1b[21;2~"                  }
                  - { key: F11,      mods: Shift,   chars: "\x1b[23;2~"                  }
                  - { key: F12,      mods: Shift,   chars: "\x1b[24;2~"                  }
                  - { key: F1,       mods: Control, chars: "\x1b[1;5P"                   }
                  - { key: F2,       mods: Control, chars: "\x1b[1;5Q"                   }
                  - { key: F3,       mods: Control, chars: "\x1b[1;5R"                   }
                  - { key: F4,       mods: Control, chars: "\x1b[1;5S"                   }
                  - { key: F5,       mods: Control, chars: "\x1b[15;5~"                  }
                  - { key: F6,       mods: Control, chars: "\x1b[17;5~"                  }
                  - { key: F7,       mods: Control, chars: "\x1b[18;5~"                  }
                  - { key: F8,       mods: Control, chars: "\x1b[19;5~"                  }
                  - { key: F9,       mods: Control, chars: "\x1b[20;5~"                  }
                  - { key: F10,      mods: Control, chars: "\x1b[21;5~"                  }
                  - { key: F11,      mods: Control, chars: "\x1b[23;5~"                  }
                  - { key: F12,      mods: Control, chars: "\x1b[24;5~"                  }
                  - { key: F1,       mods: Alt,     chars: "\x1b[1;6P"                   }
                  - { key: F2,       mods: Alt,     chars: "\x1b[1;6Q"                   }
                  - { key: F3,       mods: Alt,     chars: "\x1b[1;6R"                   }
                  - { key: F4,       mods: Alt,     chars: "\x1b[1;6S"                   }
                  - { key: F5,       mods: Alt,     chars: "\x1b[15;6~"                  }
                  - { key: F6,       mods: Alt,     chars: "\x1b[17;6~"                  }
                  - { key: F7,       mods: Alt,     chars: "\x1b[18;6~"                  }
                  - { key: F8,       mods: Alt,     chars: "\x1b[19;6~"                  }
                  - { key: F9,       mods: Alt,     chars: "\x1b[20;6~"                  }
                  - { key: F10,      mods: Alt,     chars: "\x1b[21;6~"                  }
                  - { key: F11,      mods: Alt,     chars: "\x1b[23;6~"                  }
                  - { key: F12,      mods: Alt,     chars: "\x1b[24;6~"                  }
                  - { key: F1,       mods: Super,   chars: "\x1b[1;3P"                   }
                  - { key: F2,       mods: Super,   chars: "\x1b[1;3Q"                   }
                  - { key: F3,       mods: Super,   chars: "\x1b[1;3R"                   }
                  - { key: F4,       mods: Super,   chars: "\x1b[1;3S"                   }
                  - { key: F5,       mods: Super,   chars: "\x1b[15;3~"                  }
                  - { key: F6,       mods: Super,   chars: "\x1b[17;3~"                  }
                  - { key: F7,       mods: Super,   chars: "\x1b[18;3~"                  }
                  - { key: F8,       mods: Super,   chars: "\x1b[19;3~"                  }
                  - { key: F9,       mods: Super,   chars: "\x1b[20;3~"                  }
                  - { key: F10,      mods: Super,   chars: "\x1b[21;3~"                  }
                  - { key: F11,      mods: Super,   chars: "\x1b[23;3~"                  }
                  - { key: F12,      mods: Super,   chars: "\x1b[24;3~"                  }
            '';
            ".config/xmobar/xmobarrc".text = ''
                Config { font = "xft:Iosevka:style=Bold:pixelsize=16"
                       , bgColor = "black"
                       , fgColor = "grey"
                       , position = TopW L 100
                       , lowerOnStart = False
                       , allDesktops = True
                       , persistent = True
                       , commands = [ Run Date "%a %d/%m/%y %H:%M:%S" "date" 10
                                    , Run StdinReader
                                    , Run BatteryP ["BAT0"] ["-t", "<acstatus><left>%(<timeleft>)", "-L", "10", "-H", "80", "-p", "3", "--", "-O",
                                                             "<fc=green>On</fc> - ", "-o", "", "-L", "-15", "-H", "-5", "-l", "red",
                                                             "-m", "blue", "-h", "green"] 200
                                    , Run DiskU [("/", "<free>")] ["-L", "20", "-H", "50", "-m", "1", "-p", "3"] 20
                                    , Run Cpu ["-t", "<total>%", "-m", "3"] 20
                                    , Run Memory ["-t", "<usedratio>% (<cache>M)", "-m", "3"] 20
                                    , Run CoreTemp ["-t","<core0>|<core1>°C",
                                                    "-L","40","-H","60",
                                                    "-l","lightblue",
                                                    "-n","gray90","-h","red"] 50
                                    , Run Com "${pkgs.wifi-status}/bin/wifi-status" [] "wifi" 60
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["openvpn-jobvpn.service", "[V]"] "vpn" 30
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["sshuttle.service", "[S]"] "sshuttle" 30
                                    ]
                       , sepChar = "%"
                       , alignSep = "}{"
                       , template = "%StdinReader% }{ %battery% /:%disku% %cpu% (%coretemp%) %memory% | %wifi% %sshuttle% %vpn% | <fc=#ee9a00>%date%</fc>"
                       }
            '';
        };
        services.dunst = {
            enable = true;
            settings = {
                global = {
                    alignment = "left";
                    allow_markup = "yes";
                    always_run_script = "true";
                    bounce_freq = 0;
                    browser = "firefox -new-tab";
                    dmenu = "/usr/bin/dmenu -p dunst:";
                    ellipsize = "middle";
                    follow = "keyboard";
                    font = "Iosevka Bold 10";
                    force_xinerama = "false";
                    format = "<span foreground='#F3F4F5'><b>%s %p</b></span>\n%b";
                    frame_color = "#232323";
                    frame_width = 1;
                    geometry = "300x5-15+15";
                    hide_duplicates_count = "false";
                    history_length = 20;
                    horizontal_padding = 10;
                    icon_path = "/usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/";
                    icon_position = "left";
                    idle_threshold = 120;
                    ignore_newline = "no";
                    indicate_hidden = "yes";
                    line_height = 0;
                    markup = "full";
                    max_icon_size = 32;
                    monitor = 0;
                    notification_height = 0;
                    padding = 10;
                    separator_color = "frame";
                    separator_height = 2;
                    show_age_threshold = 60;
                    show_indicators = "yes";
                    shrink = "no";
                    sort = "yes";
                    stack_duplicates = "true";
                    startup_notification = "false";
                    sticky_history = "yes";
                    transparency = 0;
                    verbosity = "mesg";
                    word_wrap = "yes";
                };
                frame = {
                    width = 3;
                    color = "#aaaaaa";
                };
                shortcuts = {
                    close = "ctrl+space";
                    close_all = "ctrl+shift+space";
                    history = "ctrl+grave";
                    context = "ctrl+shift+period";
                };
                urgency_low = {
                    background = "#232323";
                    foreground = "#A8A8A8";
                    timeout = 10;
                };
                urgency_normal = {
                    background = "#285577";
                    foreground = "#ffffff";
                    timeout = 10;
                };
                urgency_critical = {
                    background = "#D64E4E";
                    foreground = "#F0E0E0";
                    frame_color = "#D64E4E";
                    timeout = 0;
                    icon = "/usr/share/icons/gentoo/32x32/dia.png";
                };
            };
        };
        programs.zsh = {
            enable = true;
            oh-my-zsh = {
                enable = true;
                plugins = [
                    "colored-man-pages"
                    "dirpersist"
                    "urltools"
                    "virtualenv"
                    "virtualenvwrapper"
                ];
                theme = "muse";
            };
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
                PATH=$PATH:${pkgs.autojump}/bin
                . ${pkgs.autojump}/share/autojump/autojump.zsh

                if [ `uname -s` = "Linux" ]; then
                    eval `dircolors -b`
                fi

                #setopt BEEP
                setopt APPEND_HISTORY
                setopt BRACECCL
                setopt CORRECT_ALL
                setopt EXTENDED_HISTORY
                setopt HIST_EXPIRE_DUPS_FIRST
                setopt HIST_FIND_NO_DUPS
                setopt HIST_IGNORE_ALL_DUPS
                setopt HIST_IGNORE_DUPS
                setopt HIST_IGNORE_SPACE
                setopt HIST_NO_STORE
                setopt HIST_SAVE_NO_DUPS
                setopt SHARE_HISTORY
                setopt autocd
                setopt correctall
                setopt extended_glob
                setopt inc_append_history
                setopt menucomplete

                autoload -Uz compinit && compinit
                autoload -Uz promptinit && promptinit
                autoload -Uz colors && colors
                autoload -Uz vcs_info
                autoload -U dot
                autoload -U predict-on
                autoload run-help
                zmodload zsh/complist

                source ~/.zsh/functions.zsh

                bindkey "\e[3~" delete-char
                bindkey "^qs" fuzzy-search-and-edit
                bindkey ' ' magic-space # also do history expansion on space
                bindkey -e
                bindkey -r "^g"

                zle -N jump && bindkey "^[xjj" jump
                zle -N dot && bindkey . dot
                zle -N fbr && bindkey "^]bb" fbr
                zle -N fco && bindkey "^]ba" fco
                zle -N fcoc && bindkey "^]cc" fcoc
                zle -N fe && bindkey "^qe" fe
                zle -N fshow && bindkey "^]ll" fshow
                zle -N fzf-history-widget && bindkey "^R" fzf-history-widget
                zle -N predict-off
                zle -N predict-on
            '';
            sessionVariables = {
                GREP_OPTIONS = "--color=auto";
                GREP_COLOR = "1;32";
                FZF_MARKS_FILE = "$HOME/.bookmarks";
                GTAGSLIBPATH = "$HOME/.gtags/";
                WORKON_HOME = "$HOME/.virtualenvs";
            };
            shellAliases = {
                "-g findgrep" = "find_in_files";
                "-g fnd" = "findname";
                "-g grep" = "grep --color=auto --perl-regexp";
                # ERR = "2>>( sed -ue 's/.*/$fg_bold[red]&$reset_color/' 1>&2 )";
                # dubc = "sudo find . -name '__pycache__' -or -name '*.pyc' -exec rm -rf {} + && docker-compose up --build";
                TF = "tail -f";
                df = "dfc";
                dud = "(setopt globdots; du -mhs * | sort -hr)";
                git = "${pkgs.gitAndTools.hub}/bin/hub";
                jcurl = "curl_jq(){ ${pkgs.curl}/bin/curl $@ | ${pkgs.jq}/bin/jq . }; curl_jq";
                shme = "ssh_whoami(){ ssh `whoami`@$@}; ssh_whoami";
                shroot = "ssh_root(){ ssh root@$@}; ssh_root";

                ls = "${pkgs.exa}/bin/exa -F --color=auto";
                ll = "${pkgs.exa}/bin/exa -l";
                la = "${pkgs.exa}/bin/exa -A";
                zr = ". ~/.zshrc";
            };
            plugins = [ # TODO: bring back other plugins from old system
                {
                    name = "fzf-marks";
                    file = "fzf-marks.plugin.zsh";
                    src = pkgs.fetchgit {
                        url = "https://github.com/urbainvaes/fzf-marks";
                        rev = "1.1";
                        sha256 = "0wfh267kfvyx7vcyqpqv7qgi6vcffxziq5avqddnbkm3z51br0n4";
                    };
                }
                {
                    name = "enhancd";
                    file = "init.sh";
                    src = pkgs.fetchFromGitHub {
                        owner = "b4b4r07";
                        repo = "enhancd";
                        rev = "v2.2.1";
                        sha256 = "0iqa9j09fwm6nj5rpip87x3hnvbbz9w9ajgm6wkrd5fls8fn8i5g";
                    };
                }
            ];
        };
        programs.git = {
            enable = true;
            userName = "Alex Ermolov";
            userEmail = "aaermolov@gmail.com";
            signing = {
                key = "alex3rd <aaermolov@gmail.com>";
                signByDefault = true;
            };
            extraConfig = ''
                [color]
                    diff = auto
                    status = auto
                    ui = always
                [color "branch"]
                    current = yellow reverse
                    local = yellow
                    remote = green
                [color "diff"]
                    commit = cyan bold
                    frag = magenta bold
                    meta = yellow bold
                    new = green bold
                    old = red bold
                    whitespace = red reverse
                [color "diff-highlight"]
                    newHighlight = green bold 22
                    newNormal = green bold
                    oldHighlight = red bold 52
                    oldNormal = red bold
                [color "status"]
                    added = green
                    changed = yellow
                    untracked = red
                [commit]
                    template = ${config.users.extraUsers.alex3rd.home}/git-assets/git-commit-template
                [core]
                    autocrlf = false
                    excludesfile = ${config.users.extraUsers.alex3rd.home}/git-assets/.gitignore
                    quotepath = false
                [credential]
                    helper = cache --timeout=3600
                [diff]
                    algorithm = patience
                [init]
                    templatedir = ${config.users.extraUsers.alex3rd.home}/git-assets/templates
                [pager]
                    diff = ${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX
                    show = ${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX
                [push]
                    default = current
            '';
            aliases = {
                co = "checkout";
                cwo = "checkout -b origin/"; # fetch branch from primary remote, eliminates ambiguity

                bl = "branch -l";
                merged = "branch --merged master";
                nomerged = "branch --no-merged master";
                fro = "!f() { git fetch && git rebase '@{u}'}; f";

                cs = "commit -S";

                undo = "reset HEAD~1";
                hundo = "reset --hard HEAD~1";

                cont = "shortlog -n -s";
                fc = "!f() { git log --pretty=format:'* %C(yellow)%h%Creset -%C(red)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%cn>%Creset' --decorate --date=relative --grep=$1; }; f";
                hist = "log --pretty=format:\"%h %ad | %s%d [%an]\" --graph --date=short";
                lg = "log --graph --pretty='%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
                who = "shortlog -n -s --no-merges";
                sl = "log --name-only --oneline";
                updated = "show --name-only --oneline";

                # TODO: think of using --patience
                d = "!git diff --patch-with-stat --color $@ | ${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy";
                dc = "diff --cached";
                df = "diff --patch-with-stat --color --color-words --abbrev";
                fpd = "diff --no-prefix -s -p >";
                last = "diff --stat=150,120 HEAD^ HEAD";
                pd = "diff --no-prefix -s -p >";

                pf = "format-patch -1 --no-prefix -s -p FETCH_HEAD";

                s = "status -s";
                st = "status";

                remotes = "remote -v";
                sclone = "clone --depth 1";

                trycl = "git clean -d -x -n -i";
                cleanup = "clean -d -x -f";
                tcleanup = "remote prune origin && git gc && git clean -dfx && git stash clear";
            };
        };
        programs.emacs = {
            enable = true;
            package = (pkgs.emacs26.override { # build Lucid version
                withGTK2 = false;
                withGTK3 = false;
            });
            extraPackages = epkgs: [
                # epkgs.magit-filenotify
                # epkgs.magithub
                # epkgs.vdiff-magit
                epkgs.ace-window
                epkgs.actionscript-mode
                epkgs.aggressive-indent
                epkgs.amx
                epkgs.anaphora
                epkgs.atomic-chrome
                epkgs.auto-compile
                epkgs.avy
                epkgs.avy-flycheck
                epkgs.backup-each-save
                epkgs.backup-walker
                epkgs.banner-comment
                epkgs.beginend
                epkgs.blockdiag-mode
                epkgs.bug-hunter
                epkgs.c-eldoc
                epkgs.cider
                epkgs.clojure-mode
                epkgs.comment-dwim-2
                epkgs.common-lisp-snippets
                epkgs.company
                epkgs.company-ansible
                epkgs.company-c-headers
                epkgs.company-flx
                epkgs.company-go
                epkgs.company-jedi
                epkgs.company-lua
                epkgs.company-quickhelp
                epkgs.company-restclient
                epkgs.company-shell
                epkgs.company-statistics
                epkgs.copy-as-format
                epkgs.counsel
                epkgs.counsel-notmuch
                epkgs.counsel-projectile
                epkgs.counsel-tramp
                epkgs.crux
                epkgs.css-eldoc
                epkgs.csv-mode
                epkgs.darcula-theme
                epkgs.darkburn-theme
                epkgs.delight
                epkgs.diff-hl
                epkgs.dired-filetype-face
                epkgs.dired-hide-dotfiles
                epkgs.dired-narrow
                epkgs.dired-quick-sort
                epkgs.diredfl
                epkgs.docker
                epkgs.docker-compose-mode
                epkgs.docker-tramp
                epkgs.dockerfile-mode
                epkgs.dtrt-indent
                epkgs.edebug-x
                epkgs.edit-server
                epkgs.editorconfig
                epkgs.ein
                epkgs.el-get
                epkgs.eldoc-eval
                epkgs.elfeed
                epkgs.elfeed-goodies
                epkgs.elfeed-org
                epkgs.elisp-slime-nav
                epkgs.emamux
                epkgs.emmet-mode
                epkgs.emms
                epkgs.emms-info-mediainfo
                epkgs.emms-mark-ext
                epkgs.emms-mode-line-cycle
                epkgs.emms-player-simple-mpv
                epkgs.emms-soundcloud
                epkgs.emms-state
                epkgs.exec-path-from-shell
                epkgs.expand-region
                epkgs.f
                epkgs.fic-mode
                epkgs.flycheck
                epkgs.flycheck-clang-analyzer
                epkgs.flycheck-gometalinter
                epkgs.flycheck-pos-tip
                epkgs.function-args
                epkgs.git-msg-prefix
                epkgs.gitignore-mode
                epkgs.go-eldoc
                epkgs.go-guru
                epkgs.go-mode
                epkgs.go-playground
                epkgs.go-tag
                epkgs.godoctor
                epkgs.golden-ratio
                epkgs.google-translate
                epkgs.gorepl-mode
                epkgs.gotest
                epkgs.govet
                epkgs.graphql-mode
                epkgs.haskell-mode
                epkgs.hc-zenburn-theme
                epkgs.highlight-stages
                epkgs.hl-todo
                epkgs.httprepl
                epkgs.hungry-delete
                epkgs.hydra
                epkgs.ialign
                epkgs.ibuffer-vc
                epkgs.imenu-anywhere
                epkgs.info-buffer
                epkgs.info-colors
                epkgs.ini-mode
                epkgs.iqa
                epkgs.ivy
                epkgs.ivy-dired-history
                epkgs.ivy-hydra
                epkgs.ivy-pass
                epkgs.ivy-rich
                epkgs.ivy-xref
                epkgs.jedi-core
                epkgs.jinja2-mode
                epkgs.kaolin-themes
                epkgs.keychain-environment
                epkgs.labburn-theme
                epkgs.link-hint
                epkgs.lua-mode
                epkgs.magit
                epkgs.markdown-mode
                epkgs.mc-extras
                epkgs.melpaStablePackages.slime
                epkgs.mingus
                epkgs.multi-compile
                epkgs.multiple-cursors
                epkgs.mwim
                epkgs.names
                epkgs.nginx-mode
                epkgs.nix-mode
                epkgs.nixos-options
                epkgs.no-littering
                epkgs.nord-theme
                epkgs.notmuch
                epkgs.nov
                epkgs.ob-async
                epkgs.ob-blockdiag
                epkgs.org-alert
                epkgs.org-bullets
                epkgs.org-clock-today
                epkgs.org-dashboard
                epkgs.org-link-minor-mode
                epkgs.org-mru-clock
                epkgs.org-plus-contrib
                epkgs.org-pomodoro
                epkgs.org-randomnote
                epkgs.org-recent-headings
                epkgs.org-rich-yank
                epkgs.org-sticky-header
                epkgs.org-super-agenda
                epkgs.orgit
                epkgs.orglink
                epkgs.paradox
                epkgs.pass
                epkgs.pcre2el
                epkgs.pdf-tools
                epkgs.persistent-scratch
                epkgs.phi-search
                epkgs.phi-search-mc
                epkgs.pip-requirements
                epkgs.plantuml-mode
                epkgs.po-mode
                epkgs.popwin
                epkgs.prog-fill
                epkgs.projectile
                epkgs.quelpa
                epkgs.quelpa-use-package
                epkgs.rainbow-delimiters
                epkgs.rainbow-identifiers
                epkgs.rainbow-mode
                epkgs.rebox2
                epkgs.recentf-ext
                epkgs.recursive-narrow
                epkgs.regex-tool
                epkgs.region-bindings-mode
                epkgs.restart-emacs
                epkgs.reverse-im
                epkgs.russian-holidays
                epkgs.rust-mode
                epkgs.savekill
                epkgs.skeletor
                epkgs.slime-company
                epkgs.smartparens
                epkgs.smooth-scrolling
                epkgs.socyl
                epkgs.solarized-theme
                epkgs.spaceline
                epkgs.sunburn-theme
                epkgs.swiper
                epkgs.tide
                epkgs.tile
                epkgs.transpose-frame
                epkgs.twittering-mode
                epkgs.typescript-mode
                epkgs.undo-tree
                epkgs.unfill
                epkgs.unicode-fonts
                epkgs.use-package-el-get
                epkgs.vagrant-tramp
                epkgs.vimrc-mode
                epkgs.w3m
                epkgs.web-mode
                epkgs.webpaste
                epkgs.wgrep
                epkgs.which-key
                epkgs.whole-line-or-region
                epkgs.windsize
                epkgs.ws-butler
                epkgs.wttrin
                epkgs.yaml-mode
                epkgs.yasnippet
                epkgs.yatemplate
            ];
        };
        programs.htop.enable = true;
        programs.fzf = {
            enable = true;
            enableZshIntegration = true;
        };
        programs.feh.enable = true;
        programs.lesspipe.enable = true;
        programs.man.enable = true;
        programs.info.enable = true;
        programs.rofi = {
            enable = true;
            fullscreen = true;
        };
        programs.browserpass.enable = true;
        programs.command-not-found.enable = true;
        services.unclutter.enable = true;
        services.udiskie.enable = true;
        services.network-manager-applet.enable = true;
        services.random-background = {
            enable = true;
            imageDirectory = "%h/blobs/wallpaper";
            interval = "1w";
        };
        programs.direnv = {
            enable = true;
            enableZshIntegration = true;
        };
    };
}
