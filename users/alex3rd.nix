{config, pkgs, lib, ...}:

let
    # TODO: try to make simpler / more concise
    currentArch = pkgs.lib.lists.last (pkgs.lib.lists.init (pkgs.lib.strings.splitString "-" builtins.currentSystem));
    currentOs = pkgs.lib.lists.last (pkgs.lib.strings.splitString "-" builtins.currentSystem);
    tmuxPluginsBundle = with pkgs; [
        tmuxPlugins.battery
        tmuxPlugins.copycat
        tmuxPlugins.cpu
        tmuxPlugins.fpp
        tmuxPlugins.fzf-tmux-url
        tmuxPlugins.logging
        tmuxPlugins.pain-control
        tmuxPlugins.prefix-highlight
        tmuxPlugins.resurrect
        tmuxPlugins.yank
        tmuxPlugins.sessionist
    ];
in
{
    imports = [
        <home-manager/nixos>
        ../contrib/custom-scripts.nix
        ../contrib/custom-desktop-items.nix
        ../private/hometraits.nix
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

    system.activationScripts.removeOldTaffybarBinary = ''
        TAFFYBAR_CACHED_BINARY=${config.users.extraUsers.alex3rd.home}/.cache/taffybar/taffybar-${currentOs}-${currentArch}
        if [ -f $TAFFYBAR_CACHED_BINARY ]; then
            rm $TAFFYBAR_CACHED_BINARY
        fi
    '';

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

            tmuxp

            # custom tmux
            alacritty-tmux
            tmux-sessions
            shell-capture

            # custom
            rescale-wallpaper

            # NAS
            mount_nas_volume
            unmount_nas_volume

            org-protocol.desktop
        ];
        home.file = {
            ".zsh/functions.zsh".source = ../dotfiles/shell/functions.zsh;
            "common_settings".source = ../dotfiles/shell/common_settings;
            ".bookmarks".source = ../private/fzf-bookmarks;
            "nas_volumes".source = ../private/catscan_volumes; # NAS volumes list
            ".Xresources".source = ../dotfiles/x11/Xresources;
            ".config/taffybar/taffybar.hs".source = ../dotfiles/x11/taffybar/taffybar.hs;
            ".config/taffybar/taffybar.rc".source = ../dotfiles/x11/taffybar/taffybar.rc;
            ".config/autorandr/predetect" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash
                    pkill -f compton
                '';
            };
            ".config/autorandr/postswitch" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash
                    ${pkgs.feh}/bin/feh --bg-fill ${config.x11.wallpapers_dir}/${config.x11.current_wallpaper}
                '';
            };
            ".config/autorandr/mobile/config".source = ../hardware/autorandr/x230/mobile/config;
            ".config/autorandr/mobile/setup".source = ../hardware/autorandr/x230/mobile/setup;
            ".config/autorandr/docked-home/config".source = ../hardware/autorandr/x230/docked-home/config;
            ".config/autorandr/docked-home/setup".source = ../hardware/autorandr/x230/docked-home/setup;
            ".config/autorandr/docked-office-double/config".source = ../hardware/autorandr/x230/docked-office-double/config;
            ".config/autorandr/docked-office-double/setup".source = ../hardware/autorandr/x230/docked-office-double/setup;
            ".sbclrc".source = ../dotfiles/dev/_sbclrc;
            "git-assets/git-commit-template".source = ../dotfiles/dev/git-assets/git-commit-template;
            "git-assets/.gitignore".source = ../dotfiles/dev/git-assets/gitignore;
            "git-assets/templates/hooks/pre-push".source = ../dotfiles/dev/git-assets/templates/hooks/pre-push;
            ".config/pass-git-helper/git-pass-mapping.ini".source = ../dotfiles/dev/git-assets/git-pass-mapping.ini;
            ".pylintrc".source = ../dotfiles/dev/python/pylintrc;
            ".isort.cfg".source = ../dotfiles/dev/python/isort.cfg;
            ".config/flake8".source = ../dotfiles/dev/python/flake8;
            ".config/pycodestyle".source = ../dotfiles/dev/python/pycodestyle;
            ".arbtt/categorize.cfg".text = ''
                aliases (
                        "Navigator" -> "Firefox",
                        "Zathura" -> "PDF reader",
                        "telegram-desktop" -> "Telegram",
                        "Alacritty" -> "Shell",
                )

                $idle > 60 ==> tag inactive,
                current window $program == ["Navigator", "Google-chrome", "Google-chrome-stable"] ==> tag activity:web,
                current window $program == ["Zathura"] ==> tag activity:pdf,
                current window $program == ["FBReader"] ==> tag activity:fiction,

                tag apps:$current.program, -- just tags the current program

                -- projects at work
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
                  ==> tag project:$1-$2,
                current window ($program == "Alacritty" && $title =~ m!(?:~|home/alex3rd)/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
                  ==> tag project:$1-$2,

                -- personal projects
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/workspace/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/!)
                  ==> tag project:$1-$2,
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/.xmonad/!) ==> tag project:xmonad-config,
                current window ($program == "Alacritty" && $title =~ m!(?:~|home/alex3rd)/.xmonad/!) ==> tag project:xmonad-config,
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/.emacs.d/!) ==> tag project:emacs-config,
                current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,

                current window ($program == "Navigator" && $title =~ /Facebook/) ==> tag site:facebook,
                current window ($program == "Navigator" && $title =~ /Gmail/) ==> tag web:Gmail,
                current window ($program == "Navigator" && $title =~ /Google/) ==> tag web:Google,
                current window ($program == "Navigator" && $title =~ /wikipedia/) ==> tag site:wikipedia,
                current window ($program == "Navigator" && $title =~ /habr/) ==> tag site:habr,
                current window ($program == "Navigator" && $title =~ /pypi/) ==> tag site:pypi,
                current window ($program == "Navigator" && $title =~ /stackoverflow/) ==> tag site:stackoverflow,

                current window ($title =~ /^emacs - [^ ]+\.c .*$/) ==> tag edit:c,
                current window ($title =~ /^emacs - [^ ]+\.py .*$/) ==> tag edit:python,
                current window ($title =~ /^emacs - [^ ]+\.hs .*$/) ==> tag edit:haskell,
                current window ($title =~ /^emacs - [^ ]+\.lisp .*$/) ==> tag edit:cl,
                current window ($title =~ /^emacs - [^ ]+\.el .*$/) ==> tag edit:elisp,
                current window ($title =~ /^emacs - [^ ]+config\.org .*$/) ==> tag edit:elisp,
                current window ($title =~ /^emacs - [^ ]+\.pdf .*$/) ==> tag activity:pdf,

                -- $time evaluates to local time.
                $time >=  2:00 && $time <  8:00 ==> tag time-of-day:night,
                $time >=  8:00 && $time < 12:00 ==> tag time-of-day:morning,
                $time >= 12:00 && $time < 14:00 ==> tag time-of-day:lunchtime,
                $time >= 14:00 && $time < 18:00 ==> tag time-of-day:afternoon,
                $time >= 18:00 && $time < 22:00 ==> tag time-of-day:evening,
                $time >= 22:00 || $time <  2:00 ==> tag time-of-day:late-evening,

                -- This tag always refers to the last 24h
                $sampleage <= 24:00 ==> tag last-day,
                -- ...and last hour respectively
                $sampleage <= 1:00 ==> tag last-hour,

                -- year/months
                year $date == 2014 ==> tag year:2014,
                year $date == 2015 ==> tag year:2015,
                year $date == 2016 ==> tag year:2016,
                year $date == 2017 ==> tag year:2017,
                year $date == 2018 ==> tag year:2018,
                month $date == 1 ==> tag month:January,
                month $date == 2 ==> tag month:February,
                month $date == 3 ==> tag month:March,
                month $date == 4 ==> tag month:April,
                month $date == 5 ==> tag month:May,
                month $date == 6 ==> tag month:June,
                month $date == 7 ==> tag month:July,
                month $date == 8 ==> tag month:August,
                month $date == 9 ==> tag month:September,
                month $date == 10 ==> tag month:October,
                month $date == 11 ==> tag month:November,
                month $date == 12 ==> tag month:December,

                day of month $now == day of month $date ==> tag current-day,
                day of week $date == 1 ==> tag week:Monday,
                day of week $date == 2 ==> tag week:Tuesday,
                day of week $date == 3 ==> tag week:Wednesday,
                day of week $date == 4 ==> tag week:Thursday,
                day of week $date == 5 ==> tag week:Friday,
                day of week $date == 6 ==> tag week:Saturday,
                day of week $date == 7 ==> tag week:Sunday,

                month $now == month $date ==> tag current-month,
                year $now == year $date ==> tag current-year,

                ${builtins.readFile "/etc/nixos/private/categorize_private.cfg"}
            '';
            ".tmux.conf".text = ''
                # indexes
                set -g base-index 1             # first window index
                set -g renumber-windows on
                setw -g pane-base-index 1

                # misc
                set -g default-shell ${pkgs.zsh}/bin/zsh
                set -g repeat-time 1000
                set -g history-limit 102400
                set -g mouse on
                set -g prefix M-x
                set -g status-keys emacs
                set -sg escape-time 0 # faster functioning for Esc-bound apps (ex. Vim)
                setw -g aggressive-resize on
                setw -g automatic-rename on
                setw -g mode-keys emacs
                set -g set-titles on
                set -g set-titles-string '#(${pkgs.maybe_ssh_host}/bin/maybe_ssh_host):#(pwd="#{pane_current_path}"; echo $pwd)'

                # activity
                set -g bell-action any
                set -g visual-activity off
                set -g visual-bell off
                set -g visual-silence off
                setw -g monitor-activity on

                # statusbar
                set -g display-panes-time 2000
                set -g display-time 2000
                set -g status on
                set -g status-interval 1
                set -g status-justify centre
                set -g status-left '#{prefix_highlight}#[fg=green](#S) #(whoami)@#H'
                set -g status-left-length 30
                set -g status-right '#[fg=blue,bright]%k:%M:%S %d/%m/%Y \
                | #{cpu_fg_color}#{cpu_icon}#{cpu_percentage}'
                set -g status-right-length 140
                setw -g clock-mode-style 24
                setw -g window-status-current-format \
                        '#[bg=blue,fg=cyan,bold]#I#[bg=blue,fg=cyan]:#[fg=colour230]#T#[fg=dim]#F'
                setw -g window-status-format '#[fg=cyan,dim]#I#[fg=blue]:#[default]#W#[fg=grey,dim]#F'

                # colors
                set -g default-terminal "screen-256color"
                set -g message-attr bright
                set -g message-bg black
                set -g message-fg white
                set -g pane-active-border-bg default
                set -g pane-active-border-fg colour240 #base01
                set -g pane-border-bg default
                set -g pane-border-fg colour235 #base02
                set -g status-attr default
                set -g status-bg default
                set -g status-fg white
                set -ga terminal-overrides 'xterm*:smcup@:rmcup@,xterm-256color:Tc'
                setw -g clock-mode-colour green #green
                setw -g window-status-attr dim
                setw -g window-status-bg default
                setw -g window-status-current-attr bright
                setw -g window-status-current-bg default
                setw -g window-status-current-bg red
                setw -g window-status-current-fg colour166 # TODO was white, check if it works somewhere else
                setw -g window-status-fg cyan

                # TODO: investigate how session names may be templated in Nix
                # keybindings
                unbind C-b

                bind * list-clients
                bind -n C-left prev
                bind -n C-right next
                bind -n C-x send-prefix     # prefix commands for nested tmux sessions
                bind -n S-left swap-window -t -1
                bind -n S-right swap-window -t +1
                bind T neww -n "Tmux manual" "exec man tmux"
                bind l refresh-client
                bind m select-pane -m

                bind BSpace last-window

                bind -n C-y run -b "exec </dev/null; ${pkgs.xclip}/bin/xclip -o -selection clipboard | tmux load-buffer - ; tmux paste-buffer"
                bind s split-window -v "tmux list-sessions | sed -E 's/:.*$//' | grep -v \"^$(tmux display-message -p '#S')\$\" | fzf --reverse | xargs tmux switch-client -t"

                bind -T copy-mode M-e run-shell "${pkgs.shell-capture}/bin/shell-capture es"
                bind -T copy-mode M-j run-shell "${pkgs.shell-capture}/bin/shell-capture js"
                bind -T copy-mode M-n run-shell "${pkgs.shell-capture}/bin/shell-capture ns"
                bind -T copy-mode M-x run-shell "${pkgs.shell-capture}/bin/shell-capture xs"

                bind r source-file ~/.tmux.conf \; display "  Config reloaded..."
                bind y set-window-option synchronize-panes

                # plugins settings

                # add all the plugins
                ${lib.concatStrings (map (x: "run-shell ${x.rtp}\n") tmuxPluginsBundle)}
            '';
            ".config/alacritty/alacritty.yml".source = ../dotfiles/shell/alacritty.yml;
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
                                    , Run CoreTemp ["-t","<core0>/<core1>°C",
                                                    "-L","40","-H","60",
                                                    "-l","lightblue",
                                                    "-n","gray90","-h","red"] 50
                                    , Run Com "${pkgs.wifi-status}/bin/wifi-status" [] "wifi" 60
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["openvpn-jobvpn.service", "[V]"] "vpn" 30
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["sshuttle.service", "[S]"] "sshuttle" 30
                                    , Run Kbd [ ("us", "<fc=#ee9a00>us</fc>")
                                              , ("ru", "<fc=green>ru</fc>")
                                              ]
                                    ]
                       , sepChar = "%"
                       , alignSep = "}{"
                       , template = "%StdinReader% }{| %battery% | %coretemp% | %wifi% %sshuttle% %vpn% | <fc=#ee9a00>%date%</fc> |%kbd%"
                       }
            '';
            ".config/tridactyl/tridactylrc".source = ../dotfiles/x11/tridactylrc;
            "tridactylrc".source = ../dotfiles/x11/tridactylrc;
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

                ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

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
                TMUXP_CONFIGDIR = "/etc/nixos/private/tmuxp";
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
                cat = "bat";
                cat_raw = "${pkgs.coreutils}/bin/cat";
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
                    askPass = ""
                [credential]
                    helper = ${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper
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
                # epkgs.lsp-dockerfile
                # epkgs.lsp-javascript
                # epkgs.lsp-sh
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
                epkgs.company-lsp
                epkgs.company-lua
                epkgs.company-nixos-options
                epkgs.company-quickhelp
                epkgs.company-restclient
                epkgs.company-shell
                epkgs.company-statistics
                epkgs.company-web
                epkgs.copy-as-format
                epkgs.counsel
                epkgs.counsel-notmuch
                epkgs.counsel-org-clock
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
                epkgs.format-all
                epkgs.function-args
                epkgs.git-msg-prefix
                epkgs.git-timemachine
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
                epkgs.ivy-yasnippet
                epkgs.jedi-core
                epkgs.jinja2-mode
                epkgs.jsonrpc
                epkgs.kaolin-themes
                epkgs.keychain-environment
                epkgs.labburn-theme
                epkgs.link-hint
                epkgs.lsp-go
                epkgs.lsp-haskell
                epkgs.lsp-java
                epkgs.lsp-mode
                epkgs.lsp-python
                epkgs.lsp-ui
                epkgs.lua-mode
                epkgs.magit
                epkgs.magit-filenotify
                epkgs.magithub
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
                epkgs.org-capture-pop-frame
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
                epkgs.pyvenv
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
                epkgs.super-save
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
                epkgs.vdiff
                epkgs.vdiff-magit
                epkgs.vimrc-mode
                epkgs.w3m
                epkgs.web-completion-data
                epkgs.web-mode
                epkgs.web-mode-edit-element
                epkgs.web-narrow-mode
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
                epkgs.ivy-historian
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
