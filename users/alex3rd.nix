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
    programs.zsh.enable = true;

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
            ".config/pep8".source = ../dotfiles/dev/python/pep8;
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
        };
        programs.zsh = {
            enable = true;
            oh-my-zsh = {
                enable = true;
                plugins = [
                    "colored-man-pages"
                    "dirpersist"
                    "emacs"
                    "urltools"
                    "virtualenv"
                    "virtualenvwrapper"
                ];
                theme = "agnoster";
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
                #PATH=$PATH:${pkgs.autojump}/bin
                #. ${pkgs.autojump}/share/autojump/autojump.zsh

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

                zle -N jump && bindkey "^xjj" jump
                zle -N dot && bindkey . dot
                zle -N fbr && bindkey "^]c" fbr
                zle -N fco && bindkey "^]t" fco
                zle -N fcoc && bindkey "^]l" fcoc
                zle -N fe && bindkey "^qe" fe
                zle -N fshow && bindkey "^]s" fshow
                zle -N fzf-history-widget && bindkey "^R" fzf-history-widget
                zle -N predict-off
                zle -N predict-on

                #eval "$(direnv hook zsh)"
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
            extraConfig = { # TODO: resurrect some incompatible config entries using literal config
                color.diff = "auto";
                color.status = "auto";
                color.ui = "always";
                commit.gpgSign = true;
                commit.template = "${config.users.extraUsers.alex3rd.home}/git-assets/git-commit-template";
                core.autocrlf = false;
                core.excludesfile = "${config.users.extraUsers.alex3rd.home}/git-assets/.gitignore";
                core.quotepath = false;
                credential.helper = "cache --timeout=3600";
                diff.algorithm = "patience";
                gpg.program = "${pkgs.gnupg}/bin/gpg2";
                init.templatedir = "${config.users.extraUsers.alex3rd.home}/git-assets/templates";
                pager.diff = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX";
                pager.show = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX";
                push.default = "current";
            };
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
                epkgs.ace-window
                epkgs.actionscript-mode
                epkgs.aggressive-indent
                epkgs.anaphora
                epkgs.archive-rpm
                epkgs.atomic-chrome
                epkgs.auto-compile
                epkgs.auto-yasnippet
                epkgs.avy
                epkgs.avy-flycheck
                epkgs.backup-each-save
                epkgs.backup-walker
                epkgs.banner-comment
                epkgs.beginend
                epkgs.blockdiag-mode
                epkgs.bug-hunter
                epkgs.c-eldoc
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
                epkgs.counsel-gtags
                epkgs.counsel-gtags
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
                epkgs.flycheck-pos-tip
                epkgs.flycheck-pycheckers
                epkgs.function-args
                epkgs.git-msg-prefix
                epkgs.gitignore-mode
                epkgs.go-eldoc
                epkgs.go-guru
                epkgs.go-mode
                epkgs.go-playground
                epkgs.go-tag
                epkgs.golden-ratio
                epkgs.google-translate
                epkgs.gorepl-mode
                epkgs.gotest
                epkgs.govet
                epkgs.graphql-mode
                epkgs.hc-zenburn-theme
                epkgs.highlight-stages
                epkgs.httprepl
                epkgs.hungry-delete
                epkgs.hydra
                epkgs.ialign
                epkgs.ibuffer-vc
                epkgs.imenu-anywhere
                epkgs.importmagic
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
                epkgs.keyfreq
                epkgs.labburn-theme
                epkgs.link-hint
                epkgs.lua-mode
                epkgs.macro-math
                epkgs.magit
                epkgs.magit-filenotify
                epkgs.magithub
                epkgs.markdown-mode
                epkgs.mc-extras
                epkgs.mingus
                epkgs.multi-compile
                epkgs.multiple-cursors
                epkgs.mwim
                epkgs.names
                epkgs.nginx-mode
                epkgs.nix-mode
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
                epkgs.page-break-lines
                epkgs.paradox
                epkgs.pass
                epkgs.pcre2el
                epkgs.persistent-scratch
                epkgs.phi-search
                epkgs.phi-search-mc
                epkgs.pip-requirements
                epkgs.plantuml-mode
                epkgs.po-mode
                epkgs.popwin
                epkgs.prog-fill
                epkgs.projectile
                epkgs.py-autopep8
                epkgs.py-isort
                epkgs.python-environment
                epkgs.python-mode
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
                epkgs.rpm-spec-mode
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
                epkgs.speed-type
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
                epkgs.vdiff-magit
                epkgs.vimrc-mode
                epkgs.virtualenvwrapper
                epkgs.volatile-highlights
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
        services.dunst.enable = true;
    };
}
