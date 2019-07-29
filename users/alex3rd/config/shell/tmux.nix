{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
let
    shell-capture = pkgs.writeShellScriptBin "shell-capture" ''
        TEMPLATE="$1"
        if [[ ! -n $TEMPLATE ]]
        then
            exit 1
        fi
        TITLE="$*"
        if [[ -n $TMUX ]]
        then
            TITLE=$(${pkgs.tmux}/bin/tmux display-message -p '#S')
            ${pkgs.tmux}/bin/tmux send -X copy-pipe-and-cancel "${pkgs.xsel}/bin/xsel -i --primary"
        fi

        if [[ -n $TITLE ]]
        then
            ${pkgs.emacs}/bin/emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
        else
            ${pkgs.emacs}/bin/emacsclient -n "org-protocol://capture?template=$TEMPLATE"
        fi
    '';
in
{
    home-manager.users."${userName}" = {
        programs.tmux = {
            enable = true;
            baseIndex = 1;
            clock24 = true;
            escapeTime = 0;
            status = {
                currentWindowFormat = "#[bg=blue,fg=cyan,bold]#I#[bg=blue,fg=cyan]:#[fg=colour230]#T#[fg=dim]#F";
                windowFormat = "#[fg=cyan,dim]#I#[fg=blue]:#[default]#W#[fg=grey,dim]#F";
                leftFormat = "#{prefix_highlight}#[fg=green](#S) #(whoami)@#H";
                rightFormat = "#[fg=blue,bright]%k:%M:%S %d/%m/%Y | #{cpu_fg_color}#{cpu_icon}#{cpu_percentage}";
                style = "fg=white,bg=default,default";
                windowStyle = "fg=cyan,bg=default,dim";
                currentWindowStyle = "fg=colour166,bg=red,bright";
                messageStyle = "fg=white,bg=black,bright";
            };
            borderStyle = {
                active = "fg=colour240,bg=default";
                inactive = "fg=colour235,bg=default";
            };
            hooks = {
                "after-select-pane" = ''run-shell \"tmux set -g window-active-style "bg='brightblack'" && sleep .05 && tmux set -g window-active-style '''\"'';
            };
            bindings = {
                copyMode = {
                    "M-e" = ''run-shell "${shell-capture}/bin/shell-capture es"'';
                    "M-j" = ''run-shell "${shell-capture}/bin/shell-capture js"'';
                    "M-n" = ''run-shell "${shell-capture}/bin/shell-capture ns"'';
                    "M-x" = ''run-shell "${shell-capture}/bin/shell-capture xs"'';
                };
                root = {
                    "C-left" = "prev";
                    "C-right" = "next";
                    "S-left" = "swap-window -t -1";
                    "S-right" = "swap-window -t +1";
                    "C-y" = ''run -b "exec </dev/null; ${pkgs.xsel}/bin/xsel -o --clipboard | tmux load-buffer - ; \
                                    tmux paste-buffer"'';
                };
                prefixed = {
                    "*" = "list-clients";
                    "l" = "refresh-client";
                    "m" = "select-pane -m";
                    "|" = ''split-window -h -c "#{pane_current_path}"'';
                    "\\" = ''split-window -fh -c "#{pane_current_path}"'';
                    "-" = ''split-window -v -c "#{pane_current_path}"'';
                    "_" = ''split-window -fv -c "#{pane_current_path}"'';
                    "'#'" = ''split-window -h -c "#{pane_current_path}"'';
                    "@" = ''split-window -v -c "#{pane_current_path}"'';
                    "BSpace" = "last-window";
                    "r" = ''source-file ~/.tmux.conf \; display "  Config reloaded..."'';
                    "y" = "set-window-option synchronize-panes";
                    "T" = ''neww -n "Tmux manual" "exec man tmux"'';
                    "s" = ''split-window -v "tmux list-sessions | sed -E 's/:.*$//' | \
                                        grep -v \"^$(tmux display-message -p '#S')\$\" | \
                                        ${pkgs.skim}/bin/sk --reverse | xargs tmux switch-client -t"'';
                    "F12" = ''send-key "#############################################################################################"'';
                    "F11" = ''new-window "fq; $SHELL"'';
                    "S-F11" = ''run -b "exec </dev/null; ${pkgs.xsel}/bin/xsel -o --clipboard | \
                                                    ${pkgs.xe}/bin/xe ${pkgs.nq}/bin/nq ${pkgs.you-get}/bin/you-get"'';
                    "b" = ''split-window -c '#{pane_current_path}' \
                                    -v "${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null && ${pkgs.git}/bin/git -p show --color=always \
                                        $(${pkgs.git}/bin/git log --decorate=short --graph --oneline --color=always | \
                                        ${pkgs.skim}/bin/sk --ansi -m | ${pkgs.gawk}/bin/awk '{print $2}') | less -R"'';
                };
            };
            extraConfig = ''
                set -g renumber-windows on

                set -g bell-action any
                set -g visual-activity off
                set -g visual-bell off
                set -g visual-silence off
                setw -g monitor-activity on
            '';
            historyLimit = 102400;
            keyMode = "emacs";
            nestedShortcut = "C-x";
            sensibleOnTop = false;
            shortcut = "M-x";
            terminal = "screen-256color";
            secureSocket = false;
            shell = "${pkgs.zsh}/bin/zsh";
            tmuxp.enable = true;
            plugins = with pkgs; with tmuxPlugins; [
                {
                    plugin = fzf-tmux-url-with-history; # patched version, see overlays
                    extraConfig = "set -g @fzf-url-bind 'o'";
                }
                battery
                copycat
                cpu
                fpp
                logging
                prefix-highlight
                sessionist
                yank
            ];
        };
        home.file = {
            # TODO: make gc root of shell env below
            "tmuxp/housekeeping.yml".text = ''
                session_name: housekeeping
                windows:
                  - window_name: system
                    start_directory: /etc/nixos/pkgs/custom
                    panes:
                      - nix-shell
                  - window_name: repl
                    panes:
                      - shell_command:
                        - cd /etc/nixos/pkgs/nixpkgs-channels
                        - nix repl '<nixpkgs/nixos>'
            '';
            "tmuxp/media.yml".text = ''
                session_name: media
                windows:
                  - window_name: youtube
                    panes:
                      - mpsyt
            '';
            "tmuxp/dev.yml".text = ''
                session_name: dev
                windows:
                  - window_name: mc
                    start_directory: ${config.dev.workspacePath}/github.com/wiedzmin
                    panes:
                      - mc
            '';
        };
    };
}

# TODO: some impl/binding for ix.io posts
