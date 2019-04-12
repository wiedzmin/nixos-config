{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
let
    zshOptions = [
        "braceccl"
        "correctall"
        "extendedglob"
        "menucomplete"
    ];
    binDirs = [
        "/home/${userName}/scripts"
        "/home/${userName}/tools/bin"
        "/home/${userName}/.local/bin"
    ];
    zshHistFilename = ".histfile";
in
{
    home-manager.users.alex3rd = {
        home.file = {
            ".zsh/functions.zsh".text = ''
                dot() {
                    if [[ $LBUFFER = *.. ]]; then
                        LBUFFER+=/..
                    else
                        LBUFFER+=.
                    fi
                }
            '';
        };
        programs.bat = {
            enable = true;
            config = {
                theme = "TwoDark";
                pager = "less -FR";
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
                path = "${zshHistFilename}";
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
                eval "$(${pkgs.fasd}/bin/fasd --init auto)"

                ${lib.concatMapStrings (opt: "setopt ${opt}\n") zshOptions}

                source ~/.zsh/functions.zsh

                zle -N dot && bindkey . dot
                bindkey '^P' fuzzy-search-and-edit
            '';
            sessionVariables = {
                CURRENT_WM = "${config.services.xserver.windowManager.default}";
                EDITOR = "${pkgs.emacs}/bin/emacsclient";
                FZF_MARKS_FILE = "/home/${userName}/.bookmarks";
                FZF_MARKS_JUMP = "^[xjj";
                ZSH_COMMAND_TIME_COLOR = "cyan";
                GREP_COLOR = "1;32";
                GREP_OPTIONS = "--color=auto";
                GTAGSLIBPATH = "/home/${userName}/.gtags/";
                HISTFILE = "${zshHistFilename}";
                PATH = "$PATH:${lib.concatStringsSep ":" (lib.unique binDirs)}";
                SHELL = "${pkgs.zsh}/bin/zsh";
                TMUXP_CONFIGDIR = "/home/${userName}/tmuxp";
                VISUAL = "${pkgs.emacs}/bin/emacsclient";
                WORKON_HOME = "/home/${userName}/.virtualenvs";
                XAUTHORITY = "/home/${userName}/.Xauthority";
            };
            shellAliases = {
                cat = "${pkgs.bat}/bin/bat";
                cato = "${pkgs.coreutils}/bin/cat";

                df = "${pkgs.dfc}/bin/dfc";
                dud = "(setopt globdots; ${pkgs.coreutils}/bin/du -mhs * | ${pkgs.coreutils}/bin/sort -hr)";
                find = "${pkgs.fd}/bin/fd";
                findo = "${pkgs.findutils}/bin/find";

                git = "${pkgs.gitAndTools.hub}/bin/hub";
                gop = "git open";

                gpg = "${pkgs.gnupg}/bin/gpg2";

                li = "${pkgs.lsd}/bin/lsd -ial --group-dirs first";
                ls = "${pkgs.lsd}/bin/lsd -F --color=auto --group-dirs first";
                lsa = "${pkgs.lsd}/bin/lsd -ld .* --group-dirs first";
                lsd = "${pkgs.lsd}/bin/lsd -ld *(-/DN) --group-dirs first";

                untar = "tar xvvf";

                zr = ". ~/.zshrc";

                DI = "${pkgs.docker}/bin/docker inspect";
                DL = "${pkgs.docker}/bin/docker logs";
                DP = "${pkgs.docker}/bin/docker ps";
                DPA = "${pkgs.docker}/bin/docker ps -a";
            };
            plugins = [
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
                    name = "zsh-notify";
                    file = "notify.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "marzocchi";
                        repo = "zsh-notify";
                        rev = "853bc9434771b99b028f069b95e13ecdf06901d0";
                        sha256 = "0bhmv1xfjzmci9b4dy3mix2s31zj0kayrl44xx5xb8rgzlf0qbvr";
                    };
                }
                {
                    # TODO: try to integrate with fzf-based utils, expecially commit browser
                    name = "browse-commit";
                    file = "browse-commit.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "wiedzmin";
                        repo = "browse-commit";
                        rev = "cf28b2eeba622545ae751ec99532b6b60e58b845";
                        sha256 = "15c9qxxa7l47w5r28pazs0gv0016lv52mncn45s6g1d3120k5fx0";
                    };
                }
                {
                    name = "pass-zsh-completion";
                    file = "pass-zsh-completion.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "ninrod";
                        repo = "pass-zsh-completion";
                        rev = "e4d8d2c27d8999307e8f34bf81b2e15df4b76177";
                        sha256 = "1z83hgdljl7yqd1lqb10an8zkrv7s01khky27mgc1wargkslkxi9";
                    };
                }
                {
                    name = "zsh-async";
                    file = "async.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "mafredri";
                        repo = "zsh-async";
                        rev = "e6d937228729f934f2033039bb54c3a18f5f1358";
                        sha256 = "0f0bqm4245ghx31x30ircfp4njji834495g25wvrd93k2r96a669";
                    };
                }
                {
                    name = "git-extra-commands";
                    file = "git-extra-commands.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "unixorn";
                        repo = "git-extra-commands";
                        rev = "f03ff8ffce9f3e488b6a0265cb09288cc29899fe";
                        sha256 = "1qlbjn0q87jgjir3k7w4m8p6wqgjl2c7jnilczf7c205fgwksdhi";
                    };
                }
                {
                    name = "zsh-reentry-hook";
                    file = "zsh-reentry-hook.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "RobSis";
                        repo = "zsh-reentry-hook";
                        rev = "8587186df8f08b8a57ae7f87ab0bc7d503909031";
                        sha256 = "1jgin1gmw05vxf7vw414zvhq9dg06yzlzxas723f710vs58mf11a";
                    };
                }
                {
                    name = "zsh-fuzzy-search-and-edit";
                    file = "plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "seletskiy";
                        repo = "zsh-fuzzy-search-and-edit";
                        rev = "4fbb3d351b75f1007df0d5cb09292bb2321f903a";
                        sha256 = "1shhmda1iqwz79y2ianmjs5623zabckxfj2hqw4gl2axpkwnj1ib";
                    };
                }
                {
                    name = "zsh-command-time";
                    file = "command-time.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "popstas";
                        repo = "zsh-command-time";
                        rev = "afb4a4c9ae7ce64ca9d4f334a79a25e46daad0aa";
                        sha256 = "1bvyjgz6bhgg1nwr56r50p6fblgah6yiql55pgm5abnn2h876fjq";
                    };
                }
                { # NOTE: should be last in the list
                    name = "zsh-syntax-highlighting";
                    file = "zsh-syntax-highlighting.plugin.zsh";
                    src = pkgs.fetchFromGitHub {
                        owner = "zsh-users";
                        repo = "zsh-syntax-highlighting";
                        rev = "e900ad8bad53501689afcb050456400d7a8466e5";
                        sha256 = "1dfy5wvkmnp2zzk81fhc7qlywgn0j6z0vjch5ak5r3j2kqv61cmi";
                    };
                }
            ];
        };
    };
}
