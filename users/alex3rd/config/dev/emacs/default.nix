{config, pkgs, lib, ...}:
with import ../../../../../util.nix {inherit lib config pkgs;};
with import ../../../const.nix {inherit config pkgs;};
{
    home-manager.users."${userName}" = {
        home.packages = with pkgs; [
            # org-protocol.desktop
            (makeDesktopItem {
                name = "org-protocol";
                exec = "${emacs}/bin/emacsclient %U";
                comment = "";
                desktopName = "Custom org-protocol handler";
                categories = "System";
                mimeType = stdenv.lib.concatStringsSep ";" [
                    "x-scheme-handler/org-protocol"
                ];
            })

            nodePackages.bash-language-server
            ccls
            flow
            ms-pyls
            nodePackages.indium
            nodePackages.javascript-typescript-langserver
            ntangle
            plantuml
            ripgrep
            irony-server # TODO: play with it
        ];
        programs.emacs = {
            enable = true;
            package = (pkgs.emacs26.override { # build Lucid version
                withGTK2 = false;
                withGTK3 = false;
            });
            extraPackages = epkgs: [
                # epkgs.kibit-helper # TODO: setup kibit first
                # epkgs.lsp-dockerfile // https://github.com/emacs-lsp/lsp-dockerfile
                (addBuildInputs epkgs.magit-imerge [ pkgs.git ])
                epkgs.ace-link
                epkgs.ace-window
                epkgs.aggressive-indent
                epkgs.amx
                epkgs.anaphora
                epkgs.atomic-chrome
                epkgs.auto-compile
                epkgs.avy
                epkgs.avy-flycheck
                epkgs.avy-zap
                epkgs.backup-each-save
                epkgs.backup-walker
                epkgs.banner-comment
                epkgs.beginend
                epkgs.bln-mode
                epkgs.blockdiag-mode
                epkgs.browse-at-remote
                epkgs.bug-hunter
                epkgs.c-eldoc
                epkgs.carbon-now-sh
                epkgs.ccls
                epkgs.cider
                epkgs.clojure-mode
                epkgs.comment-dwim-2
                epkgs.common-lisp-snippets
                epkgs.company
                epkgs.company-c-headers
                epkgs.company-flx
                epkgs.company-go
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
                epkgs.css-eldoc
                epkgs.csv-mode
                epkgs.darkburn-theme
                epkgs.delight
                epkgs.diff-hl
                epkgs.dired-filetype-face
                epkgs.dired-git-info
                epkgs.dired-hide-dotfiles
                epkgs.dired-launch
                epkgs.dired-narrow
                epkgs.dired-quick-sort
                epkgs.diredfl
                epkgs.docker-compose-mode
                epkgs.docker-tramp
                epkgs.dockerfile-mode
                epkgs.doom-modeline
                epkgs.easy-kill
                epkgs.easy-kill-extras
                epkgs.edebug-x
                epkgs.edit-server
                epkgs.editorconfig
                epkgs.eldoc-eval
                epkgs.elisp-refs
                epkgs.elisp-slime-nav
                epkgs.emamux
                epkgs.emmet-mode
                epkgs.f
                epkgs.fic-mode
                epkgs.flycheck
                epkgs.flycheck-clang-analyzer
                epkgs.flycheck-flow
                epkgs.flycheck-gometalinter
                epkgs.flycheck-inline
                epkgs.flycheck-pos-tip
                epkgs.format-all
                epkgs.free-keys
                epkgs.function-args
                epkgs.general
                epkgs.git-msg-prefix
                epkgs.git-timemachine
                epkgs.gitconfig-mode
                epkgs.gitignore-mode
                epkgs.gitignore-mode
                epkgs.go-eldoc
                epkgs.go-guru
                epkgs.go-mode
                epkgs.go-playground
                epkgs.go-tag
                epkgs.godoctor
                epkgs.golden-ratio
                epkgs.gorepl-mode
                epkgs.gotest
                epkgs.gotham-theme
                epkgs.graphql-mode
                epkgs.haskell-mode
                epkgs.hc-zenburn-theme
                epkgs.help-find-org-mode
                epkgs.helpful
                epkgs.highlight-escape-sequences
                epkgs.highlight-indent-guides
                epkgs.highlight-numbers
                epkgs.hl-todo
                epkgs.httprepl
                epkgs.hungry-delete
                epkgs.ialign
                epkgs.ibuffer-project
                epkgs.ibuffer-vc
                epkgs.imenu-anywhere
                epkgs.imgbb
                epkgs.indium
                epkgs.info-buffer
                epkgs.info-colors
                epkgs.ini-mode
                epkgs.iqa
                epkgs.ivy
                epkgs.ivy-dired-history
                epkgs.ivy-historian
                epkgs.ivy-pass
                epkgs.ivy-rich
                epkgs.ivy-xref
                epkgs.ivy-yasnippet
                epkgs.jinja2-mode
                epkgs.kaolin-themes
                epkgs.keychain-environment
                epkgs.link-hint
                epkgs.loop
                epkgs.lsp-haskell
                epkgs.lsp-java
                epkgs.lsp-mode
                epkgs.lsp-ui
                epkgs.lua-mode
                epkgs.magit
                epkgs.magit-filenotify
                epkgs.magit-popup # *
                epkgs.magit-todos
                epkgs.magithub
                epkgs.markdown-mode
                epkgs.melpaStablePackages.slime
                epkgs.multi-compile
                epkgs.multiple-cursors
                epkgs.mwim
                epkgs.names
                epkgs.nginx-mode
                epkgs.nix-mode
                epkgs.no-littering
                epkgs.nord-theme
                epkgs.notmuch
                epkgs.ob-async
                epkgs.ob-blockdiag
                epkgs.ob-restclient
                epkgs.on-screen
                epkgs.org-alert
                epkgs.org-bullets
                epkgs.org-capture-pop-frame
                epkgs.org-clock-today
                epkgs.org-dashboard
                epkgs.org-download
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
                epkgs.parent-mode
                epkgs.pass
                epkgs.pcre2el
                epkgs.pdf-tools
                epkgs.persistent-scratch
                epkgs.phi-search
                epkgs.pinentry
                epkgs.pip-requirements
                epkgs.plantuml-mode
                epkgs.po-mode
                epkgs.polymode
                epkgs.popwin
                epkgs.posframe
                epkgs.projectile
                epkgs.py-yapf
                epkgs.pyvenv
                epkgs.quelpa
                epkgs.quelpa-use-package
                epkgs.rainbow-mode
                epkgs.rebox2
                epkgs.recentf-ext
                epkgs.recursive-narrow
                epkgs.regex-tool
                epkgs.region-bindings-mode
                epkgs.restart-emacs
                epkgs.restclient-test
                epkgs.rg
                epkgs.russian-holidays
                epkgs.savekill
                epkgs.shift-number
                epkgs.slime-company
                epkgs.smart-comment
                epkgs.smartparens
                epkgs.smooth-scrolling
                epkgs.solarized-theme
                epkgs.spaceline
                epkgs.super-save
                epkgs.swiper
                epkgs.twittering-mode
                epkgs.undo-propose
                epkgs.unfill
                epkgs.unicode-fonts
                epkgs.vagrant-tramp
                epkgs.vdiff
                epkgs.vdiff-magit
                epkgs.vimrc-mode
                epkgs.vlf
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
                epkgs.xr
                epkgs.yaml-mode
                epkgs.yasnippet
                epkgs.yatemplate
            ];
        };
        home.file = {
            "workspace/.editorconfig".text = ''
                # top-most EditorConfig file
                root = true

                # Unix-style newlines with a newline ending every file
                [*]
                end_of_line = lf
                insert_final_newline = true
                indent_style = space
                charset = utf-8
                trim_trailing_whitespace = true

                # Matches multiple files with brace expansion notation
                # Set default charset
                [*.{js,py,go}]
                charset = utf-8

                # 4 space indentation
                [*.py]
                indent_style = space
                indent_size = 4

                # Tab indentation (no size specified)
                [Makefile]
                indent_style = tab

                # Indentation override for all JS under lib directory
                [lib/**.js]
                indent_style = space
                indent_size = 2

                # Matches the exact files either package.json or .travis.yml
                [{package.json,.travis.yml}]
                indent_style = space
                indent_size = 2

                [*.{json,yml}]
                indent_style = space
                indent_size = 2

                [*.md]
                trim_trailing_whitespace = false
            '';
            ".emacs.d/.gitignore".text = ''
                */#*#
                *~
                .python-environments
                config.el
                customizations.el
                data
                dired-history
                elpa
                quelpa
                todo.org
            '';
            ".emacs.d/init.el".text = ''
                ;; -*- lexical-binding: t -*-

                (setq debug-on-error t)
                (setq debug-on-quit t)

                (setq load-prefer-newer t)
                (setq message-log-max t) ;; we don't want to lose any startup log info
                (setq shell-file-name "${pkgs.bash}/bin/bash")

                (setq gc-cons-percentage 0.3)

                (setq gc-cons-threshold most-positive-fixnum)

                (add-hook 'after-init-hook #'(lambda ()
                                               (setq gc-cons-threshold 800000)))

                (add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold most-positive-fixnum)))
                (add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold 800000)))

                (add-hook 'focus-out-hook #'garbage-collect)

                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./bootstrap.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./base.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./security.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./appearance.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./context.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./navigation.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./editing.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./majormodes.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./programming.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./clients.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./orgmode.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./help.el; })) }

                (setq debug-on-error nil)
                (setq debug-on-quit nil)
            '';
            ".emacs.d/resources/yasnippet" = {
                source = ./yasnippet-snippets;
                recursive = true;
            };
            ".emacs.d/secrets/email.el.gpg".source = ../../../private/emacs-secrets/email.el.gpg;
            ".emacs.d/secrets/ibuffer.el.gpg".source = ../../../private/emacs-secrets/ibuffer.el.gpg;
            ".emacs.d/secrets/identity.el.gpg".source = ../../../private/emacs-secrets/identity.el.gpg;
            ".emacs.d/secrets/media.el.gpg".source = ../../../private/emacs-secrets/media.el.gpg;
            ".emacs.d/secrets/vcs.el.gpg".source = ../../../private/emacs-secrets/vcs.el.gpg;
        };
    };
}

# * it seems some magit-dependent packages yet depend on magit-popup in some path, so we introduced
#   this explicit dependency and will keep it until transition to "transient" library is fully done
#   by all affected packages. (or some other root cause of "magit-popup"" will pop up)
