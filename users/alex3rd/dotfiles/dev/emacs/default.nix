{config, pkgs, lib, ...}:
with import ../../../../../toolbox/util.nix {inherit lib config pkgs;};
{
    home-manager.users.alex3rd = {
        home.packages = with pkgs; [
            org-protocol.desktop

            nodePackages.indium
            ripgrep
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
                epkgs.actionscript-mode
                epkgs.aggressive-indent
                epkgs.amx
                epkgs.anaphora
                epkgs.ansible
                epkgs.ansible-doc
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
                epkgs.company-ansible
                epkgs.company-c-headers
                epkgs.company-flx
                epkgs.company-go
                epkgs.company-lsp
                epkgs.company-lua
                epkgs.company-nixos-options
                epkgs.company-quickhelp
                epkgs.company-racer
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
                epkgs.deadgrep
                epkgs.delight
                epkgs.diff-hl
                epkgs.dired-filetype-face
                epkgs.dired-hide-dotfiles
                epkgs.dired-narrow
                epkgs.dired-quick-sort
                epkgs.diredfl
                epkgs.docker-compose-mode
                epkgs.docker-tramp
                epkgs.dockerfile-mode
                epkgs.doom-modeline
                epkgs.dtrt-indent
                epkgs.dynamic-ruler
                epkgs.easy-kill
                epkgs.easy-kill-extras
                epkgs.edebug-x
                epkgs.edit-server
                epkgs.editorconfig
                epkgs.el-get
                epkgs.eldoc-eval
                epkgs.elfeed
                epkgs.elfeed-goodies
                epkgs.elfeed-org
                epkgs.elisp-refs
                epkgs.elisp-slime-nav
                epkgs.emamux
                epkgs.emmet-mode
                epkgs.exec-path-from-shell
                epkgs.expand-region
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
                epkgs.gitconfig-mode
                epkgs.gitignore-mode
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
                epkgs.graphql-mode
                epkgs.haskell-mode
                epkgs.hc-zenburn-theme
                epkgs.help-find-org-mode
                epkgs.helpful
                epkgs.highlight-indent-guides
                epkgs.highlight-escape-sequences
                epkgs.highlight-numbers
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
                epkgs.jedi-core
                epkgs.jinja2-mode
                epkgs.jsonrpc
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
                epkgs.magithub
                epkgs.markdown-mode
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
                epkgs.ob-restclient
                epkgs.on-screen
                epkgs.org-alert
                epkgs.org-bullets
                epkgs.org-capture-pop-frame
                epkgs.org-clock-today
                epkgs.org-dashboard
                epkgs.org-download
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
                epkgs.parent-mode
                epkgs.pass
                epkgs.pcre2el
                epkgs.pdf-tools
                epkgs.persistent-scratch
                epkgs.phi-search
                epkgs.pip-requirements
                epkgs.plantuml-mode
                epkgs.po-mode
                epkgs.poly-ansible
                epkgs.polymode
                epkgs.popwin
                epkgs.posframe
                epkgs.prog-fill
                epkgs.projectile
                epkgs.py-yapf
                epkgs.pyvenv
                epkgs.quelpa
                epkgs.quelpa-use-package
                epkgs.racer
                epkgs.rainbow-mode
                epkgs.rebox2
                epkgs.recentf-ext
                epkgs.recursive-narrow
                epkgs.regex-tool
                epkgs.region-bindings-mode
                epkgs.restart-emacs
                epkgs.reverse-im
                epkgs.rg
                epkgs.russian-holidays
                epkgs.rust-mode
                epkgs.rustic
                epkgs.savekill
                epkgs.shrink-path
                epkgs.shut-up
                epkgs.skeletor
                epkgs.slime-company
                epkgs.smartparens
                epkgs.smart-comment
                epkgs.smooth-scrolling
                epkgs.solarized-theme
                epkgs.spaceline
                epkgs.super-save
                epkgs.swiper
                epkgs.tide
                epkgs.tile
                epkgs.transpose-frame
                epkgs.twittering-mode
                epkgs.undo-propose
                epkgs.unfill
                epkgs.unicode-fonts
                epkgs.use-package-el-get
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

                ${builtins.readFile ./bootstrap.el}
                ${builtins.readFile ./base.el}
                ${builtins.readFile ./security.el}
                ${builtins.readFile ./appearance.el}
                ${builtins.readFile ./context.el}
                ${builtins.readFile ./navigation.el}
                ${builtins.readFile ./editing.el}
                ${builtins.readFile ./majormodes.el}
                ${builtins.readFile ./programming.el}
                ${builtins.readFile ./clients.el}
                ${builtins.readFile ./orgmode.el}
                ${builtins.readFile ./help.el}

                (setq debug-on-error nil)
                (setq debug-on-quit nil)
            '';
            ".emacs.d/resources/yasnippet" = {
                source = ./yasnippet-snippets;
                recursive = true;
            };
            ".emacs.d/resources/ditaa0_9.jar".source = ../../../../../contrib/blobs/ditaa0_9.jar;
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
