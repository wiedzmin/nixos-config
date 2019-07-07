{config, pkgs, lib, ...}:
with import ../../../../../pkgs/util.nix {inherit lib config pkgs;};
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
            ms-pyls
            plantuml
            ripgrep
        ];
        programs.emacs = {
            enable = true;
            package = (pkgs.emacs26.override { # build Lucid version
                withGTK2 = false;
                withGTK3 = false;
            });
            extraPackages = epkgs: [
                epkgs.ace-link
                epkgs.ace-window
                epkgs.amx
                epkgs.anaphora
                epkgs.atomic-chrome
                epkgs.auto-compile
                epkgs.avy
                epkgs.avy-zap
                epkgs.backup-each-save
                epkgs.beginend
                epkgs.blockdiag-mode
                epkgs.browse-at-remote
                epkgs.comment-dwim-2
                epkgs.company
                epkgs.company-flx
                epkgs.company-lsp
                epkgs.company-nixos-options
                epkgs.company-quickhelp
                epkgs.company-restclient
                epkgs.company-statistics
                epkgs.copy-as-format
                epkgs.counsel
                epkgs.counsel-org-clock
                epkgs.counsel-projectile
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
                epkgs.doom-modeline
                epkgs.easy-kill
                epkgs.easy-kill-extras    # add to .el
                epkgs.editorconfig
                epkgs.f
                epkgs.fic-mode
                epkgs.flycheck
                epkgs.format-all
                epkgs.gcmh
                epkgs.git-timemachine
                epkgs.hc-zenburn-theme
                epkgs.helpful
                epkgs.highlight-numbers
                epkgs.hl-todo
                epkgs.imenu-anywhere
                epkgs.ini-mode
                epkgs.iqa
                epkgs.ivy
                epkgs.ivy-historian
                epkgs.ivy-pass
                epkgs.ivy-rich
                epkgs.ivy-xref
                epkgs.ivy-yasnippet
                epkgs.jinja2-mode
                epkgs.keychain-environment
                epkgs.link-hint
                epkgs.lsp-mode
                epkgs.lsp-ui
                epkgs.magit
                epkgs.magit-filenotify
                epkgs.magit-popup # *
                epkgs.markdown-mode
                epkgs.multi-compile
                epkgs.multiple-cursors
                epkgs.mwim
                epkgs.nix-mode
                epkgs.no-littering
                epkgs.ob-async
                epkgs.ob-blockdiag
                epkgs.ob-restclient
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
                epkgs.orgit
                epkgs.orglink
                epkgs.pass
                epkgs.phi-search
                epkgs.pinentry
                epkgs.pip-requirements
                epkgs.plantuml-mode
                epkgs.posframe
                epkgs.projectile
                epkgs.py-yapf
                epkgs.pyvenv
                epkgs.quelpa
                epkgs.quelpa-use-package
                epkgs.rainbow-mode
                epkgs.recentf-ext
                epkgs.recursive-narrow
                epkgs.region-bindings-mode
                epkgs.restart-emacs
                epkgs.rg
                epkgs.russian-holidays
                epkgs.savekill
                epkgs.shift-number
                epkgs.smartparens
                epkgs.super-save
                epkgs.swiper
                epkgs.undo-propose
                epkgs.unicode-fonts
                epkgs.webpaste
                epkgs.wgrep
                epkgs.which-key
                epkgs.which-key-posframe
                epkgs.ws-butler
                epkgs.yasnippet
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
                    { src = ./lang/python.el; })) }
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./clients.el; })) }
                ;; src = ./orgmode.el here
                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./help.el; })) }

                ${builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs; }) //
                    { src = ./staging.el; })) }

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
