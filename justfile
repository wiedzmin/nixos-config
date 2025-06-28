host_name := `hostname`
nixpkgs_main_input := "unstable"

default:
    @just --choose

# Build configuration flake for current host
build host=host_name:
    mkdir -p /tmp/buildroot/ && nixos-rebuild build --flake ".#{{host}}"

# Build configuration flake VM for current host
build-vm host=host_name:
    mkdir -p /tmp/buildroot/ && nixos-rebuild build-vm --flake ".#{{host}}"

# Build configuration flake for current host with debug output
build-debug host=host_name:
    mkdir -p /tmp/buildroot/ && nixos-rebuild build --flake ".#{{host}}" --show-trace

# Build configuration flake for current host without network access
build-no-net host=host_name:
    mkdir -p /tmp/buildroot/ && nixos-rebuild build --flake ".#{{host}}" --option binary-caches ''

# Build + switch configuration flake for current host
switch host=host_name:
    mkdir -p /tmp/buildroot/ && nixos-rebuild switch{{ if env_var_or_default("VERBOSE", "false") == "true" { " -v" } else { "" } }} -j 4 --sudo --flake ".#{{host}}"

# Build + switch configuration flake for current host without network access
switch-no-net host=host_name:
    mkdir -p /tmp/buildroot/ && nixos-rebuild switch{{ if env_var_or_default("VERBOSE", "false") == "true" { " -v" } else { "" } }} --sudo --flake ".#{{host}}" --option binary-caches ''

# Remove system build local output
clean:
    unlink ./result || exit 0

# Rollback to previous working configuration(s)
rollback:
    rollback

# Fix config, in case Emacs part is broken
fix-config:
    emacs -q .

# Collect garbage
collect-garbage:
    sudo nix-collect-garbage -d

# remove dead nix
remove-dead-nix:
    deadnix .

# cleanup current devenv
devenv-cleanup:
    rm -rf ${PWD}/.devenv ${PWD}/.direnv
    rm -f ${PWD}/devenv.lock ${PWD}/.devenv.flake.nix ${PWD}/.pre-commit-config.yaml
    touch .envrc

# cleanup and GC current devenv
devenv-cleanup-and-gc:
    rm -rf ${PWD}/.devenv ${PWD}/.direnv
    rm -f ${PWD}/devenv.lock ${PWD}/.devenv.flake.nix ${PWD}/.pre-commit-config.yaml
    sudo nix-collect-garbage -d
    touch .envrc

# Update flake inputs
flake-update-inputs: flake-update-inputs-nixpkgs-main \
    flake-update-inputs-nur \
    flake-update-inputs-home-manager \
    flake-update-inputs-nixos-aux \
    flake-update-inputs-devenv \
    flake-update-inputs-emacs \
    flake-update-inputs-emacs-unpackaged \
    flake-update-inputs-kitty-unpackaged \
    flake-update-inputs-shell-unpackaged

# Update flake inputs, sans `devenv-src`
flake-update-inputs-sans-devenv: flake-update-inputs-nixpkgs-main \
    flake-update-inputs-nur \
    flake-update-inputs-home-manager \
    flake-update-inputs-nixos-aux \
    flake-update-inputs-emacs \
    flake-update-inputs-emacs-unpackaged \
    flake-update-inputs-kitty-unpackaged \
    flake-update-inputs-shell-unpackaged

# Update `unstable` flake input
flake-update-inputs-nixpkgs-main:
    nix flake update {{nixpkgs_main_input}}

# Update `nur` flake input
flake-update-inputs-nur:
    nix flake update nur

# Update `home-manager` flake input
flake-update-inputs-home-manager:
    nix flake update home-manager

# Update nixos-related auxiliary inputs
flake-update-inputs-nixos-aux:
    nix flake update nixos-hardware
    nix flake update nixos-artwork

# Update `devenv-src` flake input
flake-update-inputs-devenv:
    nix flake update devenv-src

# Update `emacs` flake input
flake-update-inputs-emacs:
    nix flake update emacs

# Update `emacs` flake inputs for emacs unpackaged extensions
flake-update-inputs-emacs-unpackaged:
    nix flake update yasnippet-snippets
    nix flake update emacs-combobulate
    nix flake update emacs-consult-org-clock
    nix flake update emacs-epithet
    nix flake update emacs-git-msg-prefix
    nix flake update emacs-highlight-sexp
    nix flake update emacs-just-ts-mode
    nix flake update emacs-org-bars
    nix flake update emacs-password-menu
    nix flake update emacs-password-store-menu
    nix flake update emacs-project-headerline
    nix flake update emacs-treesit-jump
    nix flake update emacs-vue-ts-mode

# Update flake inputs for Kitty unpackaged extensions
flake-update-inputs-kitty-unpackaged:
    nix flake update kitty-grab
    nix flake update kitty-search

# Update flake inputs for unpackaged shell extensions
flake-update-inputs-shell-unpackaged:
    nix flake update git-extra-commands
    nix flake update pass-zsh-completion
    nix flake update zsh-async
    nix flake update zsh-fuzzy-search-and-edit
    nix flake update zsh-reentry-hook

# Lint configuration flake(s)
check-config-flake:
    flake-checker -n {{nixpkgs_main_input}}

# generate git log for `code-maat` consumption
maat-log:
    git log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames > maat.log

tagref:
    tagref --file-sigil fn
