host_name := `hostname`
nixpkgs_main_input := "unstable"

default:
    @just --choose

# Build configuration flake for current host
build host=host_name:
    nixos-rebuild build --flake ".#{{host}}"

# Build configuration flake for current host with debug output
build-debug host=host_name:
    nixos-rebuild build --flake ".#{{host}}" --show-trace

# Build configuration flake for current host without network access
build-no-net host=host_name:
    nixos-rebuild build --flake ".#{{host}}" --option binary-caches ''

# Build + switch configuration flake for current host
switch host=host_name:
    nixos-rebuild switch{{ if env_var_or_default("VERBOSE", "false") == "true" { " -v" } else { "" } }} -j 4 --use-remote-sudo --flake ".#{{host}}"

# Build + switch configuration flake for current host without network access
switch-no-net host=host_name:
    nixos-rebuild switch{{ if env_var_or_default("VERBOSE", "false") == "true" { " -v" } else { "" } }} --use-remote-sudo --flake ".#{{host}}" --option binary-caches ''

# Remove system build local output
clean:
    unlink ./result || exit 0

# Rollback to previous working configuration(s)
rollback:
    rollback

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

# Update flake inputs versions
flake-update-inputs: flake-update-inputs-nixpkgs-main flake-update-inputs-emacs
    nix flake update

# Update `unstable` flake input
flake-update-inputs-nixpkgs-main:
    nix flake update {{nixpkgs_main_input}}

# Update `unstable` flake input
flake-update-inputs-emacs:
    nix flake update emacs

# Lint configuration flake(s)
check-config-flake:
    flake-checker -n {{nixpkgs_main_input}}

# generate git log for `code-maat` consumption
maat-log:
    git log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames > maat.log
