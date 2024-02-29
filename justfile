default:
    @just --choose

# Update flake inputs versions
flake-update-inputs:
    nix flake update

# Build "laptoptop" flake
laptoptop-build:
    nixos-rebuild build --flake ".#laptoptop"

# Build "laptoptop" flake with debug output
laptoptop-build-debug:
    nixos-rebuild build --flake ".#laptoptop" --show-trace

# Build "laptoptop" flake without network access
laptoptop-build-no-net:
    nixos-rebuild build --flake ".#laptoptop" --option binary-caches ''

# Build + switch "laptoptop" flake
laptoptop-switch:
    nixos-rebuild switch -j 4 --use-remote-sudo --flake ".#laptoptop"

# Build + switch "laptoptop" flake without network access
laptoptop-switch-no-net:
    sudo nixos-rebuild switch --flake ".#laptoptop" --option binary-caches ''

# Build "momcat" flake
momcat-build:
    nixos-rebuild build --flake ".#momcat"

# Build "momcat" flake with debug output
momcat-build-debug:
    nixos-rebuild build --flake ".#momcat" --show-trace

# Build + switch "momcat" flake
momcat-switch:
    nixos-rebuild switch --use-remote-sudo --flake ".#momcat"

# Build + switch "momcat" flake without network access
momcat-switch-no-net:
    sudo nixos-rebuild switch --flake ".#momcat" --option binary-caches ''

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

# generate git log for `code-maat` consumption
maat-log:
    git log --all --numstat --date=short --pretty=format:'--%h--%ad--%aN' --no-renames > maat.log

check-flake:
    flake-checker -n unstable
