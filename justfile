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
    sudo nixos-rebuild switch --flake ".#laptoptop"

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
    sudo nixos-rebuild switch --flake ".#momcat"

# Build + switch "momcat" flake without network access
momcat-switch-no-net:
    sudo nixos-rebuild switch --flake ".#momcat" --option binary-caches ''

# Remove system build local output
clean:
    unlink ./result

# Rollback to previous working configuration(s)
rollback:
    rollback

# Reinstall pre-commit hooks
fix-pre-commit:
    pre-commit install
    pre-commit install --hook-type prepare-commit-msg
    pre-commit install --hook-type pre-push

