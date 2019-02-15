#!/usr/bin/env bash

nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=/etc/nixos/contrib/iso/iso.nix
