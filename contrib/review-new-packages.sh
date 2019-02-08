#!/usr/bin/env zsh

branch=nixos-unstable
fallback_branch=nixos-unstable-working
new_package_token=init

cd /etc/nixos/pkgs/nixpkgs-channels
git checkout $branch

current_system_commit_hash=`readlink -f /run/current-system | cut -f4 -d.`
git log --pretty=oneline $branch...$current_system_commit_hash | grep $new_package_token | grep -v Merge
