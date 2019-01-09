#!/usr/bin/env zsh
set -euo pipefail
set -x

branch=nixos-unstable
fallback_branch=nixos-unstable-working

cd /etc/nixos/pkgs/nixpkgs-channels
git checkout $branch

current_system_commit_hash=`readlink -f /run/current-system | cut -f4 -d.`
git branch -f $fallback_branch $current_system_commit_hash
git tag -a -s --force last_working -m "last nixpkgs built and working" $current_system_commit_hash

git fetch origin
git rebase origin/$branch
head_ts=`git show -s --format=%ct HEAD`
git tag -a -s v_$head_ts -m "checkpoint at $head_ts"
git checkout $branch
