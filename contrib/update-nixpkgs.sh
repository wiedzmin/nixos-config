#!/usr/bin/env zsh
set -euo pipefail
set -x

branch=nixos-unstable

cd /etc/nixos/nixpkgs-channels
git checkout $branch

git fetch origin
git rebase origin/$branch
head_ts=`git show -s --format=%ct HEAD`
git tag -a -s v_$head_ts -m "checkpoint at $head_ts"
