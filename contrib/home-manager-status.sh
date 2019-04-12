#!/usr/bin/env zsh

ACTION=$1
if [ ! -z "$ACTION" ]; then
    cd /etc/nixos/pkgs/home-manager

    branch=master
    branch_hash=$(git rev-parse --short $branch)
    current_hm_hash=$(cat /etc/current-home-manager)

    case "$ACTION" in
        update)
            git tag -a -s --force last_working -m "last home-manager built and working" $current_hm_hash
            git fetch origin
            git rebase origin/$branch
            branch_hash=$(git rev-parse --short $branch)
            if [ "$branch_hash" != "$current_hm_hash" ]; then
                head_ts=$(git show -s --format=%ct HEAD)
                git tag -a -s --force "v_$head_ts" -m "checkpoint at $(LC_ALL=C date -d @$head_ts)"
            fi
            exit 0
            ;;
        review)
            if [ "$branch_hash" = "$current_hm_hash" ]; then
                echo "No fresh updates, try again a bit later"
                exit 0
            fi
            echo "showing commits $branch_hash...$current_hm_hash"
            git log --pretty=oneline $branch_hash...$current_hm_hash | grep -v Merge | fzf --reverse | xargs git show
            exit 0
            ;;
        tags)
            git tag | fzf --reverse | xargs git show
            exit 0
            ;;
        *)
            echo "Unknown action: $ACTION"
            exit 1
            ;;
    esac
    git checkout $branch
    exit 0
fi
exit 1
