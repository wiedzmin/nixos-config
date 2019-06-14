#!/usr/bin/env zsh

ACTION=$1
if [ ! -z "$ACTION" ]; then
    case "$ACTION" in
        update)
            branch=master
            branch_hash=$(git rev-parse --short $branch)
            current_hm_hash=$(cat /etc/current-home-manager)

            cd /etc/nixos/pkgs/home-manager
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
        update-proposed)
            upstream_remote_name=upstream
            branch=master

            cd /etc/nixos/pkgs/home-manager-proposed
            remote_fetch_meta=$(git remote -v | grep fetch)
            has_upstream=$(echo $remote_fetch_meta | grep $upstream_remote_name)
            if [ -z $has_upstream ]; then
                echo "No $upstream_remote_name remote found."
                echo "Use 'git remote add $upstream_remote_name <upstream url>' to add it."
                exit 1
            fi
            git fetch $upstream_remote_name
            git merge $upstream_remote_name/$branch

            exit 0
            ;;
        review)
            cd /etc/nixos/pkgs/home-manager
            if [ "$branch_hash" = "$current_hm_hash" ]; then
                echo "No fresh updates, try again a bit later"
                exit 0
            fi
            echo "showing commits $branch_hash...$current_hm_hash"
            git log --pretty=oneline $branch_hash...$current_hm_hash | grep -v Merge | fzf --reverse | xargs git show
            exit 0
            ;;
        tags)
            cd /etc/nixos/pkgs/home-manager
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
