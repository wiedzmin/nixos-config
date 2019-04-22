#!/usr/bin/env zsh

ACTION=$1
USER_LOGIN=$USER

get_package_name() {
    package_path=$1
    package_name=$(echo "$package_path" | cut -d- -f2-)

    # rough filter, may fail on some non-standard versioning schemes
    version_substring_index=$(echo "$package_name" | awk '{print match($0, /\-[v\.\-0-9]*[\-a-z0-9]*$/)}')

    if [ "$version_substring_index" != "0" ]; then
        package_name=$(echo "$package_name" | cut -c1-$((version_substring_index-1)))
    fi
    echo "$package_name"
}

list_packages_hm() {
    for ppath in $(nix-store -q --references "$(nix-store -q --references /home/$USER_LOGIN/.nix-profile | grep home-manager-path)")
    do
        get_package_name "$ppath"
    done
}

list_packages_system() {
    for ppath in $(nix-store -q --references /run/current-system/sw | grep -v nixos)
    do
        get_package_name "$ppath"
    done
}

if [ ! -z "$ACTION" ]; then
    cd /etc/nixos/pkgs/nixpkgs-channels/

    branch=nixos-unstable
    fallback_branch=nixos-unstable-working
    current_system_commit_hash=$(readlink -f /run/current-system | cut -f4 -d.)
    case "$ACTION" in
        update)
            git branch -f $fallback_branch $current_system_commit_hash
            git tag -a -s --force last_working -m "last nixpkgs built and working" "$current_system_commit_hash"
            git fetch origin
            git rebase origin/$branch
            head_ts=$(git show -s --format=%ct HEAD)
            git tag -a -s "v_$head_ts" -m "checkpoint at $head_ts"
            exit 0
            ;;
        installed)
            git checkout -q $branch
            previous_state_sha=${2:-$current_system_commit_hash}
            git_log=$(git log --pretty=oneline "$branch...$previous_state_sha" | grep -v Merge)
            for pname in $(list_packages_system | sort | uniq | grep -v "^python$" | grep -v "^python3$")
            do
                echo "$git_log" | grep -sw "$pname:" | grep -v init
            done
            for pname in $(list_packages_hm)
            do
                echo "$git_log" | grep -sw "$pname:" | grep -v init
            done
            exit 0
            ;;
        new)
            new_package_token=init
            git log --pretty=oneline "$branch...$current_system_commit_hash" | grep $new_package_token | grep -v Merge
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
