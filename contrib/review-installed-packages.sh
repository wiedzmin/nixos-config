#!/usr/bin/env zsh

user_login=alex3rd
nixpkgs_branch=nixos-unstable

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
    for ppath in $(nix-store -q --references "$(nix-store -q --references /home/$user_login/.nix-profile | grep home-manager-path)")
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

main() {
    cd /etc/nixos/pkgs/nixpkgs-channels || return
    git checkout -q $nixpkgs_branch
    current_system_commit_hash=$(readlink -f /run/current-system | cut -f4 -d.)
    previous_state_sha=${1:-$current_system_commit_hash}
    git_log=$(git log --pretty=oneline "$nixpkgs_branch...$previous_state_sha" | grep -v Merge)
    for pname in $(list_packages_system | sort | uniq | grep -v "^python$" | grep -v "^python3$")
    do
        echo "$git_log" | grep -sw "$pname:" | grep -v init
    done
    for pname in $(list_packages_hm)
    do
        echo "$git_log" | grep -sw "$pname:" | grep -v init
    done
}

main "$@"

exit 0
