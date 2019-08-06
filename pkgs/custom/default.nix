p: c:

with p;
builtins.mapAttrs (name: value:
python3Packages.buildPythonPackage rec {
    pname = name;
    version = "unstable";
    src = value;
    format = "other";
    unpackPhase = "true";
    buildInputs = [ makeWrapper ];
    propagatedBuildInputs = [
        python3Full
        python3Packages.papis-python-rofi
        python3Packages.GitPython
    ];
    buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${name}";
    installPhase = "true";
}) {
    pkgsctl = ./pkgsctl;
    confctl = ./confctl;
} // builtins.mapAttrs (name: value:
writeTextFile {
    inherit name;
    destination = "/bin/${name}";
    text = callPackage value {
        config = c;
    };
    executable = true;
    checkPhase = "${bash}/bin/bash -n $src";
}) {
    git_lib = ./lib/git.nix; # TODO: add secrets pre-commit checking
    # ========
    autorandr_profiles = ./autorandr_profiles.nix;
    bitbucket_team_contributor_repos = ./bitbucket_team_contributor_repos.nix;
    bookshelf = ./bookshelf.nix;
    buku_add = ./buku_add.nix;
    buku_search_tag = ./buku_search_tag.nix;
    buku_search_url = ./buku_search_url.nix;
    ctop_hosts = ./ctop_hosts.nix;
    current_system_hash = ./current_system_hash.nix;
    dbms = ./dbms.nix;
    discover_containerized_services = ./discover_containerized_services.nix;
    docker-machine-export = ./docker-machine-export.nix;
    docker-machine-import = ./docker-machine-import.nix;
    docker_containers_traits = ./docker_containers_traits.nix; # TODO: review and fix
    docker_shell = ./docker_shell.nix;
    docker_stacks_info = ./docker_stacks_info.nix;
    extra_hosts_traits = ./extra_hosts_traits.nix;
    force_unmount_nas = ./force_unmount_nas.nix;
    gen-nix-du = ./gen-nix-du.nix;
    git_remote_diff = ./git_remote_diff.nix;
    jnettop_hosts = ./jnettop_hosts.nix;
    kill-compton = ./kill-compton.nix;
    mount_nas_volume = ./mount_nas_volume.nix;
    pass_imap_helper = ./pass_imap_helper.nix;
    remote_docker_logs = ./remote_docker_logs.nix;
    rescale-wallpaper = ./rescale-wallpaper.nix;
    screenshot_active_window = ./screenshot_active_window.nix;
    screenshot_full = ./screenshot_full.nix;
    screenshot_region = ./screenshot_region.nix;
    search_prompt = ./search_prompt.nix;
    search_selection = ./search_selection.nix;
    services_journals = ./services_journals.nix;
    shell-org-capture = ./shell-org-capture.nix;
    ssh_custom_user = ./ssh_custom_user.nix;
    systemctl-status = ./systemctl-status.nix;
    tmuxp_sessions = ./tmuxp_sessions.nix;
    unmount_nas_volume = ./unmount_nas_volume.nix;
    update-system = ./update-system.nix;
    uptime_info = ./uptime_info.nix;
    webjumps = ./webjumps.nix;
    wifi-status = ./wifi-status.nix;
}
