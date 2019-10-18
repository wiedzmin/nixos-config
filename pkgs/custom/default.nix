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
      python3Packages.dmenu-python
      python3Packages.GitPython
      python3Packages.telethon # for some hypothetical future notifications
      # TODO: move per script
      autorandr
      eternal-terminal
      systemd
      tmux
      tmuxp
    ];
    buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${name}";
    installPhase = "true";
    pythonCatchConflictsPhase = "true";
  }) {
    autorandr_profiles = ./autorandr_profiles;
    bookshelf = ./bookshelf;
    confctl = ./confctl;
    pkgsctl = ./pkgsctl;
    services_journals = ./services_journals;
    ssh_custom_user = ./ssh_custom_user;
    tmuxp_sessions = ./tmuxp_sessions;
  } // builtins.mapAttrs (name: value:
    writeTextFile {
      inherit name;
      destination = "/bin/${name}";
      text = callPackage value { config = c; };
      executable = true;
      checkPhase = "${bash}/bin/bash -n $src";
    }) {
      bootstrap_custom_config = ./bootstrap_custom_config.nix;
      build_iso = ./build_iso.nix;
      buku_add = ./buku_add.nix;
      buku_search_tag = ./buku_search_tag.nix;
      buku_search_url = ./buku_search_url.nix;
      burn_iso = ./burn_iso.nix;
      ctop_hosts = ./ctop_hosts.nix;
      current_system_hash = ./current_system_hash.nix;
      discover_containerized_services = ./discover_containerized_services.nix;
      docker_shell = ./docker_shell.nix;
      force_unmount_nas = ./force_unmount_nas.nix;
      format-config = ./format-config.nix;
      gen-nix-du = ./gen-nix-du.nix;
      goreplay-capture = ./goreplay-capture.nix;
      jnettop_hosts = ./jnettop_hosts.nix;
      kill-compton = ./kill-compton.nix;
      mount_nas_volume = ./mount_nas_volume.nix;
      remote_docker_logs = ./remote_docker_logs.nix;
      search_prompt = ./search_prompt.nix;
      search_selection = ./search_selection.nix;
      shell-org-capture = ./shell-org-capture.nix;
      systemctl-status = ./systemctl-status.nix;
      unmount_nas_volume = ./unmount_nas_volume.nix;
      update-system = ./update-system.nix;
      uptime_info = ./uptime_info.nix;
      webjumps = ./webjumps.nix;
      wifi-status = ./wifi-status.nix;
    }

    # TODO: also make as git subcommand
    # git fetch upstream --tags
    # git push origin --tags

    # https://github.com/jwiegley/nix-config/blob/master/bin/nixpkgs-bisect - integrate into pkgsctl workflow
