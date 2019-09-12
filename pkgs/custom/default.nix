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
      python3Packages.telethon # for some hypothetical future notifications
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
      text = callPackage value { config = c; };
      executable = true;
      checkPhase = "${bash}/bin/bash -n $src";
    }) {
      git_lib = ./lib/git.nix; # TODO: add secrets pre-commit checking
      # ========
      autorandr_profiles = ./autorandr_profiles.nix;
      bitbucket_team_contributor_repos = ./bitbucket_team_contributor_repos.nix;
      bootstrap_custom_config = ./bootstrap_custom_config.nix;
      build_iso = ./build_iso.nix;
      buku_add = ./buku_add.nix;
      buku_search_tag = ./buku_search_tag.nix;
      buku_search_url = ./buku_search_url.nix;
      burn_iso = ./burn_iso.nix;
      current_system_hash = ./current_system_hash.nix;
      dflinter = ./dflinter.nix;
      discover_containerized_services = ./discover_containerized_services.nix;
      dlint = ./dlint.nix;
      docker-machine-export = ./docker-machine-export.nix;
      docker-machine-import = ./docker-machine-import.nix;
      docker_containers_traits = ./docker_containers_traits.nix; # TODO: review and fix
      format-config = ./format-config.nix;
      gen-nix-du = ./gen-nix-du.nix;
      git_remote_diff = ./git_remote_diff.nix;
      goreplay-capture = ./goreplay-capture.nix;
      hadolintd = ./hadolintd.nix;
      kill-compton = ./kill-compton.nix;
      pass_imap_helper = ./pass_imap_helper.nix;
      screenshot_active_window = ./screenshot_active_window.nix;
      screenshot_full = ./screenshot_full.nix;
      screenshot_region = ./screenshot_region.nix;
      services_journals = ./services_journals.nix;
      shell-org-capture = ./shell-org-capture.nix;
      ssh_custom_user = ./ssh_custom_user.nix;
      systemctl-status = ./systemctl-status.nix;
      tmuxp_sessions = ./tmuxp_sessions.nix;
      update-system = ./update-system.nix;
      uptime_info = ./uptime_info.nix;
      vdi2qcow2 = ./vdi2qcow2.nix;
      wifi-status = ./wifi-status.nix;
    }

    # TODO: also make as git subcommand
    # git fetch upstream --tags
    # git push origin --tags

    # https://github.com/jwiegley/nix-config/blob/master/bin/nixpkgs-bisect - integrate into pkgsctl workflow
