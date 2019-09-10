p: c:

with p;
builtins.mapAttrs (
  name: value:
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
    }
) {}
// builtins.mapAttrs (
     name: value:
       writeTextFile {
         inherit name;
         destination = "/bin/${name}";
         text = callPackage value {
           config = c;
         };
         executable = true;
         checkPhase = "${bash}/bin/bash -n $src";
       }
   ) {
     git_lib = ./lib/git.nix; # TODO: add secrets pre-commit checking
     # ========
     bookshelf = ./bookshelf.nix;
     ctop_hosts = ./ctop_hosts.nix;
     dbms = ./dbms.nix;
     docker_shell = ./docker_shell.nix;
     docker_stacks_info = ./docker_stacks_info.nix;
     extra_hosts_traits = ./extra_hosts_traits.nix;
     force_unmount_nas = ./force_unmount_nas.nix;
     jnettop_hosts = ./jnettop_hosts.nix;
     mount_nas_volume = ./mount_nas_volume.nix;
     remote_docker_logs = ./remote_docker_logs.nix;
     rescale-wallpaper = ./rescale-wallpaper.nix;
     search_prompt = ./search_prompt.nix;
     search_selection = ./search_selection.nix;
     unmount_nas_volume = ./unmount_nas_volume.nix;
     webjumps = ./webjumps.nix;
   }
