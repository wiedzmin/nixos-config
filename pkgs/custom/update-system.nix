{ bash, git, nix, systemd, ... }:
''
  #!${bash}/bin/bash

  cd /etc/nixos

  build_configuration() {
      ${nix}/bin/nix build -f ./pkgs/forges/github.com/NixOS/nixpkgs-channels/nixos system $@
      result=$?
      if [[ $result == 1 ]] || [[ $result == 100 ]]
      then
          exit 1
      fi
  }

  switch_configuration() {
      dir=$(pwd)
      export SHELL=/bin/sh
      pkexec ${nix}/bin/nix-env --profile /nix/var/nix/profiles/system --set $(readlink $dir/result)
      pkexec $dir/result/bin/switch-to-configuration switch
  }

  update_nixpkgs_suffix() {
      if nixpkgs=$(${nix}/bin/nix-instantiate --find-file nixpkgs); then
          rev=
          if [ -e "$nixpkgs/.git" ]; then
              cd $nixpkgs
              rev=$(${git}/bin/git rev-parse --short HEAD)
              if ${git}/bin/git describe --always --dirty | grep -q dirty; then
                  rev+=M
              fi
          fi
          if [ -n "$rev" ]; then
              suffix=".git.$rev"
              pkexec ${bash}/bin/bash -c "echo -n $suffix > $nixpkgs/.version-suffix" || true
          fi
      fi
  }

  ensure_kernel_update() {
      current=$(readlink -f /run/current-system/kernel)
      booted=$(readlink -f /run/booted-system/kernel)

      if [ "$current" != "$booted" ]; then
          read -p "Kernel changed, reboot? " -n 1 -r
          if [[ $REPLY =~ ^[Yy]$ ]]
          then
              echo "Rebooting in 5 sec..."
              sleep 5
              echo "kernel changed, reboot" | ${systemd}/bin/systemd-cat --identifier "post-upgrade-check";
              reboot
          fi
      else
          echo "same kernel, do not reboot" | ${systemd}/bin/systemd-cat --identifier "post-upgrade-check";
      fi
  }

  update_nixpkgs_suffix
  build_configuration
  switch_configuration
  ensure_kernel_update
''
