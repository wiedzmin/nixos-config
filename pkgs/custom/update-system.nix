{ bash, nix, systemd, ... }:
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

  build_configuration
  switch_configuration
  ensure_kernel_update
''
