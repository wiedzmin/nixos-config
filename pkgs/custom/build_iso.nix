{ bash, nix, ... }:
''
    #!${bash}/bin/bash

    ${nix}/bin/nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=/etc/nixos/contrib/iso/iso.nix
''
