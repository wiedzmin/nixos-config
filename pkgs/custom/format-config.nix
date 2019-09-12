{ bash, fd, nixfmt, ... }: ''
  #!${bash}/bin/bash

  sources=$(${fd}/bin/fd -t file nix -E forges -E "*.gpg*" /etc/nixos)
  for file in "$sources"; do
    ${nixfmt}/bin/nixfmt -w 120 $file
  done
''
