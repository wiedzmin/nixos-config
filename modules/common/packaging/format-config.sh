sources=$(fd -t file nix -E secrets -E xmonad /etc/nixos)
for file in "$sources"; do
  nixfmt -w 120 $file
done
