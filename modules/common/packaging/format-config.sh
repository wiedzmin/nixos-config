sources=$(@fdBinary@ -t file nix -E secrets -E xmonad /etc/nixos)
for file in "$sources"; do
  @nixfmtBinary@ -w 120 $file
done
