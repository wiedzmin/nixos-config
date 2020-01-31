sources=$(@fdBinary@ -t file nix -E forges -E "*.gpg*" /etc/nixos)
for file in "$sources"; do
  @nixfmtBinary@ -w 120 $file
done
