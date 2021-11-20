PACKAGE=$1
TMP=$(mktemp -d)
cd "$TMP"
{
  nix-shell "<nixpkgs>" -A "$PACKAGE" --run "unpackPhase"
  mv ./* a
  cp -r a b
  $EDITOR b
} >/dev/null 2>&1
diff -u --suppress-common-lines a b
