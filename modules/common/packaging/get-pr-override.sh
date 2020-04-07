PR_NO=$1
HASH=$(curl -sL https://github.com/NixOS/nixpkgs/pull/${PR_NO}.patch |
  head -n 1 | grep -o -E -e "[0-9a-f]{40}")
echo pr${PR_NO} = "import (fetchTarball"
echo "  \"\${config.attributes.paths.nixpkgs.archive}${HASH}.tar.gz\")"
echo "    { config = config.nixpkgs.config; };"
