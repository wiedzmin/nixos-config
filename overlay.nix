inputs: final: prev:
let
  inherit (final) lib;
in
{
  commonutils = import ./modules/util.nix { inherit lib; };
}
