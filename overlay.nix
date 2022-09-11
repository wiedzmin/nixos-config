inputs: final: prev:
let
  inherit (final) system lib;
in
rec {
  commonutils = import ./modules/util.nix { inherit lib; };
}
