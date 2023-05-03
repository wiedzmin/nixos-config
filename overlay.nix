_: final: _:
let
  inherit (final) lib pkgs;
in
{
  commonutils = import ./modules/util.nix { inherit lib pkgs; };
}
