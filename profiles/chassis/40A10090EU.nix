{ lib, ... }:

{
  attributes.hardware = {
    monitors = {
      externalPrimaryHead.name = lib.mkDefault "DP-2-3";
      externalSecondaryHead.name = lib.mkDefault "DP-2-2";
      count = 3;
    };
  };
}
