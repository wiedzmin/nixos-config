{ lib, ... }:

{
  attributes.hardware = {
    monitors = {
      externalPrimaryHead.name = lib.mkDefault "DP-2";
      count = 2;
    };
  };
}
