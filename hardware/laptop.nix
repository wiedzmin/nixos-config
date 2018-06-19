{ lib, pkgs, ... }:

{
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "ondemand";
  };
}
