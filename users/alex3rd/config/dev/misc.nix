{ config, pkgs, lib, ... }:
with import ../../../../pkgs/util.nix { inherit lib config pkgs; };
with import ../../const.nix { inherit lib config pkgs; }; {
  programs = {
    mtr.enable = true;
    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };
    wavemon.enable = true;
  };
  users.extraUsers."${userName}".extraGroups = [ "wireshark" ];
}
