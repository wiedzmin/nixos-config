{ config, pkgs, ... }:
with import ../../const.nix { inherit config pkgs; }; {
  services.xserver.windowManager = {
    default = "stumpwm";
    stumpwm.enable = true;
  };
  home-manager.users."${userName}" = {
    home.file = {
      ".stumpwm.d".source = pkgs.fetchFromGitHub {
        owner = "wiedzmin";
        repo = "stumpwm-config";
        rev = "832513053bb70fc0d24620bc4607b0031193f526";
        sha256 = "0yx3xnw33zkmgl92jnzvzqqvqb7q80gz9ysm3ngbba3hawn2vmma";
      };
    };
  };
}
