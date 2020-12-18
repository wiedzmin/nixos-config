{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dbms.sqlite;
  user = config.attributes.mainUser.name;
  nixpkgs-litecli = import inputs.nixpkgs-03_12_20 ({
    config = config.nixpkgs.config // { allowUnfree = true; };
    localSystem = { system = "x86_64-linux"; };
  });
in {
  options = {
    dbms.sqlite = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Sqlite helper tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          sqlitebrowser
          nixpkgs-litecli.litecli # TODO: shell automation: fzf for selecting db file, you get the idea
        ];
      };
    })
  ];
}
