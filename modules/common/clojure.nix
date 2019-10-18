{ config, lib, pkgs, ... }:
with lib;

let cfg = config.dev.clojure;
in {
  options = {
    dev.clojure = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Clojure dev infrastructure.";
      };
    };
  };

  config = mkIf cfg.enable {
    home-manager.users."${config.attributes.mainUser.name}" = {
      home.packages = with pkgs; [
        boot
        cfr
        clojure
        leiningen
      ];
    };
  };
}
