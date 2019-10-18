{ config, lib, pkgs, ... }:
with lib;

let cfg = config.network;
in {
  options = {
    network = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable networking support";
      };
      tools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable network monitoring/debug tools";
      };
      clients.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various network clients, mostly for development";
      };
      remoteControlling.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable clients to control remote machines.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.tools.enable) {
      programs = {
        mtr.enable = true;
        wireshark = {
          enable = true;
          package = pkgs.wireshark-qt;
        };
        wavemon.enable = true;
      };

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nload
          speedtest-cli
          nethogs
          jnettop
          bmon
          ngrep
          netsniff-ng
          pcapfix
          tcpreplay
          vnstat # TODO: also review corrsponding service
          hss
        ];
      };

      users.extraUsers."${config.attributes.mainUser.name}".extraGroups = [ "wireshark" ];
    })
    (mkIf (cfg.enable && cfg.clients.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          # curlie
          # davfs2
          # vegeta # TODO: package
          eternal-terminal
          gcalcli
          hasmail
          hpWorking.http-prompt
          httplab
          rclone
          socat
          tcpdump
          websocat
          weighttp
          wuzz
        ];
      };
    })
    (mkIf (cfg.enable && cfg.remoteControlling.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          anydesk
          teamviewer
        ];
      };
    })
  ];
}
