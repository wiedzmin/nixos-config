{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.networking.l2vpn;
  connType = types.submodule {
    options = {
      id = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "Connection ID";
      };
      uuid = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "Connection UUID";
      };
      gateway = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "VPN gateway";
      };
      ipsec.esp = mkOption {
        type = types.listOf types.str;
        default = [ "aes256-sha1" "aes128-sha1" "3des-sha1!" ];
        description = "IPSEC ESP algorithms";
      };
      ipsec.ike = mkOption {
        type = types.listOf types.str;
        default = [ "aes256-sha1-ecp384" "aes128-sha1-ecp256" "3des-sha1-modp1024!" ];
        description = "IPSEC IKE algorithms";
      };
      ipsec.psk = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "Preshared key";
      };
      mru = mkOption {
        type = types.int;
        default = 1400;
        description = "MRU";
      };
      mtu = mkOption {
        type = types.int;
        default = 1400;
        description = "MTU";
      };
      user = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "Username";
      };
      password = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "Password";
      };
      dns = mkOption {
        type = types.str;
        default = ""; # TODO: make assertion
        description = "DNS server";
      };
    };
  };
  genNMConn = conn: {
    text = lib.generators.toINI { } {
      connection = {
        id = conn.id;
        uuid = conn.uuid;
        type = "vpn";
        autoconnect = "false";
        permissions = "";
      };
      vpn = {
        gateway = conn.gateway;
        ipsec-enabled = "yes";
        ipsec-esp = lib.concatStringsSep "," conn.ipsec.esp;
        ipsec-ike = lib.concatStringsSep "," conn.ipsec.ike;
        ipsec-psk = conn.ipsec.psk;
        mru = builtins.toString conn.mru;
        mtu = builtins.toString conn.mtu;
        password-flags = "0";
        user = conn.user;
        service-type = "org.freedesktop.NetworkManager.l2tp";
      };
      vpn-secrets = { password = conn.password; };
      ipv4 = {
        dns = conn.dns;
        dns-search = "";
        method = "auto";
      };
    };
    mode = "0600";
  };
in {
  options = {
    custom.networking.l2vpn = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable L2 VPN";
      };
      connections = mkOption {
        type = types.listOf connType;
        default = [ ];
        description = "Connections metadata";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      networking.networkmanager.enableStrongSwan = true;

      environment.etc = {
        "ipsec.secrets" = {
          text =
            lib.concatStringsSep "\n" (lib.forEach cfg.connections (conn: "${conn.gateway} : PSK ${conn.ipsec.psk}"));
          mode = "0600";
        };
      } // builtins.listToAttrs (lib.forEach cfg.connections
        (conn: lib.nameValuePair "NetworkManager/system-connections/${conn.id}.nmconnection" (genNMConn conn)));
    })
  ];
}
