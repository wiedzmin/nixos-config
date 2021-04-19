{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.networking.nmconnections;
  wifiModule = types.submodule {
    options = {
      iface = mkOption {
        type = types.str;
        default = "";
        description = "Network interface";
      };
      ssid = mkOption {
        type = types.str;
        default = "";
        description = "WiFi SSID";
      };
      password = mkOption {
        type = types.str;
        default = "";
        description = "WiFi password";
      };
      keyManagement = mkOption {
        type = types.enum [ "wpa-psk" "wpa-eap" ];
        default = "wpa-psk";
        description = "Keys management scheme";
      };
    };
  };
  wifi802_1xModule = types.submodule {
    options = {
      eap = mkOption {
        type = types.enum [ "ttls" ];
        default = "ttls";
        description = "EAP method";
      };
      identity = mkOption {
        type = types.str;
        default = "";
        description = "EAP identity";
      };
      password = mkOption {
        type = types.str;
        default = "";
        description = "EAP password";
      };
      phase2Auth = mkOption {
        type = types.enum [ "mschapv2" ];
        default = "mschapv2";
        description = "Phase 2 authentication method";
      };
    };
  };
  l2tpModule = types.submodule {
    options = {
      gateway = mkOption {
        type = types.str;
        default = "";
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
        default = "";
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
        default = "";
        description = "Username";
      };
      password = mkOption {
        type = types.str;
        default = "";
        description = "Password";
      };
      autoconnect = mkOption {
        type = types.bool;
        default = false;
        description = "Connect automatically";
      };
    };
  };
  connModule = types.submodule {
    options = {
      id = mkOption {
        type = types.str;
        default = "";
        description = "Connection ID";
      };
      uuid = mkOption {
        type = types.str;
        default = "";
        description = "Connection UUID";
      };
      type = mkOption {
        type = types.enum [ "wifi" "vpn" ];
        default = "wifi";
        description = "Connection type";
      };
      wifi = mkOption {
        type = types.nullOr wifiModule;
        default = { };
        description = "Generic WiFi settings";
      };
      wifiEap = mkOption {
        type = types.nullOr wifi802_1xModule;
        default = { };
        description = "802-1x settings";
      };
      l2vpn = mkOption {
        type = types.nullOr l2tpModule;
        default = { };
        description = "L2 VPN settings";
      };
      dns = mkOption {
        type = types.str;
        default = "";
        description = "DNS server";
      };
    };
  };
  genWifiAttrs = attrs: eapAttrs:
    {
      wifi = {
        mac-address-blacklist = "";
        mode = "infrastructure";
        ssid = attrs.ssid;
      };
      wifi-security = {
        auth-alg = "open";
        key-mgmt = attrs.keyManagement;
      } // lib.optionalAttrs (attrs.keyManagement == "wpa-psk") { psk = attrs.password; };
    } // lib.optionalAttrs (attrs.keyManagement == "wpa-eap") {
      eap = eapAttrs.eap;
      identity = eapAttrs.identity;
      password = eapAttrs.password;
      phase2-auth = eapAttrs.phase2Auth;
    };
  genVPNAttrs = attrs: {
    vpn = {
      gateway = attrs.gateway;
      ipsec-enabled = "yes";
      ipsec-esp = lib.concatStringsSep "," attrs.ipsec.esp;
      ipsec-ike = lib.concatStringsSep "," attrs.ipsec.ike;
      ipsec-psk = attrs.ipsec.psk;
      mru = builtins.toString attrs.mru;
      mtu = builtins.toString attrs.mtu;
      password-flags = "0";
      user = attrs.user;
      service-type = "org.freedesktop.NetworkManager.l2tp";
    };
    vpn-secrets = { password = attrs.password; };
  };
  genNMConn = conn: {
    text = lib.generators.toINI { } ({
      connection = {
        id = conn.id;
        uuid = conn.uuid;
        type = conn.type;
        permissions = ""; # "user:alex3rd:;"
      } // lib.optionalAttrs (conn.type == "vpn") { autoconnect = builtins.toString conn.l2vpn.autoconnect; }
      // lib.optionalAttrs (conn.type == "wifi") { interface-name = conn.wifi.iface; };
      ipv4 = {
        dns = conn.dns;
        dns-search = "";
        method = "auto";
      };
      ipv6 = {
        addr-gen-mode = "stable-privacy";
        dns-search = "";
        ip6-privacy = "0";
        method = "auto";
      };
      proxy = { };
    } // lib.optionalAttrs (conn.type == "vpn") (genVPNAttrs conn.l2vpn)
    // lib.optionalAttrs (conn.type == "wifi") (genWifiAttrs conn.wifi conn.wifiEap));
    mode = "0600";
  };
  isProperConn = conn: (conn.type == "wifi" && conn.id != "" &&
    conn.uuid != "" && conn.wifi.ssid != "" && conn.wifi.password != "") ||
  (conn.type == "vpn" && conn.l2vpn.ipsec.psk != "" &&
    conn.l2vpn.user != "" && conn.l2vpn.password != "" && conn.l2vpn.gateway != "");
in
{
  options = {
    ext.networking.nmconnections = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Networkmanager declarativity";
      };
      connections = mkOption {
        type = types.listOf connModule;
        default = [ ];
        description = "Connections metadata";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      environment.etc = builtins.listToAttrs (lib.forEach (builtins.filter (c: isProperConn c) cfg.connections)
        (conn: lib.nameValuePair "NetworkManager/system-connections/${conn.id}.nmconnection" (genNMConn conn)));
    })
    (mkIf ((builtins.filter (c: c.type == "vpn") cfg.connections) != [ ]) {
      networking.networkmanager.enableStrongSwan = true;
      environment.etc = {
        "ipsec.secrets" = {
          text = lib.concatStringsSep "\n" (lib.forEach (builtins.filter (c: c.type == "vpn") cfg.connections)
            (conn: "${conn.l2vpn.gateway} : PSK ${conn.l2vpn.ipsec.psk}"));
          mode = "0600";
        };
      };
    })
  ];
}
