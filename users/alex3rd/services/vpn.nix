{ config, pkgs, ... }:
with import ../const.nix {inherit config pkgs;};
{
    services.openvpn = {
        servers = {
            jobvpn = {
                # TODO: make more declarative, i.e. to hide private part and automate all the rest
                config = ''config /etc/nixos/users/${userName}/private/vpn/job.current/office.ovpn'';
                autoStart = false;
                up = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
                down = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
            };
        };
    };
}
