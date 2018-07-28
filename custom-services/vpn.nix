{ pkgs, ... }:

{
    services.openvpn = {
        servers = {
            jobvpn = {
                config = ''config /etc/nixos/private/vpn/job.current/office.ovpn'';
                autoStart = false;
                up = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
                down = "${pkgs.update-resolv-conf}/libexec/openvpn/update-resolv-conf";
            };
        };
    };
}
