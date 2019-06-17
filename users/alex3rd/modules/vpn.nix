{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

let
    cfg = config.services.vpn-client;
in {
    options = {
        services.vpn-client = {
            enable = mkOption {
                type = types.bool;
                default = false;
                description = ''
                    Whether to enable vpn client.
                '';
            };
            name = mkOption {
                type = types.str;
                default = "<NONAME>";
                description = ''
                    VPN service name. Will look as "openvpn-${cfg.name}" in systemd.
                '';
            };
            config = mkOption {
                type = types.lines;
                default = "";
                description = ''
                    Configuration of this OpenVPN client instance.  See
                    <citerefentry><refentrytitle>openvpn</refentrytitle><manvolnum>8</manvolnum></citerefentry>
                    for details.

                    To import an external config file, use <literal>configPath</literal> option.
                '';
            };
            configPath = mkOption {
                type = types.str;
                default = "";
                description = ''
                    External config file path for this OpenVPN client instance.

                    To use literal config value, provide it in <literal>config</literal> option.
                '';
            };
            # TODO: if disabled, find out how to turn off service before suspend
            keep = mkOption {
                type = types.bool;
                default = false;
                description = ''
                    Whether to keep vpn client up and running between suspends.
                '';
            };
        };
    };

    config = mkIf cfg.enable {
        assertions = [
            { assertion = cfg.name != "<NONAME>"; message = "Must provide vpn service name."; }
            { assertion = (cfg.config == "" && cfg.configPath != "") ||
                          (cfg.config != "" && cfg.configPath == "");
              message = "${cfg.name}: Must provide either configuration contents or path to config file."; }
        ];

        services.openvpn = {
            servers = {
                "${cfg.name}" = {
                    "config" = if (cfg.config == "") then
                        ''config ${cfg.configPath}''
                    else
                        cfg.config;
                    autoStart = false;
                    updateResolvConf = true;
                };
            };
        };

        systemd.user.services."keep-vpn" = mkIf cfg.keep {
            description = "Restart vpn client after suspend";
            after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
            partOf = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ]; # check if it needed/useful
            wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
            serviceConfig = {
                Type = "oneshot";
                ExecStart = "${pkgs.systemd}/bin/systemctl try-restart openvpn-${cfg.name}.service";
            };
        };
    };
}
