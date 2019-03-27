{config, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
{
    systemd.services."imapfilter" = {
        description = "Ordering mailbox through IMAP";
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "${pkgs.imapfilter}/bin/imapfilter -c /etc/imapfilter_config.lua -v";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."imapfilter" = {
        description = "Ordering mailbox through IMAP";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "30min";
        };
    };

}
