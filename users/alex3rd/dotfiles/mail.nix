{config, pkgs, lib, ...}:

let
  certsFile = "/etc/ssl/certs/ca-certificates.crt";
in
{
    home-manager.users.alex3rd = {
        home.file = {
            # TODO: create something "default" traits for browser, pdf reader and whatever and reference them below
            ".mailcap".text = ''
                application/pdf; ${pkgs.zathura}/bin/zathura %s; test=test "$DISPLAY" != ""; description=Portable Document Format; nametemplate=%s.pdf; copiousoutput
                application/x-pdf; ${pkgs.zathura}/bin/zathura %s; test=test "$DISPLAY" != ""; description=Portable Document Format; nametemplate=%s.pdf
                image/gif; ${pkgs.feh}/bin/feh %s; test=test "$DISPLAY" != ""; description=GIF Format; nametemplate=%s.gif; copiousoutput
                image/jpeg; ${pkgs.feh}/bin/feh %s; test=test "$DISPLAY" != ""; description=JPEG Format; nametemplate=%s.jpeg; copiousoutput
                image/jpeg; ${pkgs.feh}/bin/feh %s; test=test "$DISPLAY" != ""; description=JPEG Format; nametemplate=%s.jpg; copiousoutput
                image/png; ${pkgs.feh}/bin/feh %s; test=test "$DISPLAY" != ""; description=PNG Format; nametemplate=%s.png; copiousoutput
                image/vnd.djvu; ${pkgs.zathura}/bin/zathura %s; test=test "$DISPLAY" != ""; description=DjVu Format; nametemplate=%s.djvu; copiousoutput
                image/x-djvu; ${pkgs.zathura}/bin/zathura %s; test=test "$DISPLAY" != ""; description=DjVu Format; nametemplate=%s.djvu; copiousoutput
                text/html; w3m -I %{charset} -dump %s; test=test "$DISPLAY" != ""; description=HyperText; nametemplate=%s.html; copiousoutput
                video/avi; ${pkgs.mpv}/bin/mpv %s; test=test "$DISPLAY" != "" ; description=Video; nametemplate=%s.avi
                video/mp4; ${pkgs.mpv}/bin/mpv %s; test=test "$DISPLAY" != "" ; description=Video; nametemplate=%s.mp4
                video/mpeg; ${pkgs.mpv}/bin/mpv %s; test=test "$DISPLAY" != "" ; description=Video; nametemplate=%s.mpeg
                video/x-flv; ${pkgs.mpv}/bin/mpv %s; test=test "$DISPLAY" != "" ; description=Video; nametemplate=%s.flv
                video/x-matroska; ${pkgs.mpv}/bin/mpv %s; test=test "$DISPLAY" != "" ; description=Video; nametemplate=%s.mkv
                video/x-ms-wmv; ${pkgs.mpv}/bin/mpv %s; test=test "$DISPLAY" != "" ; description=Video; nametemplate=%s.wmv
            '';
            ".mbsyncrc".text = ''
                SyncState *

                IMAPAccount ${config.common.userEmail}
                Host imap.gmail.com
                User ${config.common.userEmail}
                PassCmd "gpg2 -q --for-your-eyes-only --no-tty -d ~/docs/enc/cred/${config.common.userEmail}.gpg"
                CertificateFile ${certsFile}
                SSLType IMAPS

                IMAPStore ${config.common.userEmail}-remote
                Account ${config.common.userEmail}

                MaildirStore ${config.common.userEmail}-local
                Path ~/Maildir/${config.common.userEmail}/
                Inbox ~/Maildir/${config.common.userEmail}/INBOX
                SubFolders Verbatim

                MaildirStore ${config.common.userEmail}-archive
                Path ~/Maildir/archive-${config.common.userEmail}/

                Channel ${config.common.userEmail}-archive
                Master ":${config.common.userEmail}-remote:[Gmail]/All Mail"
                Slave ":${config.common.userEmail}-archive:Archive"
                Create Slave
                SyncState *
                Sync Push Flags

                Channel ${config.common.userEmail}-trash
                Master ":${config.common.userEmail}-remote:[Gmail]/Trash"
                Slave ":${config.common.userEmail}-archive:Trash"
                Create Slave
                Sync All

                Channel ${config.common.userEmail}-drafts
                Master ":${config.common.userEmail}-remote:[Gmail]/Drafts"
                Slave ":${config.common.userEmail}-local:Drafts"
                Create Slave
                Sync All
                Expunge Both

                Channel ${config.common.userEmail}-sent
                Master ":${config.common.userEmail}-remote:[Gmail]/Sent Mail"
                Slave ":${config.common.userEmail}-local:Sent"
                Create Slave
                Sync All
                Expunge Both

                Channel ${config.common.userEmail}-inbox
                Master ":${config.common.userEmail}-remote:INBOX"
                Slave ":${config.common.userEmail}-local:INBOX"
                Create Slave
                Sync All
                Expunge Both

                Channel ${config.common.userEmail}-user-labels
                Master :${config.common.userEmail}-remote:
                Slave :${config.common.userEmail}-local:
                Create Slave
                Sync All
                Patterns "*" "!Drafts" "!Sent" "!Trash" "![Gmail]*" "!INBOX" "!Lists*" "!Cron*"
                Expunge Both

                Channel ${config.common.userEmail}-mailing-lists-and-notifications
                Master :${config.common.userEmail}-remote:
                Slave :${config.common.userEmail}-local:
                Create Slave
                Sync All
                Patterns "Lists*" "Cron*"
                # MaxMessages 2000
                Expunge Both

                Group ${config.common.userEmail}
                Channel ${config.common.userEmail}-trash
                Channel ${config.common.userEmail}-inbox
                Channel ${config.common.userEmail}-drafts
                Channel ${config.common.userEmail}-sent
                Channel ${config.common.userEmail}-user-labels
                Channel ${config.common.userEmail}-mailing-lists-and-notifications
                Channel ${config.common.userEmail}-archive
            '';
            ".msmtprc".text = ''
                defaults
                tls on
                tls_trust_file ${certsFile}
                auto_from on
                logfile ~/.msmtp.log

                account personal
                host smtp.gmail.com
                tls on
                tls_certcheck on
                auth on
                from ${config.common.userEmail}
                user ${config.common.userEmail}
                passwordeval "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/docs/enc/cred/${config.common.userEmail}.gpg"
                port 587

                account default : personal
            '';
        };
    };
}
