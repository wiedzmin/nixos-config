{config, pkgs, lib, ...}:

let
  certsFile = "/etc/ssl/certs/ca-certificates.crt";
in
{
    home-manager.users.alex3rd = {
        home.file = {
            ".mbsyncrc".text = ''
                SyncState *

                IMAPAccount ${config.common.userTraits.email}
                Host imap.gmail.com
                User ${config.common.userTraits.email}
                PassCmd "gpg2 -q --for-your-eyes-only --no-tty -d ~/docs/enc/cred/${config.common.userTraits.email}.gpg"
                CertificateFile ${certsFile}
                SSLType IMAPS

                IMAPStore ${config.common.userTraits.email}-remote
                Account ${config.common.userTraits.email}

                MaildirStore ${config.common.userTraits.email}-local
                Path ~/Maildir/${config.common.userTraits.email}/
                Inbox ~/Maildir/${config.common.userTraits.email}/INBOX
                SubFolders Verbatim

                MaildirStore ${config.common.userTraits.email}-archive
                Path ~/Maildir/archive-${config.common.userTraits.email}/

                Channel ${config.common.userTraits.email}-archive
                Master ":${config.common.userTraits.email}-remote:[Gmail]/All Mail"
                Slave ":${config.common.userTraits.email}-archive:Archive"
                Create Slave
                SyncState *
                Sync Push Flags

                Channel ${config.common.userTraits.email}-trash
                Master ":${config.common.userTraits.email}-remote:[Gmail]/Trash"
                Slave ":${config.common.userTraits.email}-archive:Trash"
                Create Slave
                Sync All

                Channel ${config.common.userTraits.email}-drafts
                Master ":${config.common.userTraits.email}-remote:[Gmail]/Drafts"
                Slave ":${config.common.userTraits.email}-local:Drafts"
                Create Slave
                Sync All
                Expunge Both

                Channel ${config.common.userTraits.email}-sent
                Master ":${config.common.userTraits.email}-remote:[Gmail]/Sent Mail"
                Slave ":${config.common.userTraits.email}-local:Sent"
                Create Slave
                Sync All
                Expunge Both

                Channel ${config.common.userTraits.email}-inbox
                Master ":${config.common.userTraits.email}-remote:INBOX"
                Slave ":${config.common.userTraits.email}-local:INBOX"
                Create Slave
                Sync All
                Expunge Both

                Channel ${config.common.userTraits.email}-user-labels
                Master :${config.common.userTraits.email}-remote:
                Slave :${config.common.userTraits.email}-local:
                Create Slave
                Sync All
                Patterns "*" "!Drafts" "!Sent" "!Trash" "![Gmail]*" "!INBOX" "!Lists*" "!Cron*"
                Expunge Both

                Channel ${config.common.userTraits.email}-mailing-lists-and-notifications
                Master :${config.common.userTraits.email}-remote:
                Slave :${config.common.userTraits.email}-local:
                Create Slave
                Sync All
                Patterns "Lists*" "Cron*"
                # MaxMessages 2000
                Expunge Both

                Group ${config.common.userTraits.email}
                Channel ${config.common.userTraits.email}-trash
                Channel ${config.common.userTraits.email}-inbox
                Channel ${config.common.userTraits.email}-drafts
                Channel ${config.common.userTraits.email}-sent
                Channel ${config.common.userTraits.email}-user-labels
                Channel ${config.common.userTraits.email}-mailing-lists-and-notifications
                Channel ${config.common.userTraits.email}-archive
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
                from ${config.common.userTraits.email}
                user ${config.common.userTraits.email}
                passwordeval "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/docs/enc/cred/${config.common.userTraits.email}.gpg"
                port 587

                account default : personal
            '';
        };
    };
}
