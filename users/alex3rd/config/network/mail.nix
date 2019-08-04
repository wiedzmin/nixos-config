{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
let
    imapfilterOptionsTimeout = 120;
    imapfilterOptionsSubscribe = true;
    imapfilterOptionsInfo = true;
    imapfilterOptionsStarttls = true;
    imapfilterOptionsKeepalive = 29;
    imapfilterOptionsCertificates = false;
    imapfilterSpamFlushAfterDays = 14;
    custom = import ../../../../pkgs/custom pkgs config;
in
{
    home-manager.users."${userName}" = {
        accounts.email = {
            accounts."personal" = {
                address = "${config.common.userTraits.email}";
                flavor = "gmail.com";
                primary = true;
                realName = "${config.common.userTraits.fullName}";
                userName = "${config.common.userTraits.email}";
                passwordCommand = "${custom.pass_imap_helper}/bin/pass_imap_helper ${config.common.userTraits.googleAccountPasswordPath}";
                signature = {
                    showSignature = "none";
                    text = config.common.userTraits.signature;
                };
                gpg = {
                    key = config.common.userTraits.primaryGpgKeyID;
                    signByDefault = true;
                };
                notmuch.enable = true;
                msmtp.enable = true;
                mbsync = {
                    enable = true;
                    create = "both";
                    expunge = "both";
                    remove = "both";
                    patterns = [
                         "!Cron*"
                         "!Drafts"
                         "!INBOX"
                         "!Lists*"
                         "!Sent"
                         "!Trash"
                         "![Gmail]*"
                         "*"
                    ];
                };
            };
        };
        programs.notmuch.enable = true;
        programs.msmtp.enable = true;
        programs.mbsync = {
            enable = true;
            extraConfig = ''
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
        };
        services.mbsync = {
            enable = true;
            postExec = "${pkgs.notmuch}/bin/notmuch new";
            frequency = "*:0/30";
        };
    };
    environment.etc."imapfilter_config.lua".text = ''
        options.timeout = ${builtins.toString imapfilterOptionsTimeout}
        options.subscribe = ${lib.boolToString imapfilterOptionsSubscribe}
        options.info = ${lib.boolToString imapfilterOptionsInfo}
        options.starttls = ${lib.boolToString imapfilterOptionsStarttls}
        options.keepalive = ${builtins.toString imapfilterOptionsKeepalive}
        options.certificates = ${lib.boolToString imapfilterOptionsCertificates}

        account_personal = IMAP {
            server = 'imap.gmail.com',
            username = '${config.common.userTraits.email}',
            password = io.popen('${custom.pass_imap_helper}/bin/pass_imap_helper ${config.common.userTraits.googleAccountPasswordPath}', 'r'):read("*a"),
            ssl = 'tls'
        }

        ------------
        -- Rules: --
        ------------
        local from_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (from: folder: "[\"${from}\"] = \"${folder}\"")
                  config.email.imapfilter.fromToFolder))}
        }

        local to_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (to: folder: "[\"${to}\"] = \"${folder}\"")
                  config.email.imapfilter.toToFolder))}
        }

        local cc_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (cc: folder: "[\"${cc}\"] = \"${folder}\"")
                  config.email.imapfilter.ccToFolder))}
        }

        local subject_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (subj: folder: "[\"${subj}\"] = \"${folder}\"")
                  config.email.imapfilter.subjectToFolder))}
        }

        local from_delete = {
            "${(builtins.concatStringsSep "\",\n    \"" config.email.imapfilter.fromDelete)}"
        }

        messages_all = account_personal['INBOX']:select_all()
        messages_new = account_personal['INBOX']:is_new()
        messages_recent = account_personal['INBOX']:is_recent()
        messages_unseen = account_personal['INBOX']:is_unseen()

        for from, folder in pairs(from_tofolder) do
            local results_from = messages_all:contain_from(from)
            results_from:move_messages(account_personal[folder])
        end

        for to, folder in pairs(to_tofolder) do
            local results_to = messages_all:contain_to(to)
            results_to:move_messages(account_personal[folder])
        end

        for cc, folder in pairs(cc_tofolder) do
            local results_cc = messages_all:contain_cc(cc)
            results_cc:move_messages(account_personal[folder])
        end

        for subject, folder in pairs(subject_tofolder) do
            local results_subject = messages_all:contain_subject(subject)
            results_subject:move_messages(account_personal[folder])
        end

        for _, from in ipairs(from_delete) do
            local results_todelete = messages_all:contain_from(from)
            account_personal['INBOX']:delete_messages(results_todelete)
        end

        account_personal['[Gmail]/Trash']:delete_messages(account_personal['[Gmail]/Trash']:is_undeleted())
        account_personal['[Gmail]/Spam']:delete_messages(account_personal['[Gmail]/Spam']:is_unanswered() *
            account_personal['[Gmail]/Spam']:is_older(${toString imapfilterSpamFlushAfterDays}))
    '';
}
