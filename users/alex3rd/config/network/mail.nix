{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
with import ../../secrets/const.nix {inherit config pkgs lib;};
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
                address = "${userEmail}";
                flavor = "gmail.com";
                primary = true;
                realName = "${userFullName}";
                userName = "${userEmail}";
                passwordCommand = "${custom.pass_imap_helper}/bin/pass_imap_helper ${userGoogleAccountPasswordPath}";
                signature = {
                    showSignature = "none";
                    text = userSignature;
                };
                gpg = {
                    key = userPrimaryGpgKeyID;
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
                Channel ${userEmail}-archive
                Master ":${userEmail}-remote:[Gmail]/All Mail"
                Slave ":${userEmail}-archive:Archive"
                Create Slave
                SyncState *
                Sync Push Flags

                Channel ${userEmail}-trash
                Master ":${userEmail}-remote:[Gmail]/Trash"
                Slave ":${userEmail}-archive:Trash"
                Create Slave
                Sync All

                Channel ${userEmail}-drafts
                Master ":${userEmail}-remote:[Gmail]/Drafts"
                Slave ":${userEmail}-local:Drafts"
                Create Slave
                Sync All
                Expunge Both

                Channel ${userEmail}-sent
                Master ":${userEmail}-remote:[Gmail]/Sent Mail"
                Slave ":${userEmail}-local:Sent"
                Create Slave
                Sync All
                Expunge Both

                Channel ${userEmail}-inbox
                Master ":${userEmail}-remote:INBOX"
                Slave ":${userEmail}-local:INBOX"
                Create Slave
                Sync All
                Expunge Both

                Channel ${userEmail}-mailing-lists-and-notifications
                Master :${userEmail}-remote:
                Slave :${userEmail}-local:
                Create Slave
                Sync All
                Patterns "Lists*" "Cron*"
                # MaxMessages 2000
                Expunge Both

                Group ${userEmail}
                Channel ${userEmail}-trash
                Channel ${userEmail}-inbox
                Channel ${userEmail}-drafts
                Channel ${userEmail}-sent
                Channel ${userEmail}-user-labels
                Channel ${userEmail}-mailing-lists-and-notifications
                Channel ${userEmail}-archive
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
            username = '${userEmail}',
            password = io.popen('${custom.pass_imap_helper}/bin/pass_imap_helper ${userGoogleAccountPasswordPath}', 'r'):read("*a"),
            ssl = 'tls'
        }

        ------------
        -- Rules: --
        ------------
        local from_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (from: folder: "[\"${from}\"] = \"${folder}\"")
                      imapfilterFromToFolder))}
        }

        local to_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (to: folder: "[\"${to}\"] = \"${folder}\"")
                  imapfilterToToFolder))}
        }

        local cc_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (cc: folder: "[\"${cc}\"] = \"${folder}\"")
                  imapfilterCcToFolder))}
        }

        local subject_tofolder = {
            ${(builtins.concatStringsSep ",\n    "
                  (lib.mapAttrsToList (subj: folder: "[\"${subj}\"] = \"${folder}\"")
                  imapfilterSubjectToFolder))}
        }

        local from_delete = {
            "${(builtins.concatStringsSep "\",\n    \"" imapfilterFromDelete)}"
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
