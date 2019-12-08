{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.email;
  pass_imap_helper = pkgs.writeShellScriptBin "pass_imap_helper" ''
    echo $(${pkgs.pass}/bin/pass $1 | ${pkgs.coreutils}/bin/head -n 1)
  '';
in {
  options = {
    custom.email = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable in-house emails handling.";
      };
      emailAddress = mkOption {
        type = types.str;
        default = "";
        description = "Email address to use.";
      };
      passwordPath = mkOption {
        type = types.str;
        default = "";
        description = "Path to email account password, in terms of `pass` tool.";
      };
      realName = mkOption {
        type = types.str;
        default = "";
        description = "User's real name.";
      };
      signature.text = mkOption {
        type = types.str;
        default = "";
        description = "User's signature.";
      };
      signature.show = mkOption {
        type = types.enum [ "append" "attach" "none" ];
        default = "none";
        description = "Method to communicate the signature.";
      };
      gpg.sign = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to sign outgoing emails with GPG key.";
      };
      gpg.keyID = mkOption {
        type = types.str;
        default = "";
        description = "GPG key ID to use for mail signing.";
      };
      defaultAccountName = mkOption {
        type = types.str;
        default = "personal";
        description = "Default user's account name for various setups.";
      };
      defaultAccountFlavor = mkOption {
        type = types.enum [ "plain" "gmail.com" "runbox.com" ];
        default = "gmail.com";
        description = "Refer to home-manager/accounts module for code and docs.";
      };
      notmuch.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable notmuch.";
      };
      msmtp.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable msmtp.";
      };
      mbsync.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable mbsync.";
      };
      mbsync.postExec = mkOption {
        type = types.str;
        default = "";
        description = "What to do after mbsync service invocation.";
      };
      mbsync.frequency = mkOption {
        type = types.str;
        default = "*:0/30";
        description = "Mbsync service execution schedule in cron-like format.";
      };
      imapfilter.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable imapfilter.";
      };
      imapfilter.timeout = mkOption {
        type = types.int;
        default = 120;
        description = "The time in seconds to wait for a mail server's response.";
      };
      imapfilter.subscribe = mkOption {
        type = types.bool;
        default = true;
        description = "Makes new mailboxes that were automatically created, get also subscribed.";
      };
      imapfilter.info = mkOption {
        type = types.bool;
        default = true;
        description = "Be a little verbose about processing mailboxes.";
      };
      imapfilter.starttls = mkOption {
        type = types.bool;
        default = true;
        description = "Ask for TLS connection, if supported.";
      };
      imapfilter.keepalive = mkOption {
        type = types.int;
        default = 29;
        description = "Time in minutes before terminating and re-issuing the IDLE command.";
      };
      imapfilter.certificates = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to accept and store server certificate, in order to validate
          the authenticity of the server in future connections.
        '';
      };
      imapfilter.server = mkOption {
        type = types.str;
        default = ""; # TODO: assertion
        description = "IMAP server to connect to.";
      };
      imapfilter.ssl = mkOption {
        type = types.str;
        default = "tls";
        description = "SSL type to use for connection.";
      };
      imapfilter.keepSpamFor = mkOption {
        type = types.int;
        default = 14;
        description = "Days to keep spam before flushing.";
      };
      imapfilter.byFrom = mkOption {
        type = types.attrs;
        default = {};
        description = "Rules to relocate mail by matching message's `From` field.";
      };
      imapfilter.byTo = mkOption {
        type = types.attrs;
        default = {};
        description = "Rules to relocate mail by matching message's `To` field.";
      };
      imapfilter.byCc = mkOption {
        type = types.attrs;
        default = {};
        description = "Rules to relocate mail by matching message's `Cc` field.";
      };
      imapfilter.bySubject = mkOption {
        type = types.attrs;
        default = {};
        description = "Rules to relocate mail by matching message's `Subject` field.";
      };
      imapfilter.deleteByFrom = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Rules to delete mail by matching message's `From` field.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = cfg.emailAddress != "";
          message = "email: Must provide email address.";
        }
        {
          assertion = cfg.passwordPath != "";
          message = "email: Must provide Pass path to password.";
        }
      ] ++ lib.optionals (cfg.gpg.sign) [{
        assertion = cfg.gpg.keyID != "";
        message = "email: Must provide GPG key ID since signing is enabled.";
      }] ++ lib.optionals (cfg.signature.show != "none") [{
        assertion = cfg.signature.text != "" && cfg.realName != "";
        message = "email: Must provide signature and user real name if signature is enabled.";
      }];

      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          gmvault
        ];
        accounts.email = {
          accounts."${cfg.defaultAccountName}" = {
            address = cfg.emailAddress;
            flavor = cfg.defaultAccountFlavor;
            primary = true;
            realName = cfg.realName;
            userName = cfg.emailAddress;
            passwordCommand = "${pass_imap_helper}/bin/pass_imap_helper ${cfg.passwordPath}";
            signature = {
              showSignature = cfg.signature.show;
              text = cfg.signature.text;
            };
            gpg = {
              key = cfg.gpgKeyID;
              signByDefault = true;
            };
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.imapfilter.enable) {
      environment.etc."imapfilter_config.lua".text = ''
        options.timeout = ${builtins.toString cfg.imapfilter.timeout}
        options.subscribe = ${lib.boolToString cfg.imapfilter.subscribe}
        options.info = ${lib.boolToString cfg.imapfilter.info}
        options.starttls = ${lib.boolToString cfg.imapfilter.starttls}
        options.keepalive = ${builtins.toString cfg.imapfilter.keepalive}
        options.certificates = ${lib.boolToString cfg.imapfilter.certificates}

        account_personal = IMAP {
            server = '${cfg.imapfilter.server}',
            username = '${cfg.emailAddress}',
            password = io.popen('${pass_imap_helper}/bin/pass_imap_helper ${cfg.passwordPath}', 'r'):read("*a"),
            ssl = '${cfg.imapfilter.ssl}'
        }

        messages_all = account_personal['INBOX']:select_all()
        messages_new = account_personal['INBOX']:is_new()
        messages_recent = account_personal['INBOX']:is_recent()
        messages_unseen = account_personal['INBOX']:is_unseen()

        ${lib.optionalString (cfg.imapfilter.byFrom != {}) ''
          local from_tofolder = {
            ${(builtins.concatStringsSep '',
              '' (lib.mapAttrsToList (from: folder: ''["${from}"] = "${folder}"'') cfg.imapfilter.byFrom))
            }
          }

          for from, folder in pairs(from_tofolder) do
            local results_from = messages_all:contain_from(from)
            results_from:move_messages(account_personal[folder])
          end
        ''}

        ${lib.optionalString (cfg.imapfilter.byTo != {}) ''
          local to_tofolder = {
            ${(builtins.concatStringsSep '',
              '' (lib.mapAttrsToList (to: folder: ''["${to}"] = "${folder}"'') cfg.imapfilter.byTo))
            }
          }

          for to, folder in pairs(to_tofolder) do
            local results_to = messages_all:contain_to(to)
            results_to:move_messages(account_personal[folder])
          end
        ''}

        ${lib.optionalString (cfg.imapfilter.byCc != {}) ''
          local cc_tofolder = {
            ${(builtins.concatStringsSep '',
              '' (lib.mapAttrsToList (cc: folder: ''["${cc}"] = "${folder}"'') cfg.imapfilter.byCc))
            }
          }

          for cc, folder in pairs(cc_tofolder) do
            local results_cc = messages_all:contain_cc(cc)
            results_cc:move_messages(account_personal[folder])
          end
        ''}

        ${lib.optionalString (cfg.imapfilter.bySubject != {}) ''
          local subject_tofolder = {
            ${(builtins.concatStringsSep '',
              '' (lib.mapAttrsToList (subj: folder: ''["${subj}"] = "${folder}"'') cfg.imapfilter.bySubject))
            }
          }

          for subject, folder in pairs(subject_tofolder) do
            local results_subject = messages_all:contain_subject(subject)
            results_subject:move_messages(account_personal[folder])
          end
        ''}

        ${lib.optionalString (cfg.imapfilter.deleteByFrom != {}) ''
          local from_delete = {
            "${(builtins.concatStringsSep ''",
              "'' cfg.imapfilter.deleteByFrom)
            }"
          }

          for _, from in ipairs(from_delete) do
            local results_todelete = messages_all:contain_from(from)
            account_personal['INBOX']:delete_messages(results_todelete)
          end
        ''}

        ${lib.optionalString (cfg.imapfilter.keepSpamFor > 0) ''
          account_personal['[Gmail]/Trash']:delete_messages(account_personal['[Gmail]/Trash']:is_undeleted())
          account_personal['[Gmail]/Spam']:delete_messages(account_personal['[Gmail]/Spam']:is_unanswered() *
            account_personal['[Gmail]/Spam']:is_older(${toString cfg.imapfilter.keepSpamFor}))
        ''}
      '';
    })
    (mkIf (cfg.enable && cfg.notmuch.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        accounts.email = {
          accounts."${cfg.defaultAccountName}" = {
            notmuch.enable = true;
          };
        };
        programs.notmuch.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.msmtp.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        accounts.email = {
          accounts."${cfg.defaultAccountName}" = {
            msmtp.enable = true;
          };
        };
        programs.msmtp.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.mbsync.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        accounts.email = {
          accounts."${cfg.defaultAccountName}" = {
            mbsync = {
              enable = true;
              create = "both";
              expunge = "both";
              remove = "both";
              patterns = [ "!Cron*" "!Drafts" "!INBOX" "!Lists*" "!Sent" "!Trash" "![Gmail]*" "*" ];
            };
          };
        };
        programs.mbsync = {
          enable = true;
          extraConfig = ''
            Channel ${cfg.emailAddress}-archive
            Master ":${cfg.emailAddress}-remote:[Gmail]/All Mail"
            Slave ":${cfg.emailAddress}-archive:Archive"
            Create Slave
            SyncState *
            Sync Push Flags

            Channel ${cfg.emailAddress}-trash
            Master ":${cfg.emailAddress}-remote:[Gmail]/Trash"
            Slave ":${cfg.emailAddress}-archive:Trash"
            Create Slave
            Sync All

            Channel ${cfg.emailAddress}-drafts
            Master ":${cfg.emailAddress}-remote:[Gmail]/Drafts"
            Slave ":${cfg.emailAddress}-local:Drafts"
            Create Slave
            Sync All
            Expunge Both

            Channel ${cfg.emailAddress}-sent
            Master ":${cfg.emailAddress}-remote:[Gmail]/Sent Mail"
            Slave ":${cfg.emailAddress}-local:Sent"
            Create Slave
            Sync All
            Expunge Both

            Channel ${cfg.emailAddress}-inbox
            Master ":${cfg.emailAddress}-remote:INBOX"
            Slave ":${cfg.emailAddress}-local:INBOX"
            Create Slave
            Sync All
            Expunge Both

            Channel ${cfg.emailAddress}-mailing-lists-and-notifications
            Master :${cfg.emailAddress}-remote:
            Slave :${cfg.emailAddress}-local:
            Create Slave
            Sync All
            Patterns "Lists*" "Cron*"
            # MaxMessages 2000
            Expunge Both

            Group ${cfg.emailAddress}
            Channel ${cfg.emailAddress}-trash
            Channel ${cfg.emailAddress}-inbox
            Channel ${cfg.emailAddress}-drafts
            Channel ${cfg.emailAddress}-sent
            Channel ${cfg.emailAddress}-user-labels
            Channel ${cfg.emailAddress}-mailing-lists-and-notifications
            Channel ${cfg.emailAddress}-archive
          '';
        };
        services.mbsync = {
          enable = true;
          postExec = cfg.mbsync.postExec;
          frequency = cfg.mbsync.frequency;
        };
      };
    })
  ];
}
