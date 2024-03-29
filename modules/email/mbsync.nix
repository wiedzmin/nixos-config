{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.ext.programs.mbsync;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";

  # Accounts for which mbsync is enabled.
  mbsyncAccounts = filter (a: a.mbsync.enable) (attrValues hm.accounts.email.accounts);

  genTlsConfig = tls:
    {
      SSLType = if !tls.enable then "None" else if tls.useStartTls then "STARTTLS" else "IMAPS";
    }
    // optionalAttrs (tls.enable && tls.certificatesFile != null) { CertificateFile = toString tls.certificatesFile; };

  genSection = header: entries:
    let
      escapeValue = escape [ ''"'' ];
      hasSpace = v: builtins.match ".* .*" v != null;
      genValue = n: v:
        if isList v then
          concatMapStringsSep " " (genValue n) v
        else if isBool v then
          (if v then "yes" else "no")
        else if isInt v then
          toString v
        else if isString v && hasSpace v then
          ''"${escapeValue v}"''
        else if isString v then
          v
        else
          let prettyV = lib.generators.toPretty { } v;
          in throw "mbsync: unexpected value for option ${n}: '${prettyV}'";
    in
    ''
      ${header}
      ${concatStringsSep "\n" (mapAttrsToList (n: v: "${n} ${genValue n v}") entries)}
    '';

  genAccountConfig = account:
    with account;
    genSection "IMAPAccount ${name}"
      ({
        Host = imap.host;
        User = userName;
        PassCmd = toString passwordCommand;
      } // genTlsConfig imap.tls // optionalAttrs (imap.port != null) { Port = toString imap.port; }
      // mbsync.extraConfig.account) + "\n"
    + genSection "IMAPStore ${name}-remote" ({ Account = name; } // mbsync.extraConfig.remote) + "\n"
    + genSection "MaildirStore ${name}-local" ({
      Path = "${maildir.absPath}/";
      Inbox = "${maildir.absPath}/${folders.inbox}";
      SubFolders = "Verbatim";
    } // optionalAttrs (mbsync.flatten != null) { Flatten = mbsync.flatten; } // mbsync.extraConfig.local) + "\n";

  genGroupConfig = name: channels:
    let genGroupChannel = n: boxes: "Channel ${n}:${concatStringsSep "," boxes}";
    in concatStringsSep "\n" ([ "Group ${name}" ] ++ mapAttrsToList genGroupChannel channels);

in
{
  options = {
    ext.programs.mbsync = {
      enable = mkEnableOption "mbsync IMAP4 and Maildir mailbox synchronizer";

      scanning.paperless.package = mkPackageOption pkgs "isync" { extraDescription = "The package to use for the mbsync binary"; };

      groups = mkOption {
        type = types.attrsOf (types.attrsOf (types.listOf types.str));
        default = { };
        example = literalExpression ''
          {
            inboxes = {
              account1 = [ "Inbox" ];
              account2 = [ "Inbox" ];
            };
          }
        '';
        description = ''
          Definition of groups.
        '';
      };

      extraConfig = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Extra configuration lines to add to the mbsync configuration.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions =
      let
        checkAccounts = pred: msg:
          let badAccounts = filter pred mbsyncAccounts;
          in {
            assertion = badAccounts == [ ];
            message = "mbsync: ${msg} for accounts: " + concatMapStringsSep ", " (a: a.name) badAccounts;
          };
      in
      [
        (checkAccounts (a: a.maildir == null) "Missing maildir configuration")
        (checkAccounts (a: a.imap == null) "Missing IMAP configuration")
        (checkAccounts (a: a.passwordCommand == null) "Missing passwordCommand")
        (checkAccounts (a: a.userName == null) "Missing username")
      ];

    home-manager.users."${user}" = {
      home.packages = [ cfg.package ];

      programs.notmuch.new.ignore = [ ".uidvalidity" ".mbsyncstate" ];

      home.file.".mbsyncrc".text =
        let
          accountsConfig = map genAccountConfig mbsyncAccounts;
          groupsConfig = mapAttrsToList genGroupConfig cfg.groups;
        in
        concatStringsSep "\n"
          ([
            ''
              # Generated by Home Manager.
            ''
          ] ++ accountsConfig ++ groupsConfig ++ optional (cfg.extraConfig != "") cfg.extraConfig) + "\n";

      home.activation = lib.optionalAttrs (mbsyncAccounts != [ ]) {
        createMaildir = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = "mkdir -m700 -p ${concatMapStringsSep " " (a: a.maildir.absPath) mbsyncAccounts}";
        };
      };
    };
  };
}
