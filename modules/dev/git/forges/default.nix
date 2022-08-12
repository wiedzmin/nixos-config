{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: check for possible conflicts between `config.ext.networking.hosts` and `config.dev.git.forges` definitions

let
  cfg = config.dev.git.forges;
  user = config.attributes.mainUser.name;
  typeToGitlinkSymbols = {
    "github" = {
      "git-link-remote-alist" = "git-link-github";
      "git-link-commit-remote-alist" = "git-link-commit-github";
    };
    "bitbucket" = {
      "git-link-remote-alist" = "git-link-bitbucket";
      "git-link-commit-remote-alist" = "git-link-commit-bitbucket";
    };
    "gitlab" = {
      "git-link-remote-alist" = "git-link-gitlab";
      "git-link-commit-remote-alist" = "git-link-commit-github";
    };
  };

  collectWorkspaceRoots = forges:
    mapAttrs' (key: meta: nameValuePair "${key}" meta.workspaceRoot)
      (filterAttrs (_: meta: lib.hasAttr "workspaceRoot" meta) forges);
  collectPassCredentials = forges:
    (foldAttrs (n: a: n // a) { } (collect (f: f ? passCredentialsMap) forges))."passCredentialsMap";
  mkMatchBlock = meta: {
    inherit (meta.ssh.matchBlock) hostname user serverAliveInterval identitiesOnly extraOptions;
  } // optionalAttrs (meta.ssh.matchBlock.identityFile != null) {
    inherit (meta.ssh.matchBlock) identityFile;
  } // optionalAttrs (meta.ssh.matchBlock.identityFile == null) {
    identityFile = builtins.toString (pkgs.writeTextFile {
      name = "${builtins.replaceStrings ["."] ["_"] meta.ssh.matchBlock.hostname}_ssh_private_key";
      text = meta.ssh.keypair.private;
    });
  };
  collectMatchBlocks = forges:
    mapAttrs' (_: meta: nameValuePair meta.ssh.matchBlock.hostname (mkMatchBlock meta)) forges;
  collectExtraConfig = forges:
    (foldAttrs (n: a: n // a) { } (collect (f: f ? extraConfig) forges))."extraConfig";
  getUrlsToTypesMapping = forges:
    mapAttrs' (_: meta: nameValuePair meta.ssh.matchBlock.hostname meta.type) forges;
  genBrowseAtRemoteTypesPatch = mapping: ''
    (eval-after-load 'browse-at-remote
      '(progn
      ${mkIndent 4}${concatStringsSep "\n${mkIndent 4}"
        (mapAttrsToList (host: type: ''
          (push '("${host}" . "${type}") browse-at-remote-remote-type-regexps)
        '') mapping)}))
  '';
  genGitlinkTypesPatch = mapping: ''
    (eval-after-load 'git-link
      '(progn
        ${mkIndent 4}${concatStringsSep "\n${mkIndent 4}"
          (mapAttrsToList (host: type: ''
            (add-to-list 'git-link-remote-alist '("${host}" ${
              typeToGitlinkSymbols."${type}"."git-link-remote-alist"}))
            (add-to-list 'git-link-commit-remote-alist '("${host}" ${
              typeToGitlinkSymbols."${type}"."git-link-commit-remote-alist"}))
          '') mapping)}))
  '';

  sshModule = types.submodule {
    options = {
      private = mkOption {
        type = types.str;
        default = "";
        description = "SSH private key.";
      };
      public = mkOption {
        type = types.str;
        default = "";
        description = "SSH public key.";
      };
    };
  };
  matchBlockModule = types.submodule {
    options = {
      hostname = mkOption {
        type = types.str;
        default = "";
        description = "Forge hostname.";
      };
      user = mkOption {
        type = types.str;
        default = "git";
        description = "Forge remote user.";
      };
      serverAliveInterval = mkOption {
        type = types.int;
        default = 60;
        description = "SSH server alive interval.";
      };
      identityFile = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Path to private key file.";
      };
      identitiesOnly = mkOption {
        type = types.bool;
        default = true;
        description = "Use only explicitly defined identities.";
      };
      extraOptions = mkOption {
        type = types.attrsOf types.str;
        default = {
          ControlMaster = "auto";
          ControlPersist = "yes";
          preferredAuthentications = "publickey";
        };
        description = "Extra SSH options.";
      };
    };
  };
  userModule = types.submodule {
    options = {
      login = mkOption {
        type = types.str;
        default = "";
        description = "Forge login.";
      };
      password = mkOption {
        type = types.str;
        default = "";
        description = "Forge password.";
      };
    };
  };
  forgeModule = types.submodule {
    options = {
      type = mkOption {
        type = types.enum [ "github" "gitlab" "bitbucket" ]; # and maybe something else
        default = "github";
        description = "Forge type.";
      };
      workspaceRoot = mkOption {
        type = types.str;
        default = "";
        description = "Local workspace root for this forge.";
      };
      credentials = mkOption {
        type = userModule;
        description = "Forge login/password.";
      };
      appToken = mkOption {
        type = types.str;
        default = "";
        description = "Forge application token.";
      };
      ssh.keypair = mkOption {
        type = sshModule;
        description = "SSH keypair.";
      };
      ssh.matchBlock = mkOption {
        type = matchBlockModule;
        description = "SSH match block.";
      };
      passCredentialsMap = mkOption {
        type = types.attrsOf types.attrs;
        default = { };
        description = "Pass credentials map.";
      };
      extraConfig = mkOption {
        type = types.attrsOf types.attrs;
        default = { };
        description = "Extra configuration for setting under `programs.git.extraConfig`.";
      };
    };
  };
in
{
  options = {
    dev.git.forges = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      forges = mkOption {
        type = types.attrsOf forgeModule;
        default = { };
        description = "Forges definitions.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs forges support.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable (
      let
        credentials = collectPassCredentials cfg.forges;
        matchBlocks = collectMatchBlocks cfg.forges;
        extraConfig = collectExtraConfig cfg.forges;
        workspaceRoots = collectWorkspaceRoots cfg.forges;
      in
      {
        assertions = [{
          assertion = config.workstation.systemtraits.enable;
          message = "git/forges: must enable systemtraits maintainence.";
        }];

        home-manager.users."${user}" = {
          xdg.configFile."pass-git-helper/git-pass-mapping.ini".text =
            generators.toINI { } credentials;
          programs.ssh.matchBlocks =
            optionalAttrs (matchBlocks != { }) matchBlocks;
          programs.git.extraConfig =
            optionalAttrs (extraConfig != { }) extraConfig;
        };
        navigation.bookmarks.workspaces.roots = workspaceRoots;
        workstation.systemtraits.instructions =
          optionalString (credentials != { }) ''
            ${pkgs.redis}/bin/redis-cli set git/credentials_mapping ${
              strings.escapeNixString (builtins.toJSON credentials)
            }
          '';
      }
    ))
    (mkIf (cfg.enable && cfg.emacs.enable) (
      let
        mapping = getUrlsToTypesMapping cfg.forges;
      in
      {
        ide.emacs.core.extraPackages = epkgs: [
          epkgs.browse-at-remote
          epkgs.forge
          epkgs.git-link
        ];
        ide.emacs.core.config =
          builtins.readFile ./emacs/forges.el +
          optionalString (length (attrValues mapping) > 0)
            ((genBrowseAtRemoteTypesPatch mapping) + (genGitlinkTypesPatch mapping));
      }
    ))
  ];
}
