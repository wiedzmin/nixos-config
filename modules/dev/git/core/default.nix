{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.core;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  dataHome = hm.xdg.dataHome;
in {
  options = {
    dev.git.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      gitignore = mkOption {
        type = types.lines;
        default = "";
        visible = false;
        internal = true;
        description = "Global gitignore contents.";
      };
      assets.dirName = mkOption {
        type = types.str;
        default = "git-assets";
        description = "Assets(templates, etc.) home subdir.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs git-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        xdg.dataFile."${cfg.assets.dirName}/.gitignore".text = cfg.gitignore;
        programs.git = {
          enable = true;
          delta = {
            enable = true;
            options = {
              features = "decorations";
              theme = "zenburn";
              diff-so-fancy = true;
              dark = true;
              highlight-removed = true;
              plus-color = "#34ad3a";
              minus-color = "#ad3436";
              whitespace-error-style = "22 reverse";
              decorations = {
                commit-decoration-style = "bold yellow box ul";
                file-style = "bold yellow ul";
                file-decoration-style = "none";
              };
            };
          };
          extraConfig = {
            "rebase" = {
              autoSquash = true;
              autoStash = true;
            };
            "core" = {
              autocrlf = false;
              excludesfile = "${dataHome}/${cfg.assets.dirName}/.gitignore";
              quotepath = false;
              askPass = "";
            };
            "credential" = { helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper"; };
            "diff" = {
              algorithm = "patience";
              gpg = { textconv = "${pkgs.gnupg}/bin/gpg2 --no-tty --decrypt"; };
            };
            "push" = { default = "current"; };
            "absorb" = { maxstack = 75; }; # TODO: package https://github.com/torbiak/git-autofixup
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs: [
        epkgs.magit
        epkgs.magit-filenotify
        epkgs.magit-popup # *
      ];
      ide.emacs.config = readSubstituted ../../../subst.nix ./emacs/core.el;
    })
  ];
}

# * it seems some magit-dependent packages yet depend on magit-popup in some path, so we introduced
#   this explicit dependency and will keep it until transition to "transient" library is fully done
#   by all affected packages. (or some other root cause of "magit-popup"" will pop up)

# think of automation (maybe using existing tools) for providing branch name for Jira-managed (and
# maybe not only) projects
