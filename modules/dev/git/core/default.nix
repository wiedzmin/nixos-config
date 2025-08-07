{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.core;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  inherit (hm.xdg) dataHome;
in
{
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
      pager = mkOption {
        type = types.enum [ "delta" "diff-so-fancy" "riff" ];
        default = "delta";
        description = "Pager tool to use";
      };
      assets.dirName = mkOption {
        type = types.str;
        default = "git-assets";
        description = "Assets(templates, etc.) home subdir.";
      };
      mergiraf.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Mergiraf merge tool.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs git-related setup.";
      };
      emacs.delta.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable `delta` within magit status buffer";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        xdg.dataFile."${cfg.assets.dirName}/.gitignore".text = cfg.gitignore;
        programs = {
          git = {
            enable = true;
            delta = {
              enable = (cfg.pager == "delta");
              options = {
                dark = true;
                diff-so-fancy = true; # NOTE: emulation mode
                features = "decorations";
                highlight-removed = true;
                hyperlinks = true;
                hyperlinks-file-link-format = "file://{path}:{line}";
                line-numbers = true;
                minus-color = "#ad3436";
                navigate = true;
                plus-color = "#34ad3a";
                side-by-side = true;
                theme = "zenburn";
                whitespace-error-style = "22 reverse";
                decorations = {
                  commit-decoration-style = "bold yellow box ul";
                  file-style = "bold yellow ul";
                  file-decoration-style = "none";
                };
              };
            };
            diff-so-fancy.enable = (cfg.pager == "diff-so-fancy");
            riff.enable = (cfg.pager == "riff");
            extraConfig = {
              "core" = {
                compression = 0;
              };
              "user" = {
                inherit (config.attributes.mainUser) email;
                name = config.attributes.mainUser.fullName;
                signingKey = config.attributes.mainUser.gpgKeyID;
              };
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
              "pack" = { window = 1; };
            };
          };
          mergiraf.enable = true;
        };
      };

      shell.prompts.starship.modulesConfiguration = {
        git_branch = { only_attached = true; };
        git_commit = {
          commit_hash_length = 10;
          tag_disabled = false;
          tag_symbol = "🔖 ";
        };
        git_status = {
          diverged = "⇕⇡$ahead_count⇣$behind_count";
          conflicted = "✗ ($count) ";
          ahead = "⇡ ($count) ";
          behind = "⇣ ($count) ";
          untracked = "♾ ‍($count) ";
          stashed = "☂ ($count) ";
          modified = "♨ ($count) ";
          staged = "[++($count)](green) ";
          renamed = "⥂ ($count) ";
          deleted = "♺ ($count) ";
          style = "bold green";
        };
      };

      attributes.gitPager.cmd = optionalString (cfg.pager == "delta") "${pkgs.delta}/bin/delta"
        + optionalString (cfg.pager == "diff-so-fancy") "${pkgs.diff-so-fancy}/bin/diff-so-fancy"
        + optionalString (cfg.pager == "riff") "${pkgs.riffdiff}/bin/riff";
      shell.core.variables = [{ GIT_PAGER = config.attributes.gitPager.cmd; }];
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        git-core = {
          matches = [
            {
              trigger = ":gitB";
              replace = "git branch -a | fzf | tr -d \"[:blank:]\" | tr -d '\\n' | xsel -ib";
            }
            {
              trigger = ":gs";
              replace = "git show {{sha}}";
              vars = [
                {
                  name = "sha";
                  type = "clipboard";
                }
              ];
            }
          ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.blamer
        epkgs.git-modes
        epkgs.magit
        epkgs.magit-commit-mark
        epkgs.magit-filenotify
        epkgs.magit-popup # *
      ] ++ optionals cfg.emacs.delta.enable epkgs.magit-delta;
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst/core.nix ] [ ./elisp/core.el ]
        + optionalString cfg.emacs.delta.enable ''
        (use-package magit-delta
          :disabled
          :hook (magit-mode-hook . magit-delta-mode))
      '';
      ide.emacs.core.customKeymaps = { "custom-git-map" = "C-'"; };
    })
  ];
}

# * it seems some magit-dependent packages yet depend on magit-popup in some path, so we introduced
#   this explicit dependency and will keep it until transition to "transient" library is fully done
#   by all affected packages. (or some other root cause of "magit-popup"" will pop up)

# think of automation (maybe using existing tools) for providing branch name for Jira-managed (and
# maybe not only) projects
