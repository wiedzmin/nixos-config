{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    dev.git.misc = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git miscellaneous setup.";
      };
      defaultUpstreamRemote = mkOption {
        type = types.str;
        default = "upstream";
        description = "Name of upstream repo remote.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs-related setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        gittags = mkPythonScriptWithDeps "gittags"
          (with pkgs; [ nurpkgs.pyfzf nurpkgs.pystdlib python3Packages.pygit2 python3Packages.redis ])
          (readSubstituted ../../../subst.nix ./scripts/gittags.py);
      };

      # TODO: make expansion for `git log --pretty=oneline --pickaxe-regex -Stoken` (commits contents)
      # TODO: make expansion for `git log --pretty=oneline --pickaxe-regex -Gtoken` (commits differences)

      home-manager.users.${user} = { home.packages = with pkgs; [ gitleaks ]; };

      dev.git.batch.commands = {
        synctags = [ "${pkgs.gittags}/bin/gittags --sync" ];
        usynctags = [ "${pkgs.gittags}/bin/gittags --sync --remote ${cfg.defaultUpstreamRemote}" ];
        trim = [ "${pkgs.gitAndTools.git-trim}/bin/git-trim --delete=merged-local" ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.diff-hl
        epkgs.git-commit
        epkgs.git-msg-prefix
      ];
      ide.emacs.core.config = readSubstituted ../../../subst.nix ./emacs/misc.el;
    })
  ];
}
