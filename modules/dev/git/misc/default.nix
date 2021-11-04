{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.misc;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
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
          (builtins.readFile ./scripts/gittags.py);
      };

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gitleaks ];
        xdg.configFile."espanso/user/git.yml".text = ''
          name: git
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":glcont"
              replace: "git log --pretty=oneline --pickaxe-regex -S$|$"

            - trigger: ":gpcont"
              replace: "git log -p --all -S '$|$'"

            - trigger: ":gldiff"
              replace: "git log --pretty=oneline --pickaxe-regex -G$|$"

            - trigger: ":gpdiff"
              replace: "git log -p --all -G '$|$'"

            - trigger: ":bdiff"
              replace: "git diff ${config.dev.git.autofetch.mainBranchName} $|$ > ../master-${config.dev.git.autofetch.mainBranchName}.patch"

            - trigger: ":tbcont"
              replace: "git log --branches -S'$|$' --oneline | awk '{print $1}' | xargs git branch -a --contains"

            - trigger: ":trec"
              replace: "git log -S$|$ --since=HEAD~50 --until=HEAD"
        '';
      };

      dev.git.batch.commands = {
        synctags = [ "${pkgs.gittags}/bin/gittags --sync" ];
        usynctags = [ "${pkgs.gittags}/bin/gittags --sync --remote ${cfg.defaultUpstreamRemote}" ];
        trim = [ "${pkgs.gitAndTools.git-trim}/bin/git-trim --delete=merged-local" ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.diff-hl epkgs.git-commit epkgs.git-msg-prefix ];
      ide.emacs.core.config = builtins.readFile ./emacs/misc.el;
    })
  ];
}
