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
        description = "Whether to enable Git VCS infrastructure.";
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

      dev.git.batch.commands = {
        synctags = [ "${pkgs.gittags}/bin/gittags --sync" ];
        usynctags = [ "${pkgs.gittags}/bin/gittags --sync --remote ${cfg.defaultUpstreamRemote}" ];
        trim = [ "${pkgs.gitAndTools.git-trim}/bin/git-trim --delete=merged-local" ];
      };
    })
  ];
}
