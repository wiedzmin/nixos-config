{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.autofetch;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    dev.git.autofetch = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable batch updates fetching.";
      };
      defaultUpstreamRemote = mkOption {
        type = types.str;
        default = "upstream";
        description = "Name of upstream repo remote.";
      };
      defaultOriginRemote = mkOption {
        type = types.str;
        default = "origin";
        description = "Name of origin repo remote.";
      };
      mainBranchName = mkOption {
        type = types.str;
        default = "master";
        description = "Subj.";
      };
      when = mkOption {
        type = types.str;
        default = "";
        description = "When to fetch updates (on calendar).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.dev.batchvcs.enable;
          message = "git: automatic updates fetching requires batch VCS setup to be enabled.";
        }
        {
          assertion = cfg.enable && cfg.when != "";
          message = "git: automatic updates fetching is enabled while not scheduled.";
        }
      ];

      nixpkgs.config.packageOverrides = _: rec {
        gitfetch = mkPythonScriptWithDeps pkgs "gitfetch"
          (with pkgs; [ nurpkgs.pyfzf nurpkgs.pystdlib python3Packages.pygit2 python3Packages.redis ])
          (builtins.readFile ./scripts/gitfetch.py);
      };

      # IDEA: place empty ".noauto" or like file in repo root to prevent any further breakage if something went wrong
      dev.batchvcs.commands = {
        mu = [ # merge upstream
          "${pkgs.gitctl}/bin/gitfetch --op fetch --remote ${cfg.defaultOriginRemote}"
          "${pkgs.gitctl}/bin/gitfetch --op merge ${cfg.mainBranchName}"
        ];
        ru = [ # rebase upstream
          "${pkgs.gitfetch}/bin/gitfetch --op fetch"
          "${pkgs.gitfetch}/bin/gitfetch --op rebase"
        ];
      };

      systemd.user.services."git-fetch-updates" = {
        description = "Fetch updates from registered git upstream(s)";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.mr}/bin/mr mu";
          WorkingDirectory = homePrefix user "";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."git-fetch-updates" =
        renderTimer "Fetch updates from registered git upstream(s)" "1m" "2m" cfg.fetchUpdates.when false "";
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ gitfetch ]; };
    })
  ];
}
