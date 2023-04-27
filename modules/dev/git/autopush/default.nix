{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.git.autopush;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    dev.git.autopush = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Git VCS infrastructure.";
      };
      when = mkOption {
        type = types.str;
        default = "";
        description = "When to push updates (on calendar).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.dev.batchvcs.enable;
          message = "git: automatic updates pushing requires batch VCS setup to be enabled.";
        }
        {
          assertion = cfg.enable && cfg.when != "";
          message = "git: automatic updates pushing is enabled while not scheduled.";
        }
      ];

      nixpkgs.config.packageOverrides = _: {
        gitpush = mkPythonScriptWithDeps pkgs "gitpush"
          (with pkgs; [ python3Packages.pyfzf nurpkgs.pystdlib python3Packages.pygit2 python3Packages.redis ])
          (builtins.readFile ./scripts/gitpush.py);
      };

      dev.batchvcs.commands = { push = [ "${pkgs.gitpush}/bin/gitpush" ]; };

      systemd.services."git-push-updates" = {
        description = "Push updates to registered git upstream(s)";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.mr}/bin/mr push";
          WorkingDirectory = homePrefix user "";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.timers."git-push-updates" =
        renderTimer "Push updates to registered git upstream(s)" "10m" "15m" cfg.pushUpdates.when false "";
    })
  ];
}
