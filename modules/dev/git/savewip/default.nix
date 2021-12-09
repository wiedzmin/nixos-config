{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.dev.git.savewip;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    dev.git.savewip = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable work-in-progress saving.";
      };
      minChangedLines = mkOption {
        type = types.int;
        default = 100;
        description = "Changed WIP LOC count to consider pushing upstream.";
      };
      when = mkOption {
        type = types.str;
        default = "";
        description = "When to save work-in-progress (on calendar).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.dev.batchvcs.enable;
          message = "git: automatic WIP saving requires batch VCS setup to be enabled.";
        }
        {
          assertion = cfg.enable && cfg.when != "";
          message = "git: automatic WIP saving is enabled while not scheduled.";
        }
      ];

      nixpkgs.config.packageOverrides = _: rec {
        gitwip = mkPythonScriptWithDeps "gitwip"
          (with pkgs; [ nurpkgs.pyfzf nurpkgs.pystdlib python3Packages.pygit2 python3Packages.redis xprintidle-ng ])
          (builtins.readFile ./scripts/gitwip.py);
      };

      dev.batchvcs.commands = { savewip = [ "${pkgs.gitwip}/bin/gitwip" ]; };

      systemd.user.services."git-save-wip" = {
        description = "Save work-in-progress in registered git repo(s)";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = ''${pkgs.bash}/bin/bash -c "${pkgs.mr}/bin/mr savewip"'';
          WorkingDirectory = homePrefix user "";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."git-save-wip" =
        renderTimer "Save work-in-progress in registered git repo(s)" "2m" "3m" cfg.saveWip.when false "";
    })
  ];
}
