{ config, lib, pkgs, ... }:
with lib;

let cfg = config.services.keynav;
in {
  options = {
    services.keynav = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = ''
          Whether to enable keynav.
        '';
      };
      # TODO: add config-related options/overrides
    };
  };

  config = mkIf cfg.enable {
    systemd.services."keynav" = let
      keynavConfig = pkgs.writeText "keynav.conf" (''
        clear
        grid-nav on
        ctrl+semicolon start
        Escape end
        ctrl+bracketleft end
        q record ~/.keynav_macros
        shift+at playback
        a history-back
        Left cut-left
        Down cut-down
        Up cut-up
        Right cut-right
        shift+Right move-right
        shift+Left move-left
        shift+Down move-down
        shift+Up move-up
        space warp
        Return warp,click 1,end
        semicolon warp,end
        w warp
        t windowzoom
        c cursorzoom 300 300
        e end
        1 click 1
        2 click 2
        3 click 3
        ctrl+h cut-left
        ctrl+j cut-down
        ctrl+k cut-up
        ctrl+l cut-right
        y cut-left,cut-up
        u cut-right,cut-up
        b cut-left,cut-down
        n cut-right,cut-down
        shift+y move-left,move-up
        shift+u move-right,move-up
        shift+b move-left,move-down
        shift+n move-right,move-down
        ctrl+y cut-left,cut-up
        ctrl+u cut-right,cut-up
        ctrl+b cut-left,cut-down
        ctrl+n cut-right,cut-down
      '');
    in {
      description = "Navigate mouse with keyboard";
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        PassEnvironment = "DISPLAY";
        ExecStart = ''${pkgs.keynav}/bin/keynav "loadconfig ${keynavConfig}"'';
      };
    };
  };
}
