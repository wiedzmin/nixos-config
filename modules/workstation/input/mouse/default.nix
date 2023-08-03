{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.input.mouse;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    workstation.input.mouse = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable various auxiliary tools
          for controlling mouse input
        '';
      };
      keynavTool = mkOption {
        type = types.enum [ "keynav" "warpd" ];
        default = "keynav";
        description = "Tool to use for navigation mouse with keyboard";
      };
      constraintMouse.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to constraint mouse within xrandr screen(s).";
      };
      constraintMouse.top = mkOption {
        type = types.int;
        default = 0;
        description = "Top margin size.";
      };
      constraintMouse.left = mkOption {
        type = types.int;
        default = 0;
        description = "Left margin size.";
      };
      constraintMouse.right = mkOption {
        type = types.int;
        default = 0;
        description = "Right margin size.";
      };
      constraintMouse.bottom = mkOption {
        type = types.int;
        default = 0;
        description = "Bottom margin size.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = optionalAttrs (cfg.keynavTool == "warpd") {
        home.packages = with pkgs; [
          warpd
          xsel
        ];
        xdg.configFile = {
          "warpd/config".text = ''
            hint_font: Iosevka

            left: Left
            down: Down
            up: Up
            right: Right

            grid_up: C-Up
            grid_left: C-Left
            grid_down: C-Down
            grid_right: C-Right

            top: C-S-Up
            middle: C-S-Right
            bottom: C-S-Down
            start: C-0
            end: C-$

            left: Left
            down: Down
            up: Up
            right: Right

            screen_chars: qweasdzxc
          '';
        };
      };
      wmCommon.keybindings.common = optionals (cfg.keynavTool == "warpd") [
        {
          key = [ prefix "Mod1" "x" ];
          cmd = "warpd --hint";
          mode = "root";
        }
        {
          key = [ prefix "Mod1" "c" ];
          cmd = "warpd --normal";
          mode = "root";
        }
        {
          key = [ prefix "Mod1" "g" ];
          cmd = "warpd --grid";
          mode = "root";
        }
      ];

      systemd.user.services = optionalAttrs (cfg.keynavTool == "keynav") {
        "keynav" = (
          let
            # FIXME: put keybinding at Nix level
            keynavConfig = pkgs.writeText "keynav.conf" ''
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
            '';
          in
          {
            description = "Navigate mouse with keyboard";
            after = [ "graphical-session-pre.target" ];
            partOf = [ "graphical-session.target" ];
            wantedBy = [ "graphical-session.target" ];
            serviceConfig = {
              Type = "simple";
              ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
              ExecStart = ''${pkgs.keynav}/bin/keynav "loadconfig ${keynavConfig}"'';
            };
          }
        );
      };
      services.xbanish.enable = true;
    })
    (mkIf (cfg.keynavTool == "warpd" && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/mouse.yml".source = yaml.generate "espanso-mouse.yml" {
          matches = [
            {
              trigger = ":wdop";
              replace = "warpd --list-options | xsel -ib";
            }
          ];
        };
      };
    })
    (mkIf cfg.constraintMouse.enable {
      systemd.user.services."xpointerbarrier" = {
        description = "Create pointer barriers around each XRandR screen";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
          ExecStart = ''
            ${pkgs.xpointerbarrier}/bin/xpointerbarrier ${builtins.toString cfg.constraintMouse.top} \
                                                        ${builtins.toString cfg.constraintMouse.left} \
                                                        ${builtins.toString cfg.constraintMouse.right} \
                                                        ${builtins.toString cfg.constraintMouse.bottom}
          '';
        };
      };
    })
  ];
}
