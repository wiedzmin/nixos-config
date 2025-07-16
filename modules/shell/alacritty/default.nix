{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.vt.alacritty;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
in
{
  options = {
    shell.vt.alacritty = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Alacritty.";
      };
      traits = mkOption {
        type = types.submodule (import ../../workstation/systemtraits/xapp-traits.nix);
        description = "Alacritty application traits";
      };
      autostart = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to autostart Alacritty with X session start";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      shell.vt.alacritty.traits = rec {
        command = {
          binary = "${pkgs.alacritty}/bin/alacritty";
          parameters = [ "-e" ];
        };
        wmClass = [ "Alacritty" "Alacritty" ];
      };

      fonts = { packages = with pkgs; [ powerline-fonts ]; };

      pim.timetracking.rules = mkArbttProgramTitleRule [ (appWindowClass cfg.traits) ]
        [ "(?:~|home/${user})/workspace/repos/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/" ] "project:$1-$2-$3";

      shell.core.variables = [
        { TERMINAL = cfg.traits.command.binary; global = true; }
        { TB_TERMINAL_CMD = cfg.traits.command.binary; global = true; }
      ];
      attributes.vt.default.traits = cfg.traits;
      home-manager.users."${user}" = {
        programs.alacritty = {
          enable = true;
          settings = {
            env = {
              LESS = "-SRXF";
              TERM = "xterm-256color";
            };
            window = {
              padding = {
                x = 2;
                y = 2;
              };
              decorations = "full";
              dynamic_title = true;
            };
            draw_bold_text_with_bright_colors = true;
            bell = {
              animation = "EaseOutExpo";
              duration = 0;
            };
            mouse_bindings = [{
              mouse = "Middle";
              action = "PasteSelection";
            }];
            selection = { semantic_escape_chars = '',│`|:"' ()[]{}<>''; };
            cursor = { style = "Beam"; };
            live_config_reload = true;
          };
        };
      };
      workstation.input.keyboard.xremap.config = {
        keymap = [
          {
            name = "${appName cfg.traits}";
            application = { only = "${appWindowClass cfg.traits}"; };
            remap = {
              "C-x" = {
                remap = {
                  "k" = "C-d";
                };
              };
            };
          }
        ];
      };
      workstation.input.keyboard.xkeysnail.rc = ''
        define_keymap(re.compile("${appWindowClass cfg.traits}"), {
            K("C-x"): {
                K("k"): K("C-d"),
            },
        }, "${appName cfg.traits}")
      '';
      wmCommon.autostart.entries = optionals cfg.autostart [{ cmd = cfg.traits.command.binary; }];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [{
        key = [ prefix "Shift" "Return" ];
        cmd = cfg.traits.command.binary;
        mode = "root";
      }];
    })
  ];
}
