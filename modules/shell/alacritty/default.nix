{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.vt.alacritty;
  user = config.attributes.mainUser.name;
  terminalCmd = [ "${pkgs.alacritty}/bin/alacritty" "-e" ];
  prefix = config.wmCommon.prefix;
in
{
  options = {
    shell.vt.alacritty = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Alacritty.";
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
      fonts = { fonts = with pkgs; [ powerline-fonts ]; };

      pim.timetracking.rules = mkArbttProgramTitleRule [ "Alacritty" ]
        [ "(?:~|home/${user})/workspace/repos/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/" ] "project:$1-$2-$3";

      shell.core.variables = [
        { TERMINAL = builtins.head terminalCmd; global = true; }
        { TB_TERMINAL_CMD = terminalCmd; }
      ];
      attributes.defaultVTCommand = terminalCmd;
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
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("Alacritty"), {
            K("C-x"): {
                K("k"): K("C-d"),
            },
        }, "Alacritty")
      '';
      wmCommon.autostart.entries = optionals (cfg.autostart) [ (builtins.head terminalCmd) ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ prefix "Shift" "Return" ];
        cmd = builtins.head terminalCmd;
        mode = "root";
      }];
    })
  ];
}
