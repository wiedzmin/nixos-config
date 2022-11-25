{ config, inputs, lib, pkgs, ... }:
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
      autostart = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to autostart Alacritty with X session start";
      };
      command = mkOption {
        type = types.listOf types.str;
        default = [ "${pkgs.alacritty}/bin/alacritty" "-e" ];
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "Alacritty" "Alacritty" ];
        visible = false;
        internal = true;
        description = "Alacritty default window class.";
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
        { TERMINAL = builtins.head cfg.command; global = true; }
        { TB_TERMINAL_CMD = cfg.command; }
      ];
      attributes.vt.default.cmd = cfg.command;
      attributes.vt.default.windowClass = cfg.windowClass;
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
      wmCommon.autostart.entries = optionals cfg.autostart [ { cmd = builtins.head cfg.command; } ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [{
        key = [ prefix "Shift" "Return" ];
        cmd = builtins.head cfg.command;
        mode = "root";
      }];
    })
  ];
}
