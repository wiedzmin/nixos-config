{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.vt.kitty;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix;
in
{
  options = {
    shell.vt.kitty = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Kitty.";
      };
      autostart = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to autostart Kitty with X session start";
      };
      command = mkOption {
        type = types.listOf types.str;
        default = [ "${pkgs.kitty}/bin/kitty" "-e" ];
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "kitty" "kitty" ];
        visible = false;
        internal = true;
        description = "Kitty default window class.";
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
      assertions = [
        {
          assertion = cfg.enable && !config.shell.tmux.enable;
          message = "Kitty and Tmux have a fair functionality overlap, so it makes no sense to be enabled simultaneously.";
        }
        {
          assertion = cfg.enable && !config.shell.vt.alacritty.enable;
          message = "Only one VT program should be enabled at the same time";
        }
      ];
      fonts = { fonts = with pkgs; [ powerline-fonts ]; };

      pim.timetracking.rules = mkArbttProgramTitleRule [ "kitty" ]
        [ "(?:~|home/${user})/workspace/repos/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/" ] "project:$1-$2-$3";

      shell.core.variables = [
        { TERMINAL = builtins.head cfg.command; global = true; }
        { TB_TERMINAL_CMD = cfg.command; }
      ];
      attributes.vt.default.cmd = cfg.command;
      attributes.vt.default.windowClass = cfg.windowClass;
      home-manager.users."${user}" = {
        programs.kitty = {
          enable = true;
          theme = "zenburned"; # FIXME: move under `appearance`
          settings = {
            # FIXME: === move under appearance ===
            # If Regular is not distinguishable, use Medium
            font_family = "Iosevka Term Light";
            bold_font = "Iosevka Term Regular";
            italic_font = "Iosevka Term Light Italic";
            bold_italic_font = "Iosevka Term Regular Italic";
            font_size = "10.0";
            # ====================================
            cursor_shape = "beam";
            active_border_color = "red";
            window_border_width = "1.5pt";
            visual_window_select_characters = "qweasdzxc";
            enabled_layouts = "splits:split_axis=horizontal,grid,tall,fat,stack";
          };
          keybindings = {
            # FIXME: investigate why "ctrl+x..." bindings do not work, using "alt+x" until then
            # windows
            "f2>n" = "new_window";
            "alt+x>n" = "new_window";
            "f2>k" = "close_window";
            "alt+x>k" = "close_window";
          } // {
            # fonts
            "ctrl+minus" = "change_font_size all -0.5";
            "ctrl+equal" = "change_font_size all +0.5";
          } // {
            # navigation
            "f2>w" = "focus_visible_window";
            "alt+tab" = "focus_visible_window";
            "ctrl+left" = "neighboring_window left";
            "ctrl+right" = "neighboring_window right";
            "ctrl+up" = "neighboring_window up";
            "ctrl+down" = "neighboring_window down";
            "ctrl+shift+left" = "move_window left";
            "ctrl+shift+right" = "move_window right";
            "ctrl+shift+up" = "move_window up";
            "ctrl+shift+down" = "move_window down";
          } // {
            # layouts
            "alt+x>2" = "launch --location=hsplit";
            "alt+x>minus" = "launch --location=hsplit";
            "alt+x>3" = "launch --location=vsplit";
            "alt+x>backslash" = "launch --location=vsplit";
            "ctrl+l>s" = "goto_layout splits:split_axis=horizontal";
            "ctrl+l>g" = "goto_layout grid";
            "ctrl+l>t" = "goto_layout tall";
            "ctrl+l>f" = "goto_layout fat";
            "ctrl+z" = "toggle_layout stack";
            "ctrl+0x2e" = "toggle_layout stack";
          } // {
            # editing
            "ctrl+y" = "paste_from_clipboard";
            "alt+s>alt+s" = "kitten hints --type word --alphabet qweasdzxc --program @";
            "alt+s>alt+p" = "kitten hints --type path --alphabet qweasdzxc --program @";
            "alt+s>alt+r" = "kitten hints --type regex --alphabet qweasdzxc --program @";
            "alt+s>alt+l" = "kitten hints --type line --alphabet qweasdzxc --program @";
            "alt+s>alt+u" = "kitten hints --type url --alphabet qweasdzxc --program @";
            "alt+s>alt+h" = "kitten hints --type hash --alphabet qweasdzxc --program @";
            "alt+s>alt+k" = "kitten hints --type hyperlink --alphabet qweasdzxc --program @";
            "alt+s>alt+i" = "kitten hints --type ip --alphabet qweasdzxc --program @";
            "alt+s>alt+n" = "kitten hints --type linenum --alphabet qweasdzxc --linenum-action background --program emacsclient";
          } // {
            # misc
            "ctrl+c" = "signal_child SIGTERM";
          };
        };
      };
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("${lib.last cfg.windowClass}"), {
            K("C-x"): K("M-x"),
        }, "kitty")
      ''; # NOTE: workaround for unexpectedly non-working direct `C-x` prefix
      wmCommon.autostart.entries = optionals cfg.autostart [ (builtins.head cfg.command) ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ prefix "Shift" "Return" ];
        cmd = builtins.head cfg.command;
        mode = "root";
      }];
    })
  ];
}
