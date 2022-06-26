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
        xdg.configFile = {
          "espanso/user/kitty.yml".text = ''
            name: kitty
            parent: default

            matches:
              - trigger: ":ksk"
                replace: "kitty +kitten show_key"

              - trigger: ":ksmk"
                replace: "kitty +kitten show_key -m kitty"
          '';
        };
        programs.kitty = {
          enable = true;
          settings = {
            # appearance
            cursor_shape = "beam";
            active_border_color = "red";
            window_border_width = "1.0pt";
            draw_minimal_borders = "no";
            tab_bar_edge = "top";
            tab_bar_background = "none";
            tab_bar_style = "powerline";
            tab_title_template = "{fmt.fg.c2c2c2}{title}";
            active_tab_title_template = "{fmt.fg._fff}{title}";
            active_tab_font_style = "bold-italic";
            url_style = "curly";

            enabled_layouts = concatStringsSep "," [
              "splits:split_axis=horizontal"
              "grid"
              "tall"
              "fat"
              "horizontal"
              "vertical"
              "stack"
            ];
          } // {
            # navigation
            visual_window_select_characters = "qweasdzxc";
            scrollback_lines = "10000"; # TODO: make module option
          } // {
            # resize
            remember_window_size = "yes";
            window_resize_step_cells = "2";
            window_resize_step_lines = "2";
          };
          keybindings = {
            # FIXME: investigate why "ctrl+x..." bindings do not work, using "alt+x" until then
            # windows
            "f2>k" = "close_window";
            "alt+x>k" = "close_window";
            "f2>d" = "detach_window";
            "f2>shift+d" = "detach_window ask";
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

            "alt+shift+left_bracket" = "scroll_home";
            "alt+shift+right_bracket" = "scroll_end";
            "alt+shift+page_down" = "scroll_page_down";
            "alt+shift+page_up" = "scroll_page_up";

            "alt+x>page_up" = "show_last_command_output";
          } // {
            # resize
            "alt+shift+left" = "resize_window narrower";
            "alt+shift+right" = "resize_window wider";
            "alt+shift+down" = "resize_window taller";
            "alt+shift+up" = "resize_window shorter 3";
            "alt+shift+delete" = "resize_window reset";
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
            "ctrl+l>h" = "goto_layout horizontal";
            "ctrl+l>v" = "goto_layout vertical";
            "ctrl+z" = "toggle_layout stack";
            "ctrl+0x2e" = "toggle_layout stack";
          } // {
            # editing
            "ctrl+0x76" = "paste_from_clipboard";
            "ctrl+0x78" = "copy_to_clipboard";
            "home" = "send_text all \\x1b\\x62"; # C-a
            "end" = "send_text all \\x1b\\x66"; # C-e
            "ctrl+alt+slash>enter" = "create_marker";
            "ctrl+alt+slash>escape" = "remove_marker";
            "alt+s>p" = "scroll_to_mark prev";
            "alt+s>n" = "scroll_to_mark next";
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
            "ctrl+c" = "copy_or_interrupt";
          };
        };
      };
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("${lib.last cfg.windowClass}"), {
            K("C-x"): K("M-x"),
            K("M-Shift-comma"): K("M-Shift-LEFT_BRACE"),
            K("M-Shift-dot"): K("M-Shift-RIGHT_BRACE"),
        }, "kitty")
      ''; # NOTE: workarounds for some unexpectedly non-working prefixes
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
