{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# [[file:todo.org::*get opts/mappings list from https://sw.kovidgoyal.net/kitty/][get opts/mappings list from https://sw.kovidgoyal.net/kitty/]]

# some hex ctrl-prefixed keycodes:
# \x09 --> tab
# \x15 --> ^U
# \x12 --> ^R
# \x0c --> ^L
# \x01 --> ^A
# \x05 --> ^E
# \x04 --> ^D

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
        description = "Whether to enable Kitty. Docs at https://sw.kovidgoyal.net/kitty/";
      };
      autostart = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to autostart Kitty with X session start";
      };
      command = mkOption {
        type = types.listOf types.str;
        default = [ "${pkgs.kitty}/bin/kitty" "-c" ];
        description = "Default command line to invoke";
      };
      scrollbackSize = mkOption {
        type = types.int;
        default = 100000;
        description = "Scrollback buffer size";
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

      navigation.bookmarks.entries = {
        kitty-docs = {
          desc = "Kitty VT documentation";
          remote = {
            url = "https://sw.kovidgoyal.net/kitty/";
            jump = true;
            searchSuffix = "search/?q=";
          };
        };
      };

      home-manager.users."${user}" = {
        xdg.configFile = {
          # TODO: set window class of EDITOR/emacsclient opened by `hyperlinked_grep` to prevent moving window somewhere
          "kitty/open-actions.conf".text = ''
            # Open any file with a fragment in vim, fragments are generated
            # by the hyperlink_grep kitten and nothing else so far.
            protocol file
            fragment_matches [0-9]+
            action launch --type=overlay $EDITOR +''${FRAGMENT} ''${FILE_PATH}

            # Open text files without fragments in the editor
            protocol file
            mime text/*
            action launch --type=overlay $EDITOR ''${FILE_PATH}

            protocol file
            ext csv
            action launch --type=overlay ${pkgs.visidata}/bin/vd ''${FILE_PATH}

            protocol file
            mime inode/directory
            action launch --location hsplit ''${MYFEXP} ''${FILE_PATH}

            # Open any image in the full kitty window by clicking on it
            protocol file
            mime image/*
            action launch --type=overlay kitty +kitten icat --hold ''${FILE_PATH}

            protocol file
            file *.*
            action launch --type=overlay $EDITOR ''${FILE_PATH}
          '';
          "espanso/user/kitty.yml".text = ''
            name: kitty
            parent: default

            matches:
              - trigger: ":ksk"
                replace: "kitty +kitten show_key"

              - trigger: ":ksmk"
                replace: "kitty +kitten show_key -m kitty"

              - trigger: ":khg"
                replace: "kitty +kitten hyperlinked_grep $|$"

              - trigger: ":ksa" # NOTE: temporary workaround for some strangely behaving (in terms of input) ssh sessions (no cause revealed yet)
                replace: "nix shell \"nixpkgs#sakura\" -c sakura"
          '';
          "kitty/grab" = {
            source = pkgs.kitty_grab;
            recursive = true;
          };
          "kitty/search" = {
            source = pkgs.kitty_search;
            recursive = true;
          };
        };
        # TODO: kitten hint for dealing with sha256
        programs.kitty = {
          enable = true;
          # TODO: play with opacity (settings, mappable actions, etc.)
          # TODO: play with scrolling functionality
          # TODO: bind tabs-related actions

          # TODO: review escape sequences ============
          # \x1b[2;5~ # same esc-seq as C-Insert # kitty_mod+c
          # \x1b[2;2~ # same esc-seq as S-Insert # kitty_mod+v
          # \x1b[2;3~
          # \x63
          # \x1bB # alt+left
          # \x1bF # alt+right
          # \x1b[1;5H
          # \x1b[1;5F
          # \x1b[1;3H
          # \x1b[1;3F
          # \x1b[1;5A
          # \x1b[1;5B
          # \x1b[1;5D
          # \x1b[1;5C
          # \x1b[1;2A
          # \x1b[1;2B
          # \x1b[1;2D
          # \x1b[1;2C
          # \x1b[1;6A
          # \x1b[1;6B
          # \x1b[1;6D
          # \x1b[1;6C
          # \x1b[1;3A
          # \x1b[1;3B
          # \x1b[1;3D
          # \x1b[1;3C
          # \x1b[1;7A
          # \x1b[1;7B
          # \x1b[1;7D
          # \x1b[1;7C
          # \x1b[1;4A
          # \x1b[1;4B
          # \x1b[1;4D
          # \x1b[1;4C
          # \x1b[5;5~
          # \x1b[5;6~
          # \x1b[5;3~
          # \x1b[5;7~
          # \x1b[6;5~
          # \x1b[6;6~
          # \x1b[6;3~
          # \x1b[6;7~
          # \x1b[2;5~
          # \x1b[2;2~
          # \x1b[2;6~
          # \x1b[2;3~
          # \x1b[2;4~
          # \x1b[2;7~
          # \x1b[3;5~
          # \x1b[3;2~
          # \x1b[3;6~
          # \x1b[3;3~
          # \x1b[3;4~
          # \x1b[3;7~
          # \x1b[13;2u
          # \x1b[13;5u
          # \x1b[13;5u
          # \x1b[13;2u
          # \x1b[9;5u
          # \x1b[9;6u
          # \x0c
          # \x1b\x08
          # \x15
          # ==========================================
          settings = {
            # appearance
            cursor_shape = "beam";
            cursor_beam_thickness = "1.5";
            cursor_blink_interval = "-1";
            active_border_color = "red";
            window_border_width = "1.0pt";
            draw_minimal_borders = "no";
            tab_bar_edge = "top";
            tab_bar_background = "none";
            tab_bar_style = "powerline";
            tab_title_template = "{fmt.fg.c2c2c2} {index}: {f'{title[:6]}…{title[-6:]}' if title.rindex(title[-1]) + 1 > 13 else title} ";
            active_tab_title_template = "{fmt.fg._fff}{title}";
            active_tab_font_style = "bold-italic";
            url_style = "curly";
            bell_on_tab = "🔔 ";
            adjust_line_height = "115%";
            enable_audio_bell = "no";
            mark1_background = "#98d3cb";
            mark1_foreground = "black";
            mark2_background = "#f2dcd3";
            mark2_foreground = "black";
            mark3_background = "#f274bc";
            mark3_foreground = "black";
            tab_activity_symbol = "▲";
            pointer_shape_when_dragging = "beam";
            pointer_shape_when_grabbed = "arrow";
            url_color = "#0087bd";
            scrollback_fill_enlarged_window = "yes";
            enabled_layouts = concatStringsSep ", " [
              "splits:split_axis=horizontal"
              "grid"
              "tall:bias=70;full_size=1;mirrored=true"
              "fat:bias=50;full_size=1;mirrored=false"
              "horizontal"
              "vertical"
              "stack"
            ];
          } // {
            # navigation
            visual_window_select_characters = "qwerasdfzxcv";
            scrollback_lines = "${builtins.toString cfg.scrollbackSize}";
            scrollback_pager = "bat"; # TODO: consider making module for `bat` and make respective option for `kitty`
          } // {
            # resize
            remember_window_size = "yes";
            window_resize_step_cells = "2";
            window_resize_step_lines = "2";
            resize_draw_strategy = "scale";
          } // {
            # editing
            clipboard_control = "write-clipboard write-primary no-append";
          } // {
            # misc
            # kitty_mod = "ctrl+shift"; # TODO: play with this
            allow_hyperlinks = "yes";
            allow_remote_control = "yes";
            detect_urls = "yes";
            select_by_word_characters = ":@-./_~?&=%+#";
            strip_trailing_spaces = "smart";
            update_check_interval = "0";
            url_prefixes = "http https file mailto git";
            window_alert_on_bell = "yes";
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
            "ctrl+0" = "change_font_size all 0";
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

            "alt+x>[" = "combine : show_scrollback : send_text all \x2F";

            # FIXME: debug and add token-wise selection, see example below:
            # "Shift+Alt+B" = "select stream word left";
            # "Shift+Alt+F" = "select stream word right";
            # TODO: try https://github.com/yurikhan/kitty-smart-scroll, see example below:
            # map Ctrl+Shift+Home  kitten smart_scroll.py scroll_home Ctrl+Shift+Home
            # map Ctrl+Shift+End   kitten smart_scroll.py scroll_end  Ctrl+Shift+End
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
            "ctrl+shift+r" = "send_text all \\x12";
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
            "alt+x>page_up" = "kitten grab/grab.py";
            "alt+i" = "kitten grab/grab.py";
            "alt+x>f" = "launch --location=hsplit --allow-remote-control kitty +kitten search/search.py @active-kitty-window-id";
            "ctrl+alt+enter" = "launch --location=hsplit --cwd=current"; # TODO: keybinding
          };
          extraConfig = ''
            # These are not broken after 0.21.0 https://github.com/kovidgoyal/kitty/issues/3718
            mouse_map super+left press grabbed mouse_discard_event
            mouse_map super+left release grabbed,ungrabbed mouse_click_url
            mouse_map super+alt+left press ungrabbed mouse_selection rectangle
          '';
        };
      };
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("${lib.last cfg.windowClass}"), {
            K("C-x"): K("M-x"),
            K("C-r"): K("C-Shift-r"),
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
