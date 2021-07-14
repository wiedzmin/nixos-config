{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.appearance.colors.dracula;
  user = config.attributes.mainUser.name;
in
{
  options.appearance.colors.dracula = { enable = mkEnableOption "dracula"; };

  config = mkIf cfg.enable {
    home-manager.users."${user}" = {
      programs.alacritty = {
        settings = {
          colors = {
            primary = {
              background = "0x282a36";
              foreground = "0xf8f8f2";
            };
            cursor = {
              text = "CellBackground";
              cursor = "CellForeground";
            };
            vi_mode_cursor = {
              text = "CellBackground";
              cursor = "CellForeground";
            };
            search = {
              matches = {
                foreground = "0x44475a";
                background = "0x50fa7b";
              };
              focused_match = {
                foreground = "0x44475a";
                background = "0xffb86c";
              };
              bar = {
                background = "0x282a36";
                foreground = "0xf8f8f2";
              };
            };
            line_indicator = {
              foreground = "None";
              background = "None";
            };
            selection = {
              text = "CellForeground";
              background = "0x44475a";
            };
            normal = {
              black = "0x000000";
              red = "0xff5555";
              green = "0x50fa7b";
              yellow = "0xf1fa8c";
              blue = "0xbd93f9";
              magenta = "0xff79c6";
              cyan = "0x8be9fd";
              white = "0xbfbfbf";
            };
            bright = {
              black = "0x4d4d4d";
              red = "0xff6e67";
              green = "0x5af78e";
              yellow = "0xf4f99d";
              blue = "0xcaa9fa";
              magenta = "0xff92d0";
              cyan = "0x9aedfe";
              white = "0xe6e6e6";
            };
            dim = {
              black = "0x14151b";
              red = "0xff2222";
              green = "0x1ef956";
              yellow = "0xebf85b";
              blue = "0x4d5b86";
              magenta = "0xff46b0";
              cyan = "0x59dffc";
              white = "0xe6e6d1";
            };
          };
        };
      };
      services.dunst.settings = {
        urgency_low = {
          background = "#282a36";
          foreground = "#6272a4";
        };
        urgency_normal = {
          background = "#282a36";
          foreground = "#bd93f9";
        };
        urgency_critical = {
          background = "#ff5555";
          foreground = "#f8f8f2";
        };
      };
      programs.qutebrowser.extraConfig = ''
        def blood(c, options = {}):
            palette = {
                'background': '#282a36',
                'background-alt': '#282a36',
                'background-attention': '#181920',
                'border': '#282a36',
                'current-line': '#44475a',
                'selection': '#44475a',
                'foreground': '#f8f8f2',
                'foreground-alt': '#e0e0e0',
                'foreground-attention': '#ffffff',
                'comment': '#6272a4',
                'cyan': '#8be9fd',
                'green': '#50fa7b',
                'orange': '#ffb86c',
                'pink': '#ff79c6',
                'purple': '#bd93f9',
                'red': '#ff5555',
                'yellow': '#f1fa8c'
            }

            spacing = options.get('spacing', {
                'vertical': 5,
                'horizontal': 5
            })

            padding = options.get('padding', {
                'top': spacing['vertical'],
                'right': spacing['horizontal'],
                'bottom': spacing['vertical'],
                'left': spacing['horizontal']
            })

            ## Background color of the completion widget category headers.
            c.colors.completion.category.bg = palette['background']

            ## Bottom border color of the completion widget category headers.
            c.colors.completion.category.border.bottom = palette['border']

            ## Top border color of the completion widget category headers.
            c.colors.completion.category.border.top = palette['border']

            ## Foreground color of completion widget category headers.
            c.colors.completion.category.fg = palette['foreground']

            ## Background color of the completion widget for even rows.
            c.colors.completion.even.bg = palette['background']

            ## Background color of the completion widget for odd rows.
            c.colors.completion.odd.bg = palette['background-alt']

            ## Text color of the completion widget.
            c.colors.completion.fg = palette['foreground']

            ## Background color of the selected completion item.
            c.colors.completion.item.selected.bg = palette['selection']

            ## Bottom border color of the selected completion item.
            c.colors.completion.item.selected.border.bottom = palette['selection']

            ## Top border color of the completion widget category headers.
            c.colors.completion.item.selected.border.top = palette['selection']

            ## Foreground color of the selected completion item.
            c.colors.completion.item.selected.fg = palette['foreground']

            ## Foreground color of the matched text in the completion.
            c.colors.completion.match.fg = palette['orange']

            ## Color of the scrollbar in completion view
            c.colors.completion.scrollbar.bg = palette['background']

            ## Color of the scrollbar handle in completion view.
            c.colors.completion.scrollbar.fg = palette['foreground']

            ## Background color for the download bar.
            c.colors.downloads.bar.bg = palette['background']

            ## Background color for downloads with errors.
            c.colors.downloads.error.bg = palette['background']

            ## Foreground color for downloads with errors.
            c.colors.downloads.error.fg = palette['red']

            ## Color gradient stop for download backgrounds.
            c.colors.downloads.stop.bg = palette['background']

            ## Color gradient interpolation system for download backgrounds.
            ## Type: ColorSystem
            ## Valid values:
            ##   - rgb: Interpolate in the RGB color system.
            ##   - hsv: Interpolate in the HSV color system.
            ##   - hsl: Interpolate in the HSL color system.
            ##   - none: Don't show a gradient.
            c.colors.downloads.system.bg = 'none'

            ## Background color for hints. Note that you can use a `rgba(...)` value
            ## for transparency.
            c.colors.hints.bg = palette['background']

            ## Font color for hints.
            c.colors.hints.fg = palette['purple']

            ## Hints
            c.hints.border = '1px solid ' + palette['background-alt']

            ## Font color for the matched part of hints.
            c.colors.hints.match.fg = palette['foreground-alt']

            ## Background color of the keyhint widget.
            c.colors.keyhint.bg = palette['background']

            ## Text color for the keyhint widget.
            c.colors.keyhint.fg = palette['purple']

            ## Highlight color for keys to complete the current keychain.
            c.colors.keyhint.suffix.fg = palette['selection']

            ## Background color of an error message.
            c.colors.messages.error.bg = palette['background']

            ## Border color of an error message.
            c.colors.messages.error.border = palette['background-alt']

            ## Foreground color of an error message.
            c.colors.messages.error.fg = palette['red']

            ## Background color of an info message.
            c.colors.messages.info.bg = palette['background']

            ## Border color of an info message.
            c.colors.messages.info.border = palette['background-alt']

            ## Foreground color an info message.
            c.colors.messages.info.fg = palette['comment']

            ## Background color of a warning message.
            c.colors.messages.warning.bg = palette['background']

            ## Border color of a warning message.
            c.colors.messages.warning.border = palette['background-alt']

            ## Foreground color a warning message.
            c.colors.messages.warning.fg = palette['red']

            ## Background color for prompts.
            c.colors.prompts.bg = palette['background']

            # ## Border used around UI elements in prompts.
            c.colors.prompts.border = '1px solid ' + palette['background-alt']

            ## Foreground color for prompts.
            c.colors.prompts.fg = palette['cyan']

            ## Background color for the selected item in filename prompts.
            c.colors.prompts.selected.bg = palette['selection']

            ## Background color of the statusbar in caret mode.
            c.colors.statusbar.caret.bg = palette['background']

            ## Foreground color of the statusbar in caret mode.
            c.colors.statusbar.caret.fg = palette['orange']

            ## Background color of the statusbar in caret mode with a selection.
            c.colors.statusbar.caret.selection.bg = palette['background']

            ## Foreground color of the statusbar in caret mode with a selection.
            c.colors.statusbar.caret.selection.fg = palette['orange']

            ## Background color of the statusbar in command mode.
            c.colors.statusbar.command.bg = palette['background']

            ## Foreground color of the statusbar in command mode.
            c.colors.statusbar.command.fg = palette['pink']

            ## Background color of the statusbar in private browsing + command mode.
            c.colors.statusbar.command.private.bg = palette['background']

            ## Foreground color of the statusbar in private browsing + command mode.
            c.colors.statusbar.command.private.fg = palette['foreground-alt']

            ## Background color of the statusbar in insert mode.
            c.colors.statusbar.insert.bg = palette['background-attention']

            ## Foreground color of the statusbar in insert mode.
            c.colors.statusbar.insert.fg = palette['foreground-attention']

            ## Background color of the statusbar.
            c.colors.statusbar.normal.bg = palette['background']

            ## Foreground color of the statusbar.
            c.colors.statusbar.normal.fg = palette['foreground']

            ## Background color of the statusbar in passthrough mode.
            c.colors.statusbar.passthrough.bg = palette['background']

            ## Foreground color of the statusbar in passthrough mode.
            c.colors.statusbar.passthrough.fg = palette['orange']

            ## Background color of the statusbar in private browsing mode.
            c.colors.statusbar.private.bg = palette['background-alt']

            ## Foreground color of the statusbar in private browsing mode.
            c.colors.statusbar.private.fg = palette['foreground-alt']

            ## Background color of the progress bar.
            c.colors.statusbar.progress.bg = palette['background']

            ## Foreground color of the URL in the statusbar on error.
            c.colors.statusbar.url.error.fg = palette['red']

            ## Default foreground color of the URL in the statusbar.
            c.colors.statusbar.url.fg = palette['foreground']

            ## Foreground color of the URL in the statusbar for hovered links.
            c.colors.statusbar.url.hover.fg = palette['cyan']

            ## Foreground color of the URL in the statusbar on successful load
            c.colors.statusbar.url.success.http.fg = palette['green']

            ## Foreground color of the URL in the statusbar on successful load
            c.colors.statusbar.url.success.https.fg = palette['green']

            ## Foreground color of the URL in the statusbar when there's a warning.
            c.colors.statusbar.url.warn.fg = palette['yellow']

            ## Status bar padding
            c.statusbar.padding = padding

            ## Background color of the tab bar.
            ## Type: QtColor
            c.colors.tabs.bar.bg = palette['selection']

            ## Background color of unselected even tabs.
            ## Type: QtColor
            c.colors.tabs.even.bg = palette['selection']

            ## Foreground color of unselected even tabs.
            ## Type: QtColor
            c.colors.tabs.even.fg = palette['foreground']

            ## Color for the tab indicator on errors.
            ## Type: QtColor
            c.colors.tabs.indicator.error = palette['red']

            ## Color gradient start for the tab indicator.
            ## Type: QtColor
            c.colors.tabs.indicator.start = palette['orange']

            ## Color gradient end for the tab indicator.
            ## Type: QtColor
            c.colors.tabs.indicator.stop = palette['green']

            ## Color gradient interpolation system for the tab indicator.
            ## Type: ColorSystem
            ## Valid values:
            ##   - rgb: Interpolate in the RGB color system.
            ##   - hsv: Interpolate in the HSV color system.
            ##   - hsl: Interpolate in the HSL color system.
            ##   - none: Don't show a gradient.
            c.colors.tabs.indicator.system = 'none'

            ## Background color of unselected odd tabs.
            ## Type: QtColor
            c.colors.tabs.odd.bg = palette['selection']

            ## Foreground color of unselected odd tabs.
            ## Type: QtColor
            c.colors.tabs.odd.fg = palette['foreground']

            # ## Background color of selected even tabs.
            # ## Type: QtColor
            c.colors.tabs.selected.even.bg = palette['background']

            # ## Foreground color of selected even tabs.
            # ## Type: QtColor
            c.colors.tabs.selected.even.fg = palette['foreground']

            # ## Background color of selected odd tabs.
            # ## Type: QtColor
            c.colors.tabs.selected.odd.bg = palette['background']

            # ## Foreground color of selected odd tabs.
            # ## Type: QtColor
            c.colors.tabs.selected.odd.fg = palette['foreground']

            ## Tab padding
            c.tabs.padding = padding
            c.tabs.indicator.width = 1
            c.tabs.favicons.scale = 1

        dracula.draw.blood(c, {
            'spacing': {
                'vertical': 6,
                'horizontal': 8
            }
        })
      '';
      programs.zathura.options = {
        adjust-open = "width";
        completion-bg = "#282a36";
        completion-fg = "#6272a4";
        completion-group-bg = "#282a36";
        completion-group-fg = "#6272a4";
        completion-highlight-bg = "#44475a";
        completion-highlight-fg = "#f8f8f2";
        default-bg = "#282a36";
        default-fg = "#f8f8f2";
        highlight-active-color = "#ff79c6";
        highlight-color = "#ffb86c";
        index-active-bg = "#44475a";
        index-active-fg = "#f8f8f2";
        index-bg = "#282a36";
        index-fg = "#f8f8f2";
        inputbar-bg = "#282a36";
        inputbar-fg = "#f8f8f2";
        notification-bg = "#282a36";
        notification-error-bg = "#ff5555";
        notification-error-fg = "#f8f8f2";
        notification-fg = "#f8f8f2";
        notification-warning-bg = "#ffb86c";
        notification-warning-fg = "#44475a";
        recolor = false;
        recolor-darkcolor = "#f8f8f2";
        recolor-keephue = false;
        recolor-lightcolor = "#282a36";
        render-loading = true;
        render-loading-bg = "#f8f8f2";
        render-loading-fg = "#282a36";
        statusbar-bg = "#282a36";
        statusbar-fg = "#f8f8f2";
      };
    };
    wm.i3.theme = {
      client = ''
        # class                 border  bground text    indicator child_border
        client.focused          #6272A4 #6272A4 #F8F8F2 #6272A4   #6272A4
        client.focused_inactive #44475A #44475A #F8F8F2 #44475A   #44475A
        client.unfocused        #282A36 #282A36 #BFBFBF #282A36   #282A36
        client.urgent           #44475A #FF5555 #F8F8F2 #FF5555   #FF5555
        client.placeholder      #282A36 #282A36 #F8F8F2 #282A36   #282A36
        client.background       #F8F8F2
      '';
      bar = ''
        background #282A36
        statusline #F8F8F2
        separator  #44475A

        focused_workspace  #44475A #44475A #F8F8F2
        active_workspace   #282A36 #44475A #F8F8F2
        inactive_workspace #282A36 #282A36 #BFBFBF
        urgent_workspace   #FF5555 #FF5555 #F8F8F2
        binding_mode       #FF5555 #FF5555 #F8F8F2
      '';
    };
  };
}
