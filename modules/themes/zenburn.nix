{ config, lib, pkgs, ... }:

with lib;

# TODO: dunst

let
  cfg = config.themes.zenburn;
  zenburnEmacs = ''
    (use-package zenburn-theme
      :ensure t
      :hook
      (after-init-hook . (lambda () (load-theme 'zenburn t))))
  '';
in {
  options.themes.zenburn = {
    enable = mkEnableOption "zenburn";
  };

  config = mkIf cfg.enable {
    ide.emacs.config = ''${zenburnEmacs}'';
    home-manager.users."${config.attributes.mainUser.name}" = {
      programs.emacs.extraPackages = epkgs: [
        epkgs.zenburn-theme
      ];
      programs.zathura.options = {
        completion-bg = "#404040";
        completion-fg = "#7cb8bb";
        completion-highlight-bg = "#7cb8bb";
        completion-highlight-fg = "#ffffff";
        default-bg = "#383838";
        default-fg = "#404040";
        highlight-active-color = "#7cb8bb";
        highlight-color = "#e0cf9f";
        inputbar-bg = "#383838";
        inputbar-fg = "#ffffff";
        notification-bg = "#383838";
        notification-error-bg = "#383838";
        notification-error-fg = "#dca3a3";
        notification-fg = "#ffffff";
        notification-warning-bg = "#383838";
        notification-warning-fg = "#dca3a3";
        recolor = false;
        recolor-darkcolor = "#c0c0c0";
        recolor-keephue = false;
        recolor-lightcolor = "#383838";
        statusbar-bg = "#606060";
        statusbar-fg = "#808080";
      };
      # credits @jollheef
      gtk = {
        gtk2.extraConfig = ''
          style "default"
          {
              GtkButton::default_border                    = {0, 0, 0, 0}
              GtkButton::default_outside_border            = {0, 0, 0, 0}
              GtkButton::child_displacement_x              = 0
              GtkButton::child_displacement_y              = 1
              GtkButton::default_spacing                   = 4
              GtkButton::focus-padding                     = 0
              GtkCheckButton::indicator_size               = 8
              GtkMenuBar::internal-padding                 = 1
              GtkMenuBar::shadow_type                      = out
              GtkHandleBox::shadow_type                    = out
              GtkMenuItem::selected_shadow_type            = etched-in
              GtkPaned::handle_full_size                   = 1
              GtkPaned::handle_size                        = 4
              GtkRadioButton::indicator_size               = 10
              GtkRange::slider_width                       = 12
              GtkRange::stepper_size                       = 10
              GtkRange::stepper_spacing                    = 0
              GtkRange::trough_border                      = 0
              GtkScrollbar::has_backward_stepper           = 1
              GtkScrollbar::has_secondary_backward_stepper = 0
              GtkScrollbar::min_slider_length              = 10
              GtkToolbar::shadow_type                      = out
              GtkWidget::focus-line-width                  = 1
              GtkWidget::focus_padding                     = 1
              GtkWidget::interior_focus                    = 1
              GtkWidget::internal_padding                  = 2
              GtkEntry::cursor_color                       = "#000000"
              GtkEntry::secondary_cursor_color             = "#fcfcfc"
              GtkTextView::cursor_color                    = "#000000"
              GtkTextView::secondary_cursor_color          = "#fcfc00"
              GtkEntry::cursor_aspect_ratio                = 0.1
              GtkEntry::cursor_aspect_ratio                = 0.1
              GtkWidget::link-color                        = "#569cc1"
              GtkWidget::visited-link-color                = "#f2f2f2"

              xthickness             = 1
              ythickness             = 1


              base[ACTIVE]      = "#2e3330"
              base[INSENSITIVE] = "#303030"
              base[NORMAL]      = "#333333"
              base[PRELIGHT]    = "#1e2320"
              base[SELECTED]    = "#1e2320"

              bg[ACTIVE]        = "#2e3330"
              bg[INSENSITIVE]   = "#3f3f3f"
              bg[NORMAL]        = "#3f3f3f"
              bg[PRELIGHT]      = "#2e3330"
              bg[SELECTED]      = "#2e3330"

              fg[ACTIVE]        = "#dcdccc"
              fg[INSENSITIVE]   = "#dcdccc"
              fg[NORMAL]        = "#dcdccc"
              fg[PRELIGHT]      = "#dcdccc"
              fg[SELECTED]      = "#dcdccc"

              text[ACTIVE]      = "#dcdccc"
              text[INSENSITIVE] = "#dcdccc"
              text[NORMAL]      = "#dcdccc"
              text[PRELIGHT]    = "#dcdccc"
              text[SELECTED]    = "#dcdccc"

              engine "xfce"
              {
            smooth_edge = true
                  boxfill
                  {
                      fill_style = plain
                  }
              }
          }
          widget_class "*"                   style "default"

          style "menustyle" = "default"
          {
              xthickness = 2
              ythickness = 2
          }
          widget_class "*BonoboDockItem"     style "menustyle"
          class "*BonoboDockItem"            style "menustyle"
          widget_class "*ToolBar"            style "menustyle"
          class "*ToolBar"                   style "menustyle"
          widget_class "*MenuBar"            style "menustyle"
          class "*MenuBar"                   style "menustyle"

          style "button" = "default"
          {
              xthickness = 2
              ythickness = 2

              engine "xfce"
              {
                  smooth_edge = true
                  grip_style = none
                  boxfill
                  {
                      fill_style = gradient
                      orientation = vertical
                      shade_start = 1.25
                      shade_end = 1.00
                  }
              }
          }
          widget_class "*Button*"            style "button"
          class "*Button*"                   style "button"
          widget_class "*button*"            style "button"
          class "*button*"                   style "button"
          widget_class "*OptionMenu*"        style "button"
          class "*OptionMenu*"               style "button"
          # widget_class "*Tree*"            style "button"
          # class "*Tree*"                   style "button"
          # widget_class "*GtkScale*"        style "button"
          # class "*GtkScale*"               style "button"

          style "sbstyle" = "default"
          {
              xthickness = 2
              ythickness = 2
              engine "xfce"
              {
                  smooth_edge = true
                  grip_style = none
                  boxfill
                  {
                      fill_style = gradient
                      orientation = automatic
                      shade_start = 1.25
                      shade_end = 1.00
                  }
              }
          }
          widget_class "*Scrollbar*"         style "sbstyle"
          class "*Scrollbar*"                style "sbstyle"
          widget_class "*GtkScale*"          style "sbstyle"
          class "*GtkScale*"                 style "sbstyle"

          style "progress" = "default"
          {
              xthickness = 2
              ythickness = 2
          }
          widget_class "*GtkProgress*"       style "progress"
          class "*GtkProgress*"              style "progress"

          style "menuitem" = "default"
          {
              xthickness = 1
              ythickness = 2
          }

          widget_class "*MenuItem*"          style "menuitem"
          class "*MenuItem*"                 style "menuitem"

          style "flat" = "default"
          {
              xthickness = 2
              ythickness = 2
          }
          widget_class "*HandleBox"         style "flat"

          # This is for the window borders (xfwm4 & metacity)
          #
          style "titlebar"
          {
              bg[SELECTED]      = "#003263"
              fg[SELECTED]      = "#ffffff"
              bg[INSENSITIVE]   = "#002849"
              fg[INSENSITIVE]   = "#9a9ada"
          }
          widget "xfwm"                      style "titlebar"
          class "MetaFrames"                 style "titlebar"
          widget_class "MetaFrames"          style "titlebar"
        '';
        gtk3.extraCss = ''
          @define-color fg_active #dcdccc;
          @define-color fg_insensitive #dcdccc;
          @define-color fg_normal #dcdccc;
          @define-color fg_prelight #dcdccc;
          @define-color fg_selected #dcdccc;

          @define-color bg_active #2e3330;
          @define-color bg_insensitive #3f3f3f;
          @define-color bg_normal #3f3f3f;
          @define-color bg_prelight #2e3330;
          @define-color bg_selected #2e3330;

          @define-color base_active #2e3330;
          @define-color base_insensitive #303030;
          @define-color base_normal #333333;
          @define-color base_prelight #1e2320;
          @define-color base_selected #1e2320;

          @define-color text_active #fcfcfc;
          @define-color text_insensitive #fcfcfc;
          @define-color text_normal #fcfcfc;
          @define-color text_prelight #fcfcfc;
          @define-color text_selected #fcfcfc;

          * {
              engine: xfce;

              -xfce-smooth-edge: true;
              -xfce-grip-style: disabled;

              -GtkButton-default-border        : 0;
              -GtkButton-default-outside-border: 0;
              -GtkButton-child-displacement-x  : 0;
              -GtkButton-child-displacement-y  : 1;
              -GtkCheckButton-indicator-size   : 8;
              -GtkMenuBar-internal-padding     : 1;
              -GtkMenuBar-shadow-type          : out;
              -GtkPaned-handle-size            : 4;
              -GtkRange-slider-width           : 12;
              -GtkRange-stepper-size           : 10;
              -GtkRange-stepper-spacing        : 0;
              -GtkRange-trough-border          : 0;
              -GtkScrollbar-has-backward-stepper: true;
              -GtkScrollbar-has-secondary-backward-stepper: false;
              -GtkScrollbar-min-slider-length  : 10;
              -GtkToolbar-shadow-type          : out;
              -GtkWidget-focus-line-width      : 1;
              -GtkWidget-focus-padding         : 1;
              -GtkWidget-interior-focus        : true;
              -GtkWidget-cursor-color          : #fcfcfc;
              -GtkWidget-secondary-cursor-color: #fcfcfc;
              -GtkWidget-cursor-aspect-ratio   : 0.1;
              -GtkWidget-link-color           : #569cc1;
              -GtkWidget-visited-link-color   : #f2f2f2;


              border-width: 1px 1px;

              color: @fg_normal;
              background-color: @bg_normal;
              border-color: darker(@bg_normal);
          }

          *:active {
              color: @fg_active;
              background-color: @bg_active;
              border-color: darker(@bg_active);
          }

          *:disabled {
              color: @fg_insensitive;
              background-color: @bg_insensitive;
              border-color: darker(@bg_insensitive);
              text-shadow: 1 1 white;
          }

          *:hover {
              color: @fg_prelight;
              background-color: @bg_prelight;
              border-color: darker(@bg_prelight);
          }

          *:selected {
              color: @fg_selected;
              background-color: @bg_selected;
              border-color: darker(@bg_selected);
          }

          .view, .entry {
              color: @text_normal;
              background-color: @base_normal;
              border-color: darker(@bg_normal);
          }

          .view:active, .entry:active {
              color: @text_active;
              background-color: @base_active;
              border-color: darker(@bg_normal);
          }

          .view:disabled, .entry:disabled {
              color: @text_insensitive;
              background-color: @base_insensitive;
              border-color: darker(@bg_normal);
          }

          .view:hover, .entry:hover {
              color: @text_prelight;
              background-color: @base_prelight;
              border-color: darker(@bg_normal);
          }

          .view:selected, .entry:selected {
              color: @text_selected;
              background-color: @base_selected;
              border-color: darker(@bg_normal);
          }

          .view row:nth-child(odd) {
              background-color: shade(@base_normal, 0.93);
          }

          .view row:nth-child(odd):active {
              background-color: shade(@base_active, 0.93);
          }

          .view row:nth-child(odd):disabled {
              background-color: shade(@base_insensitive, 0.93);
          }

          .view row:nth-child(odd):hover {
              background-color: shade(@base_prelight, 0.93);
          }

          .view row:nth-child(odd):selected {
              background-color: shade(@base_selected, 0.93);
          }

          GtkCalendar {
              color: @text_normal;
          }

          .header {
              color: @fg_normal;
          }

          GtkCalendar:active {
              color: @text_active;
          }

          GtkCalendar:disabled {
              color: @text_insensitive;
          }

          GtkCalendar:hover {
              color: @text_prelight;
          }

          GtkCalendar:selected {
              color: @text_selected;
          }

          .highlight {
              color: @text_active;
              background-color: @base_active;
          }

          GtkCalendar:indeterminate {
              color: mix(lighter(@bg_normal), darker(@bg_normal), 0.5);
          }

          GtkCalendar:selected {
              color: @text_active;
              background-color: @base_active;
          }

          .check, .radio {
              color: @text_normal;
              background-color: @base_normal;
              border-color: darker(@bg_normal);
          }

          .check:active, .radio:active {
              color: @text_normal;
              background-color: @base_normal;
              border-color: darker(@bg_normal);
          }

          .check:disabled, .radio:disabled {
              color: @text_insensitive;
              background-color: @base_insensitive;
              border-color: darker(@bg_insensitive);
          }

          .check:hover, .radio:hover {
              color: @text_prelight;
              background-color: @base_prelight;
              border-color: darker(@bg_prelight);
          }

          .check:selected, .radio:selected {
              color: @text_active;
              background-color: @base_active;
              border-color: darker(@bg_active);
          }

          .check:indeterminate, .radio:indeterminate {
              color: @fg_normal;
              background-color: @base_normal;
              border-color: darker(@bg_normal);
          }

          .check:disabled:indeterminate, .radio:disabled:indeterminate {
              color: @fg_insensitive;
              background-color: @base_insensitive;
              border-color: darker(@bg_insensitive);
          }

          .cell {
              color: @text_normal;
          }

          .cell:active {
              color: @text_active;
          }

          .cell:disabled {
              color: @text_insensitive;
          }

          .cell:hover {
              color: @text_prelight;
          }

          .cell:selected {
              color: @text_selected;
          }

          .spinbutton.button {
              color: @fg_normal;
              background-color: @bg_normal;
          }

          .spinbutton.button:active {
              color: @fg_active;
              background-color: @bg_active;
          }

          .spinbutton.button:disabled {
              color: @fg_insensitive;
              background-color: @bg_insensitive;
          }

          .spinbutton.button:hover {
              color: @fg_prelight;
              background-color: @bg_prelight;
          }

          .spinbutton.button:selected {
              color: @fg_selected;
              background-color: @bg_selected;
          }

          .progressbar {
              background-color: @bg_prelight;
              border-color: darker(@bg_prelight);

              border-width: 1px 1px;
          }

          .scale.trough, .scrollbar.trough {
              background-image: -gtk-gradient(linear, left top, right top, from(shade(@bg_active, 1.0)), to(shade(@bg_active, 1.25)));
              border-color: darker(@bg_active);
          }

          .trough.horizontal {
              background-image: -gtk-gradient(linear, left top, left bottom, from(shade(@bg_active, 1.0)), to(shade(@bg_active, 1.25)));
              border-color: darker(@bg_active);
          }

          .notebook tab, .notebook tab * {
              color: @fg_active;
              background-color: @bg_active;
              border-color: darker(@bg_active);
          }

          .notebook tab:active, .notebook tab *:active {
              color: @fg_normal;
              background-color: @bg_normal;
              border-color: darker(@bg_normal);
          }

          .expander {
              color: @base_normal;
              border-color: @fg_normal;
          }

          .expander:active {
              color: @base_normal;
              border-color: @fg_normal;
          }

          .expander:disabled {
              color: @base_insensitive;
              border-color: @fg_insensitive;
          }

          .expander:hover {
              color: @fg_prelight;
              border-color: @fg_prelight;
          }

          .expander:selected {
              color: @base_selected;
              border-color: @fg_selected;
          }

          GtkComboBox {
              padding: 0px;
          }

          GtkComboBox .button {
              padding: 1px;
          }

          .radio {
              -GtkCheckButton-indicator-size: 10;
          }

          .menuitem {
              border-width: 1px 2px;
          }

          .button, .slider.horizontal {
              background-image: -gtk-gradient(linear, left top, left bottom, from(shade(@bg_normal, 1.25)), to(shade(@bg_normal, 1.0)));

              -GtkWidget-focus-padding: 0;
          }

          .button:active, .slider.horizontal:active {
              background-image: -gtk-gradient(linear, left top, left bottom, from(shade(@bg_active, 1.0)), to(shade(@bg_active, 1.25)));
          }

          .button:disabled, .slider.horizontal:disabled {
              background-image: -gtk-gradient(linear, left top, left bottom, from(shade(@bg_insensitive, 1.25)), to(shade(@bg_insensitive, 1.0)));
          }

          .button:hover, .slider.horizontal:hover {
              background-image: -gtk-gradient(linear, left top, left bottom, from(shade(@bg_prelight, 1.25)), to(shade(@bg_prelight, 1.0)));
          }

          .button:selected, .slider.horizontal:selected {
              background-image: -gtk-gradient(linear, left top, left bottom, from(shade(@bg_selected, 1.0)), to(shade(@bg_selected, 1.25)));
          }

          .menuitem {
              border-width: 1px 1px;
          }

          .menuitem:hover {
              border-style: solid;
          }

          .slider.vertical {
              background-image: -gtk-gradient(linear, left top, right top, from(shade(@bg_normal, 1.25)), to(shade(@bg_normal, 1.0)));
          }

          .slider.vertical:active {
              background-image: -gtk-gradient(linear, left top, right top, from(shade(@bg_active, 1.0)), to(shade(@bg_active, 1.25)));
          }

          .slider.vertical:disabled {
              background-image: -gtk-gradient(linear, left top, right top, from(shade(@bg_insensitive, 1.25)), to(shade(@bg_insensitive, 1.0)));
          }

          .slider.vertical:hover {
              background-image: -gtk-gradient(linear, left top, right top, from(shade(@bg_prelight, 1.25)), to(shade(@bg_prelight, 1.0)));
          }

          .slider.vertical:selected {
              background-image: -gtk-gradient(linear, left top, right top, from(shade(@bg_selected, 1.0)), to(shade(@bg_selected, 1.25)));
          }
        '';
      };
    };
  };
}
