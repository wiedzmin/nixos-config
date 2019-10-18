{config, pkgs, lib, ...}:
with lib;
{
  home-manager.users."${config.attributes.mainUser.name}" = {
    home.file = {
      ".config/alacritty/alacritty.yml".text = ''
        # use https://github.com/jwilm/alacritty/blob/master/alacritty.yml for reference
        env:
          TERM: xterm-256color
        window:
          dimensions:
            columns: 80
            lines: 24
          padding:
            x: 2
            y: 2
          decorations: true
        tabspaces: 8
        draw_bold_text_with_bright_colors: true
        font:
          normal:
            family: Iosevka
            style: Bold
          bold:
            family: Iosevka
            style: Bold
          italic:
            family: Iosevka
            style: Italic
          size: 11.0
        render_timer: false
        custom_cursor_colors: false
        # Colors (Oxide)
        visual_bell:
          animation: EaseOutExpo
          duration: 1
        background_opacity: 0.8
        mouse_bindings:
          - { mouse: Middle, action: PasteSelection }
        mouse:
          double_click: { threshold: 300 }
          triple_click: { threshold: 300 }
          faux_scrolling_lines: 1
        selection:
          semantic_escape_chars: ",│`|:\"' ()[]{}<>"
        dynamic_title: true
        hide_cursor_when_typing: false
        cursor_style: Beam
        live_config_reload: true
      '';
    };
  };
}
