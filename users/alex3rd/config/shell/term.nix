{ config, pkgs, lib, ... }:
with import ../../const.nix { inherit config pkgs; }; {
  home-manager.users."${userName}" = {
    programs.alacritty = {
      enable = true;
      settings = {
        env = { TERM = "xterm-256color"; };
        window = {
          padding = {
            x = 2;
            y = 2;
          };
          decorations = "full";
        };
        tabspaces = 8;
        draw_bold_text_with_bright_colors = true;
        font = {
          normal = {
            family = "${fontTermName}";
            style = "${fontTermWeight}";
          };
          bold = {
            family = "${fontTermName}";
            style = "${fontTermWeight}";
          };
          italic = {
            family = "${fontTermName}";
            style = "Italic";
          };
          size = fontSizeAlacritty;
        };
        visual_bell = {
          animation = "EaseOutExpo";
          duration = 1;
        };
        mouse_bindings = [{
          mouse = "Middle";
          action = "PasteSelection";
        }];
        selection = { semantic_escape_chars = '',│`|:"' ()[]{}<>''; };
        dynamic_title = true;
        cursor = { style = "Beam"; };
        live_config_reload = true;
      };
    };
  };
}
