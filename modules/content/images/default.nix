{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.images;
  user = config.attributes.mainUser.name;
  mkKeyValue = key: value:
    let
      mvalue =
        if builtins.isBool value then
          (if value then "True" else "False")
        else
          builtins.toString value;
    in
    "${key}=${mvalue}";
  toINIColon = generators.toINI { mkKeyValue = generators.mkKeyValueDefault { } ":"; };
  toINICustom = generators.toINI { inherit mkKeyValue; };
in
{
  options = {
    content.images = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable imaging content setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          exiv2
          mediainfo
          imagemagick
          vimiv-qt
        ];
        xdg.configFile."vimiv/keys.conf".text = toINIColon {
          GLOBAL = {
            "<colon>" = "command";
            "o" = "command --text='open '";
            "yy" = "copy-name";
            "ya" = "copy-name --abspath";
            "yA" = "copy-name --abspath --primary";
            "yY" = "copy-name --primary";
            "x" = "delete %%";
            "gi" = "enter image";
            "gl" = "enter library";
            "gm" = "enter manipulate";
            "gt" = "enter thumbnail";
            "f" = "fullscreen";
            "G" = "goto -1";
            "gg" = "goto 1";
            "m" = "mark %%";
            "q" = "quit";
            "." = "repeat-command";
            "j" = "scroll down";
            "h" = "scroll left";
            "l" = "scroll right";
            "k" = "scroll up";
            "/" = "search";
            "?" = "search --reverse";
            "N" = "search-next";
            "P" = "search-prev";
            "b" = "set statusbar.show!";
            "tl" = "toggle library";
            "tm" = "toggle manipulate";
            "tt" = "toggle thumbnail";
            "^" = "open ..";
          };
          IMAGE = {
            "M" = "center";
            "|" = "flip";
            "_" = "flip --vertical";
            "<end>" = "goto -1";
            "<home>" = "goto 1";
            "i" = "metadata";
            "<page-down>" = "next";
            "n" = "next";
            "<ctrl>n" = "next --keep-zoom";
            "<space>" = "play-or-pause";
            "<page-up>" = "prev";
            "p" = "prev";
            "<ctrl>p" = "prev --keep-zoom";
            ">" = "rotate";
            "<" = "rotate --counter-clockwise";
            "W" = "scale --level=1";
            "<equal>" = "scale --level=fit";
            "w" = "scale --level=fit";
            "E" = "scale --level=fit-height";
            "e" = "scale --level=fit-width";
            "J" = "scroll-edge down";
            "H" = "scroll-edge left";
            "L" = "scroll-edge right";
            "K" = "scroll-edge up";
            "sl" = "set slideshow.delay +0.5";
            "sh" = "set slideshow.delay -0.5";
            "ss" = "slideshow";
            "+" = "zoom in";
            "-" = "zoom out";
          };
          LIBRARY = {
            "go" = "goto 1 --open-selected";
            "n" = "scroll down --open-selected";
            "p" = "scroll up --open-selected";
            "L" = "set library.width +0.05";
            "H" = "set library.width -0.05";
          };
          THUMBNAIL = {
            "+" = "zoom in";
            "-" = "zoom out";
          };
          COMMAND = {
            "<tab>" = "complete";
            "<shift><tab>" = "complete --inverse";
            "<ctrl>p" = "history next";
            "<ctrl>n" = "history prev";
            "<up>" = "history-substr-search next";
            "<down>" = "history-substr-search prev";
            "<escape>" = "leave-commandline";
          };
          MANIPULATE = {
            "<colon>" = "command";
            "f" = "fullscreen";
            "b" = "set statusbar.show!";
          };
        };
        xdg.configFile."vimiv/vimiv.conf".text = toINICustom {
          GENERAL = {
            monitor_filesystem = true;
            shuffle = false;
            startup_library = true;
            style = "custom";
          };
          COMMAND = {
            history_limit = 100;
          };
          COMPLETION = {
            fuzzy = true;
          };
          SEARCH = {
            ignore_case = true;
            incremental = true;
          };
          IMAGE = {
            autoplay = true;
            autowrite = "ask";
            overzoom = 1.0;
          };
          LIBRARY = {
            width = 0.3;
            show_hidden = false;
          };
          THUMBNAIL = {
            size = 128;
          };
          SLIDESHOW = {
            delay = 2.0;
            indicator = "slideshow:";
          };
          STATUSBAR = {
            collapse_home = true;
            show = true;
            message_timeout = 60000;
            mark_indicator = "<b>*</b>";
            left = "{pwd}";
            left_image = "{index}/{total} {basename} [{zoomlevel}]";
            left_thumbnail = "{thumbnail-index}/{thumbnail-total} {thumbnail-name}";
            left_manipulate = "{basename}   {image-size}   Modified: {modified}   {processing}";
            center_thumbnail = "{thumbnail-size}";
            center = "{slideshow-indicator} {slideshow-delay} {transformation-info}";
            right = "{keys}  {mark-count}  {mode}";
            right_image = "{keys}  {mark-indicator} {mark-count}  {mode}";
          };
          KEYHINT = {
            delay = 500;
            timeout = 5000;
          };
          TITLE = {
            fallback = "vimiv";
            image = "vimiv - {basename}";
          };
          METADATA = {
            keys1 = concatStringsSep ", " [
              "Exif.Image.Make"
              "Exif.Image.Model"
              "Exif.Image.DateTime"
              "Exif.Photo.ExposureTime"
              "Exif.Photo.FNumber"
              "Exif.Photo.IsoSpeedRatings"
              "Exif.Photo.FocalLength"
              "Exif.Photo.LensMake"
              "Exif.Photo.LensModel"
              "Exif.Photo.ExposureBiasValue"
            ];
            keys2 = concatStringsSep ", " [
              "Exif.Photo.ExposureTime"
              "Exif.Photo.FNumber"
              "Exif.Photo.IsoSpeedRatings"
              "Exif.Photo.FocalLength"
            ];
            keys3 = "Exif.Image.Artist, Exif.Image.Copyright";
          };
          PLUGINS = {
            print = "default";
          };
          ALIASES = { };
        };
        xdg.configFile."vimiv/styles/custom".text = lib.generators.toINI { } {
          STYLE = {
            base00 = "#ffffff";
            base01 = "#e0e0e0";
            base02 = "#d6d6d6";
            base03 = "#8e908c";
            base04 = "#969896";
            base05 = "#4d4d4c";
            base06 = "#282a2e";
            base07 = "#1d1f21";
            base08 = "#c82829";
            base09 = "#f5871f";
            base0a = "#eab700";
            base0b = "#718c00";
            base0c = "#3e999f";
            base0d = "#81a2be";
            base0e = "#8959a8";
            base0f = "#a3685a";
            font = "10pt Iosevka";
            "image.bg" = "#ffffff";
            "image.scrollbar.width" = "8px";
            "image.scrollbar.bg" = "#ffffff";
            "image.scrollbar.fg" = "#8e908c";
            "image.scrollbar.padding" = "2px";
            "library.font" = "10pt Iosevka";
            "library.fg" = "#282a2e";
            "library.padding" = "2px";
            "library.directory.fg" = "#1d1f21";
            "library.even.bg" = "#e0e0e0";
            "library.odd.bg" = "#e0e0e0";
            "library.selected.bg" = "#81a2be";
            "library.selected.fg" = "#1d1f21";
            "library.search.highlighted.fg" = "#e0e0e0";
            "library.search.highlighted.bg" = "#969896";
            "library.scrollbar.width" = "8px";
            "library.scrollbar.bg" = "#ffffff";
            "library.scrollbar.fg" = "#8e908c";
            "library.scrollbar.padding" = "2px";
            "library.border" = "0px solid";
            "statusbar.font" = "10pt Iosevka";
            "statusbar.bg" = "#d6d6d6";
            "statusbar.fg" = "#282a2e";
            "statusbar.error" = "#c82829";
            "statusbar.warning" = "#f5871f";
            "statusbar.info" = "#3e999f";
            "statusbar.message_border" = "2px solid";
            "statusbar.padding" = "4";
            "thumbnail.font" = "10pt Iosevka";
            "thumbnail.fg" = "#282a2e";
            "thumbnail.bg" = "#ffffff";
            "thumbnail.padding" = "20";
            "thumbnail.selected.bg" = "#81a2be";
            "thumbnail.search.highlighted.bg" = "#969896";
            "thumbnail.default.bg" = "#3e999f";
            "thumbnail.error.bg" = "#c82829";
            "thumbnail.frame.fg" = "#282a2e";
            "completion.height" = "16em";
            "completion.fg" = "#282a2e";
            "completion.even.bg" = "#d6d6d6";
            "completion.odd.bg" = "#d6d6d6";
            "completion.selected.fg" = "#1d1f21";
            "completion.selected.bg" = "#81a2be";
            "keyhint.padding" = "2px";
            "keyhint.border_radius" = "10px";
            "keyhint.suffix_color" = "#3e999f";
            "manipulate.fg" = "#282a2e";
            "manipulate.focused.fg" = "#3e999f";
            "manipulate.bg" = "#ffffff";
            "manipulate.slider.left" = "#81a2be";
            "manipulate.slider.handle" = "#969896";
            "manipulate.slider.right" = "#d6d6d6";
            "manipulate.image.border" = "2px solid";
            "manipulate.image.border.color" = "#3e999f";
            "mark.color" = "#8959a8";
            "keybindings.bindings.color" = "#3e999f";
            "keybindings.highlight.color" = "#8959a8";
            "metadata.padding" = "2px";
            "metadata.border_radius" = "10px";
            "image.straighten.color" = "#eab700";
            "prompt.font" = "10pt Iosevka";
            "prompt.fg" = "#282a2e";
            "prompt.bg" = "#d6d6d6";
            "prompt.padding" = "2px";
            "prompt.border_radius" = "10px";
            "prompt.border" = "2px solid";
            "prompt.border.color" = "#3e999f";
            "library.selected.bg.unfocus" = "#8881a2be";
            "thumbnail.selected.bg.unfocus" = "#8881a2be";
            "metadata.bg" = "#AAd6d6d6";
          };
        };
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.images "vimiv.desktop";
        programs.zsh.shellAliases = {
          viq = "${pkgs.vimiv-qt}/bin/vimiv";
        };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/images.yml".text = ''
          name: images
          parent: default

          matches:
            - trigger: ":idim"
              replace: "identify -verbose $|$"
        '';
      };
    })
  ];
}
