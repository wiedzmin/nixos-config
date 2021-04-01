{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.appearance.wallpaper;
  user = config.attributes.mainUser.name;
  rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
    ${pkgs.feh}/bin/feh --bg-${cfg.transform} ${cfg.rootDir}/${cfg.current}
  '';
in {
  options = {
    appearance.wallpaper = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable wallpaper customization";
      };
      boot.splashImage = mkOption {
        type = types.str;
        default = "";
        description = "Boot splash image";
      };
      rootDir = mkOption {
        type = types.str;
        default = "";
        description = "Wallpapers root directory.";
      };
      current = mkOption {
        type = types.str;
        default = "";
        description = "Current wallpaper.";
      };
      transform = mkOption {
        default = "fill";
        type = types.enum [ "fill" "max" "scale" "tile" ];
        description = ''
          `fill`: Like `scale`, but preserves aspect ratio by zooming the image until it fits.
                  Either a horizontal or a vertical part of the image will be cut off.
          `max`: Like `fill`, but scale the image to the maximum size that fits the screen with borders on one side.
                 The border color can be set using --image-bg.
          `scale`: Fit the file into the background without repeating it, cutting off stuff or using borders.
                   But the aspect ratio is not preserved either.
          `tile`: Tile (repeat) the image in case it is too small for the screen
        '';
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
      assertions = [{
        assertion = cfg.rootDir != "" && cfg.current != "";
        message = "appearance: must provide wallpapers path and image to use.";
      }];

      home-manager.users.${user} = {
        programs.autorandr.hooks = {
          postswitch = { "rescale-wallpaper" = "${rescale-wallpaper}/bin/rescale-wallpaper"; };
        };
        programs.feh.enable = true;
      };

      boot.loader.grub.splashImage = optionalString (cfg.boot.splashImage != "") cfg.boot.splashImage;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = [ "w" ];
        cmd = "${rescale-wallpaper}/bin/rescale-wallpaper";
        mode = "xserver";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ rescale-wallpaper ]; };
    })
  ];
}
