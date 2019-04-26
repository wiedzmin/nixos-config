{config, pkgs, lib, ...}:
with import ../../../toolbox/util.nix {inherit lib config pkgs;};
with import ../const.nix {inherit lib config pkgs;};
with lib;
{
    options = {
        sys.wallpaper = rec {
            baseDir = mkOption {
                type = types.str;
                default = "/home/${userName}/blobs/wallpaper";
                description = "Wallpapers directory location";
            };
            current = mkOption {
                type = types.str;
                default = builtins.elemAt inventory 6;
                description = "Current wallpaper image file";
            };
            inventory = [
                 "alena-aenami-7p-m-1k.jpg"
                 "alena-aenami-from-this-moment-1k.png"
                 "alena-aenami-over-the-city-1k.jpg"
                 "homeland__8_00_am_by_alexandreev_dapgzdw.jpg"
                 "mountain.jpg"
                 "westernmongolia.jpg"
                 "mongolia_2.jpg"
            ];
        };
        sys.fonts = rec {
            main = {
                name = mkOption { type = types.str; description = "Main font's family";
                    default = builtins.elemAt inventory 7;
                };
                weight = mkOption { type = types.str; description = "Main font's weight";
                    default = "Bold";
                };
                weightKeyword = mkOption { type = types.str; description = "Main font's weight keyword";
                    default = "weight";
                };
                useWeight = mkOption { type = types.bool; description = "Whether to use weight value in font's definition";
                    default = true;
                };
                sizeKeyword = mkOption { type = types.str; description = "XFT keyword to use for size definition";
                    default = "size";
                };
                antialias = mkOption { type = types.bool; description = "Antialias value for selected font";
                    default = false;
                };
                useAntialias = mkOption { type = types.bool; description = "Whether to use antialias value in font's definition";
                    default = false;
                };
            };
            code = {
                name = mkOption { type = types.str; description = "Code font's family";
                    default = builtins.elemAt inventory 7;
                };
                weight = mkOption { type = types.str; description = "Code font's weight";
                    default = "Bold";
                };
                weightKeyword = mkOption { type = types.str; description = "Code font's weight keyword";
                    default = "weight";
                };
                useWeight = mkOption { type = types.bool; description = "Whether to use weight value in font's definition";
                    default = true;
                };
                sizeKeyword = mkOption { type = types.str; description = "XFT keyword to use for size definition";
                    default = "size";
                };
                antialias = mkOption { type = types.bool; description = "Antialias value for selected font";
                    default = false;
                };
                useAntialias = mkOption { type = types.bool; description = "Whether to use antialias value in font's definition";
                    default = false;
                };
            };
            term = {
                name = mkOption { type = types.str; description = "Terminal font's family";
                    default = builtins.elemAt inventory 7;
                };
                weight = mkOption { type = types.str; description = "Terminal font's weight";
                    default = "Bold";
                };
                useWeight = mkOption { type = types.bool; description = "Whether to use weight value in font's definition";
                    default = true;
                };
                sizeKeyword = mkOption { type = types.str; description = "XFT keyword to use for size definition";
                    default = "size";
                };
                antialias = mkOption { type = types.bool; description = "Antialias value for selected font";
                    default = false;
                };
                useAntialias = mkOption { type = types.bool; description = "Whether to use antialias value in font's definition";
                    default = false;
                };
            };
            inventory = [
                "AnonymousPro"
                "DejavuSansMono"
                "FantasqueMono"
                "FiraCode"
                "FiraMono"
                "Hack"
                "Inconsolata"
                "Iosevka"
                "Mononoki"
                "Profont"
                "RobotoMono"
                "SourceCodePro"
                "Unifont"
            ];
            size = {
                Xmessage = mkOption { type = types.str; description = "Main font's size for Xmessage";
                    default = "16";
                };
                Dzen = mkOption { type = types.str; description = "Main font's size for Dzen2";
                    default = "16";
                };
                Emacs = mkOption { type = types.str; description = "Main font's size for GUI Emacs";
                    default = "14";
                };
                URxvt = mkOption { type = types.str; description = "Main font's size for URxvt";
                    default = "12";
                };
                Alacritty = mkOption { type = types.float; description = "Main font's size for Alacritty";
                    default = 11.0;
                };
                Dunst = mkOption { type = types.str; description = "Main font's size for Dunst";
                    default = "10";
                };
            };
        };
        sys.defaultShellClass = mkOption { type = types.str; description = "Default shell program WM_CLASS, for arbtt tagging";
            default = "Alacritty"; # TODO: generalize in some way
        };
        sys.defaultBrowserCmd = mkOption { type = types.str; description = "Default browser command for new webpages";
            default = "${firefoxOpenPageCmd}";
        };
        sys.kmscon.autologinUser = mkOption { type = types.str; description = "user to log into automatically in kmscon";
            default = "${userName}";
        };
    };
}
