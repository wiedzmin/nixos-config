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
        # TODO: move fonts settings to public
        sys.fonts = rec {
            main = {
                name = mkOption {
                    type = types.str;
                    default = builtins.elemAt inventory 7;
                    description = "Main font's family";
                };
                weight = mkOption {
                    type = types.str;
                    default = "Bold";
                    description = "Main font's weight";
                };
            };
            code = {
                name = mkOption {
                    type = types.str;
                    default = builtins.elemAt inventory 7;
                    description = "Code font's family";
                };
                weight = mkOption {
                    type = types.str;
                    default = "Bold";
                    description = "Code font's weight";
                };
            };
            term = {
                name = mkOption {
                    type = types.str;
                    default = builtins.elemAt inventory 7;
                    description = "Code font's family";
                };
                weight = mkOption {
                    type = types.str;
                    default = "Bold";
                    description = "Code font's weight";
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
                Xmessage = mkOption {
                    type = types.str;
                    default = "16";
                    description = "Main font's size for Xmessage";
                };
                Dzen = mkOption {
                    type = types.str;
                    default = "16";
                    description = "Main font's size for Dzen2";
                };
                Emacs = mkOption {
                    type = types.str;
                    default = "14";
                    description = "Main font's size for GUI Emacs";
                };
                URxvt = mkOption {
                    type = types.str;
                    default = "12";
                    description = "Main font's size for URxvt";
                };
                Alacritty = mkOption {
                    type = types.str;
                    default = "11.0";
                    description = "Main font's size for Alacritty";
                };
                Dunst = mkOption {
                    type = types.str;
                    default = "10";
                    description = "Main font's size for Dunst";
                };
            };
        };
        sys.defaultShellClass = mkOption { # TODO: generalize in some way
            type = types.str;
            default = "Alacritty";
            description = "Default shell program WM_CLASS, for arbtt tagging";
        };
        sys.defaultBrowserCmd = mkOption {
            type = types.str;
            default = "${firefoxOpenPageCmd}";
            description = "Default browser command for new webpages";
        };
        sys.kmscon.autologinUser = mkOption {
            type = types.str;
            default = "${userName}";
            description = "user to log into automatically in kmscon";
        };
    };
}
