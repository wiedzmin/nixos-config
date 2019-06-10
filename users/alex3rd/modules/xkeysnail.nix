{ config, lib, pkgs, ...}:
with import ../const.nix {inherit config pkgs;};
with lib;

# TODO: think of adding more options
let
    cfg = config.services.xkeysnail;
in {
    options = {
        services.xkeysnail = {
            enable = mkOption {
                type = types.bool;
                default = false;
                example = true;
                description = ''
                    Whether to enable xkeysnail.
                '';
            };
            userName = mkOption {
                type = types.str;
                default = "";
                description = ''
                    Name of a user who runs service.
                '';
            };
        };
    };

    config = mkMerge [
        {
            assertions = [ # FIXME: assertion condition fires before this at "compile" time
                { assertion = cfg.userName != ""; message = "Must provide user name."; }
            ];
        }

        (mkIf cfg.enable {
            systemd.user.services."xkeysnail" = {
                description = "Xkeysnail";
                wantedBy = [ "graphical.target" ];
                partOf = [ "graphical.target" ];
                environment = {
                    DISPLAY = ":0";
                    XAUTHORITY = "/home/${userName}/.Xauthority";
                };
                serviceConfig = {
                    PIDFile = "/var/run/xkeysnail.pid";
                    Restart = "always";
                    RestartSec = 1;
                    ExecStart = "/run/wrappers/bin/sudo ${pkgs.xkeysnail}/bin/xkeysnail /home/${userName}/.config/xkeysnail/config.py";
                };
            };
            home-manager.users."${cfg.userName}" = {
                home.file = {
                    ".config/xkeysnail/config.py".text = ''
                        # -*- coding: utf-8 -*-

                        import re
                        from xkeysnail.transform import *

                        define_conditional_modmap(re.compile(r'Emacs'), {
                            Key.RIGHT_CTRL: Key.ESC,
                        })

                        define_keymap(re.compile("Firefox"), {
                            K("C-j"): K("C-f6"), # Type C-j to focus to the content
                            K("C-g"): K("f5"),
                            K("C-Shift-Right"): K("C-TAB"),
                            K("C-Shift-Left"): K("C-Shift-TAB"),
                            K("C-Shift-comma"): Combo(Modifier.ALT, Key.KEY_1),
                            K("C-Shift-dot"): Combo(Modifier.ALT, Key.KEY_9),
                            K("C-n"): K("C-g"),
                            K("C-Shift-n"): K("C-Shift-g"),
                            K("M-comma"): K("M-Left"),
                            K("M-dot"): K("M-Right"),
                            K("C-x"): {
                                K("b"): K("b"),
                                K("k"): K("C-w"),
                                K("u"): K("C-Shift-t"),
                                K("C-s"): K("C-s"),
                                K("C-c"): K("C-q"),
                            },
                        }, "Firefox")

                        define_keymap(re.compile("TelegramDesktop"), {
                            K("C-x"): {
                                K("C-c"): K("C-q"),
                            },
                            K("C-s"): K("Esc"),
                            K("C-t"): [K("Shift-Left"), K("C-x"), K("Left"), K("C-v"), K("Right")],
                        }, "Telegram")

                        define_keymap(re.compile("Alacritty"), {
                            K("C-x"): {
                                K("k"): K("C-d"),
                            },
                        }, "Alacritty")

                        # Emacs-like keybindings in non-Emacs applications
                        define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "Alacritty"), {
                            # Cursor
                            K("C-b"): with_mark(K("left")),
                            K("C-f"): with_mark(K("right")),
                            K("C-p"): with_mark(K("up")),
                            K("C-n"): with_mark(K("down")),
                            K("C-h"): with_mark(K("backspace")),
                            # Forward/Backward word
                            K("M-b"): with_mark(K("C-left")),
                            K("M-f"): with_mark(K("C-right")),
                            # Beginning/End of line
                            K("C-a"): with_mark(K("home")),
                            K("C-e"): with_mark(K("end")),
                            # Page up/down
                            K("M-v"): with_mark(K("page_up")),
                            K("C-v"): with_mark(K("page_down")),
                            # Beginning/End of file
                            K("M-Shift-comma"): with_mark(K("C-home")),
                            K("M-Shift-dot"): with_mark(K("C-end")),
                            # Newline
                            K("C-m"): K("enter"),
                            K("C-j"): K("enter"),
                            K("C-o"): [K("enter"), K("left")],
                            # Copy
                            K("C-w"): [K("C-x"), set_mark(False)],
                            K("M-w"): [K("C-c"), set_mark(False)],
                            K("C-y"): [K("C-v"), set_mark(False)],
                            # Delete
                            K("C-d"): [K("delete"), set_mark(False)],
                            K("M-d"): [K("C-delete"), set_mark(False)],
                            # Kill line
                            K("C-k"): [K("Shift-end"), K("C-x"), set_mark(False)],
                            # Undo
                            K("C-slash"): [K("C-z"), set_mark(False)],
                            K("C-Shift-ro"): K("C-z"),
                            # Mark
                            K("C-space"): set_mark(True),
                            #K("C-M-space"): with_or_set_mark(K("C-right")),
                            # Search
                            K("C-s"): K("F3"),
                            K("C-r"): K("Shift-F3"),
                            K("M-Shift-key_5"): K("C-h"),
                            # Cancel
                            K("C-g"): [K("esc"), set_mark(False)],
                            # Escape
                            K("C-q"): escape_next_key,
                            # C-x YYY
                            K("C-x"): {
                                # C-x h (select all)
                                K("h"): [K("C-home"), K("C-a"), set_mark(True)],
                                # C-x C-f (open)
                                K("C-f"): K("C-o"),
                                # C-x C-s (save)
                                # K("C-s"): K("C-s"),
                                # C-x k (kill tab)
                                K("k"): K("C-f4"),
                                # C-x C-c (exit)
                                K("C-c"): K("C-q"),
                                # cancel
                                K("C-g"): pass_through_key,
                                # C-x u (undo)
                                K("u"): [K("C-z"), set_mark(False)],
                            }
                        }, "Emacs-like keys")
                    '';
                };
            };
        })
    ];
}
