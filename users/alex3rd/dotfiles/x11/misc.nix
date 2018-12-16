{config, pkgs, lib, ...}:

{
    imports = [
        ../../scripts/statuses.nix
        ../../scripts/navigation.nix
        ../../private/traits/nas.nix
    ];

    system.activationScripts.ensureBacklightPermissions = ''
        chmod a+w /sys/class/backlight/intel_backlight/brightness
    '';

    environment.etc."Xmodmaprc".text = ''
        clear mod1
        clear mod4
        clear mod5
        keycode 64 = Alt_L Meta_L
        keycode 133 = Super_L
        keycode 108 = Hyper_L
        keycode 191 = Insert
        add mod1 = Meta_L
        add mod1 = Alt_L
        add mod4 = Super_L
        add mod5 = Hyper_L
    '';

    home-manager.users.alex3rd = {
        home.packages = with pkgs; [

            # helper scripts for WMs
            rescale-wallpaper
            rofi_list_autorandr_profiles
            rofi_list_bookshelf
            rofi_list_tmuxp_sessions
            rofi_mount_nas_volume
            rofi_ref_extra_hosts
            rofi_searchengines_prompt
            rofi_searchengines_selection
            rofi_ssh_custom_user
            rofi_unmount_nas_volume
            rofi_webjumps
            screenshot_active_window
            screenshot_full
            screenshot_region
            show_uptime_info
        ];
        home.file = {
            "${config.common.snippetsFile}".text = ''
                ${lib.concatStringsSep "\n" config.common.snippets}
            '';
            ".arbtt/categorize.cfg".text = ''
                aliases (
                        "Navigator" -> "Firefox",
                        "Zathura" -> "PDF reader",
                        "telegram-desktop" -> "Telegram",
                        "Alacritty" -> "Shell",
                )

                $idle > 60 ==> tag inactive,
                current window $program == ["Navigator", "Google-chrome", "Google-chrome-stable"] ==> tag activity:web,
                current window $program == ["Zathura"] ==> tag activity:pdf,
                current window $program == ["FBReader"] ==> tag activity:fiction,

                tag apps:$current.program, -- just tags the current program

                -- projects at work
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
                  ==> tag project:$1-$2,
                current window ($program == "Alacritty" && $title =~ m!(?:~|home/alex3rd)/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
                  ==> tag project:$1-$2,

                -- personal projects
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/workspace/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/!)
                  ==> tag project:$1-$2,
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/.xmonad/!) ==> tag project:xmonad-config,
                current window ($program == "Alacritty" && $title =~ m!(?:~|home/alex3rd)/.xmonad/!) ==> tag project:xmonad-config,
                current window ($program == "emacs" && $title =~ m!(?:~|home/alex3rd)/.emacs.d/!) ==> tag project:emacs-config,
                current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,

                current window ($program == "Navigator" && $title =~ /Facebook/) ==> tag site:facebook,
                current window ($program == "Navigator" && $title =~ /Gmail/) ==> tag web:Gmail,
                current window ($program == "Navigator" && $title =~ /Google/) ==> tag web:Google,
                current window ($program == "Navigator" && $title =~ /wikipedia/) ==> tag site:wikipedia,
                current window ($program == "Navigator" && $title =~ /habr/) ==> tag site:habr,
                current window ($program == "Navigator" && $title =~ /pypi/) ==> tag site:pypi,
                current window ($program == "Navigator" && $title =~ /stackoverflow/) ==> tag site:stackoverflow,

                current window ($title =~ /^emacs - [^ ]+\.c .*$/) ==> tag edit:c,
                current window ($title =~ /^emacs - [^ ]+\.py .*$/) ==> tag edit:python,
                current window ($title =~ /^emacs - [^ ]+\.hs .*$/) ==> tag edit:haskell,
                current window ($title =~ /^emacs - [^ ]+\.lisp .*$/) ==> tag edit:cl,
                current window ($title =~ /^emacs - [^ ]+\.el .*$/) ==> tag edit:elisp,
                current window ($title =~ /^emacs - [^ ]+config\.org .*$/) ==> tag edit:elisp,
                current window ($title =~ /^emacs - [^ ]+\.pdf .*$/) ==> tag activity:pdf,

                -- $time evaluates to local time.
                $time >=  2:00 && $time <  8:00 ==> tag time-of-day:night,
                $time >=  8:00 && $time < 12:00 ==> tag time-of-day:morning,
                $time >= 12:00 && $time < 14:00 ==> tag time-of-day:lunchtime,
                $time >= 14:00 && $time < 18:00 ==> tag time-of-day:afternoon,
                $time >= 18:00 && $time < 22:00 ==> tag time-of-day:evening,
                $time >= 22:00 || $time <  2:00 ==> tag time-of-day:late-evening,

                -- This tag always refers to the last 24h
                $sampleage <= 24:00 ==> tag last-day,
                -- ...and last hour respectively
                $sampleage <= 1:00 ==> tag last-hour,

                -- year/months
                year $date == 2014 ==> tag year:2014,
                year $date == 2015 ==> tag year:2015,
                year $date == 2016 ==> tag year:2016,
                year $date == 2017 ==> tag year:2017,
                year $date == 2018 ==> tag year:2018,
                month $date == 1 ==> tag month:January,
                month $date == 2 ==> tag month:February,
                month $date == 3 ==> tag month:March,
                month $date == 4 ==> tag month:April,
                month $date == 5 ==> tag month:May,
                month $date == 6 ==> tag month:June,
                month $date == 7 ==> tag month:July,
                month $date == 8 ==> tag month:August,
                month $date == 9 ==> tag month:September,
                month $date == 10 ==> tag month:October,
                month $date == 11 ==> tag month:November,
                month $date == 12 ==> tag month:December,

                day of month $now == day of month $date ==> tag current-day,
                day of week $date == 1 ==> tag week:Monday,
                day of week $date == 2 ==> tag week:Tuesday,
                day of week $date == 3 ==> tag week:Wednesday,
                day of week $date == 4 ==> tag week:Thursday,
                day of week $date == 5 ==> tag week:Friday,
                day of week $date == 6 ==> tag week:Saturday,
                day of week $date == 7 ==> tag week:Sunday,

                month $now == month $date ==> tag current-month,
                year $now == year $date ==> tag current-year,

                ${builtins.concatStringsSep "\n"
                           (pkgs.stdenv.lib.mapAttrsToList
                                 (ip: meta: builtins.concatStringsSep "\n"
                                            (map (hostname: "current window ($program == \"" +
                                                            config.sys.default_shell_class + "\" && $title =~ /" +
                                                            hostname + "/) ==> tag ssh:" + hostname + ",")
                                                 meta.hostNames))
                                 (config.job.extra_hosts // config.misc.extra_hosts))}

            '';
            ".config/xmobar/xmobarrc".text = ''
                Config { font = "xft:Iosevka:style=Bold:pixelsize=16"
                       , bgColor = "black"
                       , fgColor = "grey"
                       , position = TopW L 100
                       , lowerOnStart = False
                       , allDesktops = True
                       , persistent = True
                       , commands = [ Run Date "%a %d/%m/%y %H:%M:%S" "date" 10
                                    , Run StdinReader
                                    , Run BatteryP ["BAT0"] ["-t", "<acstatus><left>%(<timeleft>)", "-L", "10", "-H", "80", "-p", "3", "--", "-O",
                                                             "<fc=green>▲</fc>", "-i", "<fc=green>=</fc>", "-o", "<fc=yellow>▼</fc>",
                                                             "-L", "-15", "-H", "-5", "-l", "red", "-m", "blue", "-h", "green"] 200
                                    , Run Com "${pkgs.wifi-status}/bin/wifi-status" [] "wifi" 60
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["openvpn-jobvpn.service", "[V]"] "vpn" 30
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["sshuttle.service", "[S]"] "sshuttle" 30
                                    , Run Kbd [ ("us", "<fc=#ee9a00>us</fc>")
                                              , ("ru", "<fc=green>ru</fc>")
                                              ]
                                    ]
                       , sepChar = "%"
                       , alignSep = "}{"
                       , template = "%StdinReader% }{| %battery% | %wifi% %sshuttle% %vpn% | <fc=#ee9a00>%date%</fc> |%kbd%"
                       }
            '';
            ".config/rofi/oxide.rasi".text = ''
                /**
                 * Oxide Color theme
                 * Author: Diki Ananta <diki1aap@gmail.com>
                 * Repository: https://github.com/dikiaap/dotfiles
                 * License: MIT
                 **/
                * {
                    selected-normal-foreground:  @lightfg;
                    foreground:                  rgba ( 196, 202, 212, 100 % );
                    normal-foreground:           @foreground;
                    alternate-normal-background: rgba ( 42, 42, 42, 100 % );
                    red:                         rgba ( 194, 65, 65, 100 % );
                    selected-urgent-foreground:  @lightfg;
                    blue:                        rgba ( 43, 131, 166, 100 % );
                    urgent-foreground:           @lightfg;
                    alternate-urgent-background: @red;
                    active-foreground:           @lightfg;
                    lightbg:                     @foreground;
                    selected-active-foreground:  @lightfg;
                    alternate-active-background: @blue;
                    background:                  rgba ( 33, 33, 33, 100 % );
                    alternate-normal-foreground: @foreground;
                    normal-background:           @background;
                    lightfg:                     rgba ( 249, 249, 249, 100 % );
                    selected-normal-background:  rgba ( 90, 90, 90, 100 % );
                    border-color:                @foreground;
                    spacing:                     2;
                    separatorcolor:              rgba ( 183, 183, 183, 100 % );
                    urgent-background:           @red;
                    selected-urgent-background:  rgba ( 214, 78, 78, 100 % );
                    alternate-urgent-foreground: @urgent-foreground;
                    background-color:            rgba ( 0, 0, 0, 0 % );
                    alternate-active-foreground: @active-foreground;
                    active-background:           @blue;
                    selected-active-background:  rgba ( 39, 141, 182, 100 % );
                }
                window {
                    background-color: @background;
                    border:           0;
                    padding:          8;
                }
                mainbox {
                    border:  0;
                    padding: 0;
                }
                message {
                    border:       2px dash 0px 0px;
                    border-color: @separatorcolor;
                    padding:      1px;
                }
                textbox {
                    text-color: @foreground;
                }
                listview {
                    fixed-height: 0;
                    border:       0;
                    border-color: @separatorcolor;
                    spacing:      2px;
                    scrollbar:    true;
                    padding:      2px 0px 0px;
                }
                element {
                    border:  0;
                    padding: 1px;
                }
                element normal.normal {
                    background-color: @normal-background;
                    text-color:       @normal-foreground;
                }
                element normal.urgent {
                    background-color: @urgent-background;
                    text-color:       @urgent-foreground;
                }
                element normal.active {
                    background-color: @active-background;
                    text-color:       @active-foreground;
                }
                element selected.normal {
                    background-color: @selected-normal-background;
                    text-color:       @selected-normal-foreground;
                }
                element selected.urgent {
                    background-color: @selected-urgent-background;
                    text-color:       @selected-urgent-foreground;
                }
                element selected.active {
                    background-color: @selected-active-background;
                    text-color:       @selected-active-foreground;
                }
                element alternate.normal {
                    background-color: @alternate-normal-background;
                    text-color:       @alternate-normal-foreground;
                }
                element alternate.urgent {
                    background-color: @alternate-urgent-background;
                    text-color:       @alternate-urgent-foreground;
                }
                element alternate.active {
                    background-color: @alternate-active-background;
                    text-color:       @alternate-active-foreground;
                }
                scrollbar {
                    width:        4px;
                    border:       0;
                    handle-color: rgba ( 85, 85, 85, 100 % );
                    handle-width: 8px;
                    padding:      0;
                }
                sidebar {
                    border:       2px dash 0px 0px;
                    border-color: @separatorcolor;
                }
                button {
                    spacing:    0;
                    text-color: @normal-foreground;
                }
                button selected {
                    background-color: @selected-normal-background;
                    text-color:       @selected-normal-foreground;
                }
                inputbar {
                    spacing:    0px;
                    text-color: @normal-foreground;
                    padding:    1px;
                    children:   [ prompt,textbox-prompt-colon,entry,case-indicator ];
                }
                case-indicator {
                    spacing:    0;
                    text-color: @normal-foreground;
                }
                entry {
                    spacing:    0;
                    text-color: @normal-foreground;
                }
                prompt {
                    spacing:    0;
                    text-color: @normal-foreground;
                }
                textbox-prompt-colon {
                    expand:     false;
                    str:        ":";
                    margin:     0px 0.3000em 0.0000em 0.0000em;
                    text-color: inherit;
                }
            '';
            ".config/fusuma/config.yml".text = ''
                swipe:
                  3:
                    left:
                      command: '${pkgs.xdotool}/bin/xdotool key alt+Left'
                    right:
                      command: '${pkgs.xdotool}/bin/xdotool key alt+Right'
                    up:
                      command: '${pkgs.xdotool}/bin/xdotool key ctrl+t'
                      threshold: 1.5
                    down:
                      command: '${pkgs.xdotool}/bin/xdotool key ctrl+w'
                      threshold: 1.5
                  4:
                    left:
                      command: '${pkgs.xdotool}/bin/xdotool key super+Left'
                    right:
                      command: '${pkgs.xdotool}/bin/xdotool key super+Right'
                    up:
                      command: '${pkgs.xdotool}/bin/xdotool key super+a'
                    down:
                      command: '${pkgs.xdotool}/bin/xdotool key super+s'
                pinch:
                  2:
                    in:
                      command: '${pkgs.xdotool}/bin/xdotool key ctrl+plus'
                      threshold: 0.1
                    out:
                      command: '${pkgs.xdotool}/bin/xdotool key ctrl+minus'
                      threshold: 0.1

                threshold:
                  swipe: 1
                  pinch: 1

                interval:
                  swipe: 1
                  pinch: 1
            '';
            ".config/synology/nas.yml".text = ''
                nas:
                  hostname: ${config.nas.hostname}
                  users:
                    admin:
                      login: ${config.nas.primary_user}
                      password: ${config.nas.primary_user_password}
                  # FIXME: use more versatile parser, because those implemented with bash have limited functionality
                  volumes: ${builtins.concatStringsSep " " config.nas.volumes}
                  mount:
                    basedir: ${config.nas.local_mount_base}
            '';
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
                        K("C-s"): K("C-s"),
                        K("C-c"): K("C-q"),
                    },
                }, "Firefox")

                define_keymap(re.compile("TelegramDesktop"), {
                    K("C-x"): {
                        K("C-c"): K("C-q"),
                    },
                    K("C-s"): K("Esc"),
                }, "Telegram")

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
            ".gmrunrc".text = ''
                # gmrun configuration file
                # gmrun is (C) Mihai Bazon, <mishoo@infoiasi.ro>
                # GPL v2.0 applies

                # Set terminal
                Terminal = urxvt
                TermExec = ''${Terminal} -e
                AlwaysInTerm = ssh telnet ftp lynx mc vi vim pine centericq perldoc man

                # Set window geometry (except height)
                Width = 400
                Top = 100
                Left = 200

                # History size
                History = 1024

                # Shows last history line selected when invoked
                ShowLast = 1

                # Show files starting with '.'
                # Default is 0 (off), set it to 1 if you want "hidden" files to show up
                # in the completion window
                ShowDotFiles = 1

                # Timeout (in milliseconds) after which gmrun will simulate a TAB press
                # Set this to NULL if don't like this feature.
                TabTimeout = 0

                # URL handlers
                # If the entered text is "http://www.google.com" then:
                #   - %u gets replaced with the whole URL ("http://www.google.com")
                #   - %s gets replaced with "//www.google.com".  This is useful for URL-s
                #     like "man:printf" --> %s will get replaced with "printf"
                URL_http = firefox -remote "openURL(%u, new-window)"
                URL_mailto = firefox -remote "mailto(%s)"
                URL_man = ''${TermExec} 'man %s'
                URL_info = ''${TermExec} 'info %s'
                URL_pd = ''${TermExec} 'perldoc %s'
                URL_file = nautilus %s
                URL_readme = ''${TermExec} 'less /usr/doc/%s/README'
                URL_info = ''${TermExec} 'info %s'
                URL_sh = sh -c '%s'

                # extension handlers
                EXT:doc,rtf = AbiWord %s
                EXT:txt,cc,cpp,h,java,html,htm,epl,tex,latex,js,css,xml,xsl,am = emacs %s
                EXT:ps = zathura %s
                EXT:pdf = zathura %s
            '';
            # TODO: check/Nixify paths
            ".i3status.conf".text = ''
                # i3status configuration file.
                # see "man i3status" for documentation.

                # It is important that this file is edited as UTF-8.
                # The following line should contain a sharp s:
                # ß
                # If the above line is not correctly displayed, fix your editor first!

                general {
                        output_format = "dzen2"
                        colors = true
                        interval = 5
                }

                order += "tztime local"
                order += "cpu_usage"
                order += "cpu_temperature 0"
                order += "load"
                order += "disk /"
                order += "run_watch DHCP"
                order += "path_exists VPN"
                order += "wireless _first_"
                order += "battery 0"
                order += "volume master"

                wireless _first_ {
                         format_up = "W: %quality, %essid"
                         format_down = "W: down"
                }

                battery 0 {
                        format = "%status %percentage %remaining"
                }

                battery 0 {
                        format = "%status %percentage %remaining"
                        format_down = "No battery"
                        status_chr = "⚇ CHR"
                        status_bat = "⚡ BAT"
                        status_full = "☻ FULL"
                        path = "/sys/class/power_supply/BAT%d/uevent"
                        low_threshold = 10
                }

                run_watch DHCP {
                          pidfile = "/var/run/dhclient*.pid"
                }

                path_exists VPN {
                            path = "/proc/sys/net/ipv4/conf/tun0"
                }

                tztime local {
                       format = "%H:%M:%S %d-%m-%Y"
                }

                load {
                     format = "%1min"
                }

                disk "/" {
                     format = "%free"
                }

                cpu_temperature 0 {
                                format = "/ %degrees °C"
                                path = "/sys/devices/platform/coretemp.0/hwmon/hwmon1/temp2_input"
                }

                volume master {
                       format = "♪: %volume"
                       format_muted = "♪: muted (%volume)"
                       device = "pulse"
                       mixer = "Master"
                       mixer_idx = 0
                }

                cpu_usage {
                          format = "CPU: %usage"
                }
            '';
            ".config/rofi-pass/config".text = ''
                # permanently set alternative root dir. Use ":" to separate multiple roots
                # which can be switched at runtime with shift+left/right
                # root=/path/to/root

                # rofi command. Make sure to have "$@" as last argument
                _rofi () {
                    ${pkgs.rofi}/bin/rofi -i -no-auto-select "$@"
                }

                # default command to generate passwords
                _pwgen () {
                    ${pkgs.pwgen}/bin/pwgen -y "$@"
                }

                # image viewer to display qrcode of selected entry
                # qrencode is needed to generate the image and a viewer
                # that can read from pipes. Known viewers to work are feh and display
                _image_viewer () {
                    ${pkgs.feh}/bin/feh -
                }

                # xdotool needs the keyboard layout to be set using setxkbmap
                # You can do this in your autostart scripts (e.g. xinitrc)

                # If for some reason, you cannot do this, you can set the command here.
                # and set fix_layout to true
                fix_layout=false

                layout_cmd () {
                    ${pkgs.xkblayout-state}/bin/xkblayout-state set 0
                }

                # fields to be used
                URL_field='url'
                USERNAME_field='login'
                AUTOTYPE_field='autotype'

                # delay to be used for :delay keyword
                delay=2

                # rofi-pass needs to close itself before it can type passwords. Set delay here.
                wait=0.2

                # delay between keypresses when typing (in ms)
                xdotool_delay=12

                ## Programs to be used
                # Editor
                EDITOR='${pkgs.emacs}/bin/emacsclient'

                # Browser
                BROWSER='${pkgs.firefox-unwrapped}/bin/firefox'

                ## Misc settings

                default_do='menu' # menu, autotype, copyPass, typeUser, typePass, copyUser, copyUrl, viewEntry, typeMenu, actionMenu, copyMenu, openUrl
                auto_enter='false'
                notify='false'
                default_autotype='user :tab pass'

                # color of the help messages
                # leave empty for autodetection
                help_color="#4872FF"

                # Clipboard settings
                # Possible options: primary, clipboard, both
                clip=clipboard

                # Seconds before clearing pass from clipboard
                clip_clear=45

                ## Options for generating new password entries

                # open new password entries in editor
                edit_new_pass="true"

                # default_user is also used for password files that have no user field.
                #default_user="''${ROFI_PASS_DEFAULT_USER-$(whoami)}"
                #default_user2=alex3rd
                #password_length=12

                # Custom Keybindings
                autotype="Alt+1"
                type_user="Alt+2"
                type_pass="Alt+3"
                open_url="Alt+4"
                copy_name="Alt+u"
                copy_url="Alt+l"
                copy_pass="Alt+p"
                show="Alt+o"
                copy_entry="Alt+2"
                type_entry="Alt+1"
                copy_menu="Alt+c"
                action_menu="Alt+a"
                type_menu="Alt+t"
                help="Alt+h"
                switch="Alt+x"
                insert_pass="Alt+n"
            '';
        };
        services.dunst = {
            enable = true;
            settings = {
                global = {
                    alignment = "left";
                    allow_markup = "yes";
                    always_run_script = "true";
                    bounce_freq = 0;
                    browser = "firefox -new-tab";
                    dmenu = "/usr/bin/dmenu -p dunst:";
                    ellipsize = "middle";
                    follow = "keyboard";
                    font = "Iosevka Bold 10";
                    force_xinerama = "false";
                    format = "<span foreground='#F3F4F5'><b>%s %p</b></span>\n%b";
                    frame_color = "#232323";
                    frame_width = 1;
                    geometry = "300x5-15+15";
                    hide_duplicates_count = "false";
                    history_length = 20;
                    horizontal_padding = 10;
                    icon_path = "/usr/share/icons/gnome/16x16/status/:/usr/share/icons/gnome/16x16/devices/";
                    icon_position = "left";
                    idle_threshold = 120;
                    ignore_newline = "no";
                    indicate_hidden = "yes";
                    line_height = 0;
                    markup = "full";
                    max_icon_size = 32;
                    monitor = 0;
                    notification_height = 0;
                    padding = 10;
                    separator_color = "frame";
                    separator_height = 2;
                    show_age_threshold = 60;
                    show_indicators = "yes";
                    shrink = "no";
                    sort = "yes";
                    stack_duplicates = "true";
                    startup_notification = "false";
                    sticky_history = "yes";
                    transparency = 0;
                    verbosity = "mesg";
                    word_wrap = "yes";
                };
                frame = {
                    width = 3;
                    color = "#aaaaaa";
                };
                shortcuts = {
                    close = "ctrl+space";
                    close_all = "ctrl+shift+space";
                    history = "ctrl+grave";
                    context = "ctrl+shift+period";
                };
                urgency_low = {
                    background = "#232323";
                    foreground = "#A8A8A8";
                    timeout = 3;
                };
                urgency_normal = {
                    background = "#285577";
                    foreground = "#ffffff";
                    timeout = 5;
                };
                urgency_critical = {
                    background = "#D64E4E";
                    foreground = "#F0E0E0";
                    frame_color = "#D64E4E";
                    timeout = 7;
                    icon = "/usr/share/icons/gentoo/32x32/dia.png";
                };
            };
        };
        programs.feh.enable = true;
        programs.rofi = {
            enable = true;
            fullscreen = false;
            borderWidth = 1;
            colors = {
                window = {
                    background = "#fdf6e3";
                    border = "#002b36";
                    separator = "#eee8d5";
                };
                rows = {
                    active = {
                        background = "#fdf6e3";
                        foreground = "#268bd2";
                        backgroundAlt = "#eee8d5";
                        highlight = {
                            background = "#268bd2";
                            foreground = "#fdf6e3";
                        };
                    };
                    normal = {
                        background = "#fdf6e3";
                        foreground = "#002b36";
                        backgroundAlt = "#eee8d5";
                        highlight = {
                            background = "#586e75";
                            foreground = "#eee8d5";
                        };
                    };
                    urgent = {
                        background = "#fdf6e3";
                        foreground = "#dc322f";
                        backgroundAlt = "#eee8d5";
                        highlight = {
                            background = "#dc322f";
                            foreground = "#fdf6e3";
                        };
                    };

                };
            };
            cycle = true;
            rowHeight = 1;
            # scrollbar = true;
            lines = 15;
            location = "center";
            padding = 5;
            separator = "none";
            terminal = "alacritty";
            width = 80;
            xoffset = 0;
            yoffset = 0;
            font = "Iosevka Bold 12"; # TODO: templatize
            theme = "${config.users.extraUsers.alex3rd.home}/.config/rofi/oxide.rasi";
            # TODO: review https://davedavenport.github.io/rofi/manpage.html
            extraConfig = ''
                rofi.line-margin:                    3
                rofi.scroll-method:                  1
                rofi.scrollbar-width:                8
                rofi.hide-scrollbar:                 true

                rofi.modi:                           combi,drun,keys,run,ssh,window
                rofi.combi-modi:                     window,run,ssh
                rofi.matching:                       normal
                rofi.tokenize:                       true
                rofi.disable-history:                false
                rofi.levenshtein-sort:               true
                rofi.threads:                        0

                rofi.run-command:                    {cmd}
                rofi.run-shell-command:              {terminal} -e {cmd}
                rofi.window-command:                 xkill -id {window}
                rofi.window-format:                  {w}   {c}   {t}
                rofi.window-match-fields:            title,class

                rofi.parse-hosts:                    true
                rofi.parse-known-hosts:              false
                rofi.ssh-client:                     ${pkgs.eternal-terminal}/bin/et
                rofi.ssh-command:                    ${pkgs.tmux}/bin/tmux new-window '{ssh-client} ${config.network.defaultRemoteUser}@{host}'

                rofi.kb-accept-alt:                  Shift+Return
                rofi.kb-accept-custom:               Control+Return
                rofi.kb-accept-entry:                Control+j,Control+m,Return,KP_Enter
                rofi.kb-cancel:                      Escape,Control+g,Control+bracketleft
                rofi.kb-clear-line:                  Control+w
                rofi.kb-custom-10:                   Alt+0
                rofi.kb-custom-11:                   Alt+exclam
                rofi.kb-custom-12:                   Alt+at
                rofi.kb-custom-13:                   Alt+numbersign
                rofi.kb-custom-14:                   Alt+dollar
                rofi.kb-custom-15:                   Alt+percent
                rofi.kb-custom-16:                   Alt+dead_circumflex
                rofi.kb-custom-17:                   Alt+ampersand
                rofi.kb-custom-18:                   Alt+asterisk
                rofi.kb-custom-19:                   Alt+parenleft
                rofi.kb-custom-1:                    Alt+1
                rofi.kb-custom-2:                    Alt+2
                rofi.kb-custom-3:                    Alt+3
                rofi.kb-custom-4:                    Alt+4
                rofi.kb-custom-5:                    Alt+5
                rofi.kb-custom-6:                    Alt+6
                rofi.kb-custom-7:                    Alt+7
                rofi.kb-custom-8:                    Alt+8
                rofi.kb-custom-9:                    Alt+9
                rofi.kb-delete-entry:                Shift+Delete
                rofi.kb-mode-next:                   Shift+Right,Control+Tab
                rofi.kb-mode-previous:               Shift+Left,Control+Shift+Tab
                rofi.kb-move-char-back:              Left,Control+b
                rofi.kb-move-char-forward:           Right,Control+f
                rofi.kb-move-end:                    Control+e
                rofi.kb-move-front:                  Control+a
                rofi.kb-move-word-back:              Alt+b
                rofi.kb-move-word-forward:           Alt+f
                rofi.kb-page-next:                   Page_Down
                rofi.kb-page-prev:                   Page_Up
                rofi.kb-primary-paste:               Control+V,Shift+Insert
                rofi.kb-remove-char-back:            BackSpace,Control+h
                rofi.kb-remove-char-forward:         Delete,Control+d
                rofi.kb-remove-to-eol:               Control+k
                rofi.kb-remove-to-sol:               Control+u
                rofi.kb-remove-word-back:            Control+Alt+h,Control+BackSpace
                rofi.kb-remove-word-forward:         Control+Alt+d
                rofi.kb-row-down:                    Down,Control+n
                rofi.kb-row-first:                   Home,KP_Home
                rofi.kb-row-last:                    End,KP_End
                rofi.kb-row-left:                    Control+Page_Up
                rofi.kb-row-right:                   Control+Page_Down
                rofi.kb-row-select:                  Control+space
                rofi.kb-row-tab:                     Tab
                rofi.kb-row-up:                      Up,Control+p,Shift+Tab,Shift+ISO_Left_Tab
                rofi.kb-screenshot:                  Alt+S
                rofi.kb-secondary-paste:             Control+v,Insert
                rofi.kb-toggle-case-sensitivity:     grave,dead_grave
                rofi.kb-toggle-sort:                 Alt+grave
            '';
        };
        programs.browserpass.enable = true;
        services.unclutter.enable = true;
        services.udiskie.enable = true;
        services.network-manager-applet.enable = true;
        services.random-background = {
            enable = true;
            imageDirectory = "%h/blobs/wallpaper";
            interval = "1w";
        };
    };
}
