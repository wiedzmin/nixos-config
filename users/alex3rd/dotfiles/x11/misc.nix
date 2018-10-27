{config, pkgs, lib, ...}:

{
    environment.etc."Xmodmap".text = ''
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
            # custom
            rescale-wallpaper

            # NAS
            mount_nas_volume
            unmount_nas_volume
        ];
        home.file = {
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

                ${builtins.readFile "/etc/nixos/private/categorize_private.cfg"}
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
                                    , Run CoreTemp ["-t","<core0>/<core1>°C",
                                                    "-L","40","-H","60",
                                                    "-l","lightblue",
                                                    "-n","gray90","-h","red"] 50
                                    , Run Com "${pkgs.wifi-status}/bin/wifi-status" [] "wifi" 60
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["openvpn-jobvpn.service", "[V]"] "vpn" 30
                                    , Run Com "${pkgs.systemctl-status}/bin/systemctl-status" ["sshuttle.service", "[S]"] "sshuttle" 30
                                    , Run Com "${pkgs.status_uptime}/bin/status_uptime" [] "uptime" 600
                                    , Run Kbd [ ("us", "<fc=#ee9a00>us</fc>")
                                              , ("ru", "<fc=green>ru</fc>")
                                              ]
                                    ]
                       , sepChar = "%"
                       , alignSep = "}{"
                       , template = "%StdinReader% }{| %battery% | %coretemp% | %wifi% %sshuttle% %vpn% | %uptime% | <fc=#ee9a00>%date%</fc> |%kbd%"
                       }
            '';
            ".config/tridactyl/tridactylrc".source = ./tridactylrc;
            "tridactylrc".source = ./tridactylrc;
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
                    timeout = 10;
                };
                urgency_normal = {
                    background = "#285577";
                    foreground = "#ffffff";
                    timeout = 10;
                };
                urgency_critical = {
                    background = "#D64E4E";
                    foreground = "#F0E0E0";
                    frame_color = "#D64E4E";
                    timeout = 0;
                    icon = "/usr/share/icons/gentoo/32x32/dia.png";
                };
            };
        };
        programs.feh.enable = true;
        programs.rofi = {
            enable = true;
            fullscreen = true;
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
