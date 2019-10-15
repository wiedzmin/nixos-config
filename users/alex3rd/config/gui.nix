{ config, pkgs, lib, ... }:
with import ../../../pkgs/util.nix { inherit lib config; };
with import ../secrets/const.nix { inherit config pkgs lib; };
let
  custom = import ../../../pkgs/custom pkgs config;
  userCustom = import ../custom pkgs config;
  firefox-addons = pkgs.recurseIntoAttrs (pkgs.callPackage ../../../pkgs/firefox-addons { });
in {
  imports = [ ./wm/xmonad.nix ];
  services = {
    xserver = {
      enable = true;
      startDbusSession = true;
      videoDrivers = [ "modesetting" ];
      useGlamor = true;
      exportConfiguration = true;
      desktopManager = {
        xterm.enable = false;
        gnome3.enable = false;
        default = "none";
      };
      displayManager = {
        lightdm = {
          enable = true;
          background = "black";
          greeters.mini = {
            enable = true;
            user = config.attributes.mainUser.name;
          };
        };
        gdm.enable = false;
        job = {
          logToFile = true;
          logToJournal = true;
        };
        sessionCommands = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          ${pkgs.wmname}/bin/wmname LG3D
        '';
      };
      autoRepeatDelay = 200;
      autoRepeatInterval = 40;
      xkbOptions = "caps:none";
      layout = "us,ru";
      libinput.enable = false;
      multitouch = {
        enable = true;
        invertScroll = true;
        ignorePalm = true;
        tapButtons = false;
        additionalOptions = ''
          Option        "ButtonIntegrated" "true"
          Option        "ButtonMoveEmulate" "false"
          Option        "ClickTime" "25"
          Option        "EdgeBottomSize" "5"
          Option        "FingerHigh" "5"
          Option        "FingerLow" "1"
          Option        "Hold1Move1StationaryMaxMove" "1000"
          Option        "IgnoreThumb" "true"
          Option        "ScrollCoastDuration" "600"
          Option        "ScrollCoastEnableSpeed" "0.05"
          Option        "ScrollDistance" "100"
          Option        "ScrollSensitivity" "0"
          Option        "Sensitivity" "0.3"
          Option        "SwipeDistance" "700"
          Option        "SwipeDownButton" "0"
          Option        "SwipeLeftButton" "8"
          Option        "SwipeRightButton" "9"
          Option        "SwipeUpButton" "0"
          Option        "TapButton4" "0"
          Option        "ThumbRatio" "70"
          Option        "ThumbSize" "25"
        '';
      };
    };
    arbtt.enable = true;
  };
  programs.light.enable = true;

  environment.systemPackages = with pkgs; [ gmrun xorg.xhost xorg.xmessage ];
  home-manager.users."${config.attributes.mainUser.name}" = {
    programs.autorandr = {
      enable = true;
      hooks = {
        predetect = { "kill-compton" = "${custom.kill-compton}/bin/kill-compton"; };
      };
      profiles = {
        "mobile" = {
          fingerprint = {
            "LVDS-1" =
              "00ffffffffffff0030e4d8020000000000160103801c1078ea8855995b558f261d505400000001010101010101010101010101010101601d56d85000183030404700159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d534c42330059";
          };
          config = {
            "LVDS-1" = {
              enable = true;
              primary = true;
              position = "0x0";
              mode = "1366x768";
              gamma = "1.0:0.909:0.833";
              rate = "60.10";
            };
          };
        };
        "docked-home" = {
          fingerprint = {
            "HDMI-2" =
              "00ffffffffffff001e6dbc594f53010006170103803c2278ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c450056512100001e000000fd00384b1e530f000a202020202020000000fc003237454133330a202020202020000000ff0033303652414e4e324a3836330a00dd";
            "HDMI-3" =
              "00ffffffffffff000469b124010101011d18010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0045374c4d51533037373132380a0023";
            "LVDS-1" =
              "00ffffffffffff0030e4d8020000000000160103801c1078ea8855995b558f261d505400000001010101010101010101010101010101601d56d85000183030404700159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d534c42330059";
          };
          config = {
            "HDMI-2" = {
              enable = true;
              position = "0x0";
              mode = "1920x1080";
              gamma = "1.0:0.909:0.833";
              rate = "60.00";
            };
            "HDMI-3" = {
              enable = true;
              position = "1366x1080";
              mode = "1920x1080";
              gamma = "1.0:0.909:0.833";
              rate = "60.00";
              rotate = "left";
            };
            "LVDS-1" = {
              enable = true;
              primary = true;
              position = "0x1080";
              mode = "1366x768";
              gamma = "1.0:0.909:0.833";
              rate = "60.10";
            };
          };
        };
        "docked-office" = {
          fingerprint = {
            "HDMI-2" =
              "00ffffffffffff0009d111804554000015180103803420782e4ca5a7554da226105054a56b8061c0810081809500d1c0b300a9400101283c80a070b023403020360006442100001a000000ff004e354530373434373031390a20000000fd00324c1e5311000a202020202020000000fc0042656e5120424c323431310a20003e";
            "HDMI-3" =
              "00ffffffffffff000469b124010101010f17010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0044344c4d51533034313530370a003e";
            "LVDS-1" =
              "00ffffffffffff0030e4d8020000000000160103801c1078ea8855995b558f261d505400000001010101010101010101010101010101601d56d85000183030404700159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d534c42330059";
          };
          config = {
            "HDMI-2" = {
              enable = true;
              position = "1366x1200";
              mode = "1920x1200";
              gamma = "1.0:0.909:0.833";
              rate = "59.95";
            };
            "HDMI-3" = {
              enable = true;
              position = "0x0";
              mode = "1920x1200";
              gamma = "1.0:0.909:0.833";
              rate = "59.95";
            };
            "LVDS-1" = {
              enable = true;
              primary = true;
              position = "0x1200";
              mode = "1366x768";
              gamma = "1.0:0.909:0.833";
              rate = "60.10";
            };
          };
        };
        "undocked-parents-dsub" = {
          fingerprint = {
            "VGA-1" =
              "00ffffffffffff004c2d0e0139314a4d100f01036c261e782aee95a3544c99260f5054bfef808180714f010101010101010101010101302a009851002a4030701300782d1100001e000000fd00384b1e510e000a202020202020000000fc0053796e634d61737465720a2020000000ff00485348593430323338330a202000d2";
            "LVDS-1" =
              "00ffffffffffff0030e4d8020000000000160103801c1078ea8855995b558f261d505400000001010101010101010101010101010101601d56d85000183030404700159c1000001b000000000000000000000000000000000000000000fe004c4720446973706c61790a2020000000fe004c503132355748322d534c42330059";
          };
          config = {
            "VGA-1" = {
              enable = true;
              position = "0x0";
              mode = "1280x1024";
              gamma = "1.0:0.909:0.833";
              rate = "60.02";
            };
            "LVDS-1" = {
              enable = true;
              primary = true;
              position = "0x1024";
              mode = "1366x768";
              gamma = "1.0:0.909:0.833";
              rate = "60.00";
            };
          };
        };
      };
    };
    programs.firefox = {
      enable = true;
      package = pkgs.firefox.overrideAttrs (attrs: { enableTridactylNative = true; });
      extensions = with firefox-addons; [
        display-anchors
        ghosttext
        passff
        tridactyl
        url-in-title
        web_media_controller
      ];
      profiles = {
        default = {
          name = "profile.default";
          path = "profile.default";
          settings = {
            "extensions.autoDisableScopes" = 0;
            "browser.ctrlTab.recentlyUsedOrder" = false;
            "browser.download.dir" = "/home/${config.attributes.mainUser.name}/Downloads";
            "browser.link.open_newwindow" = 2;
            "browser.sessionstore.restore_on_demand" = true;
            "browser.sessionstore.restore_tabs_lazily" = true;
            "browser.shell.checkDefaultBrowser" = true;
            "browser.startup.page" = 3;
            "extensions.pocket.enabled" = false;
            "lightweightThemes.selectedThemeID" = "firefox-compact-dark@mozilla.org";
          };
          handlers = {
            defaultHandlersVersion = { "en-US" = 4; };
            mimeTypes = { "application/pdf" = { action = 3; }; };
            schemes = {
              mailto = {
                action = 4;
                handlers = [
                  null
                  {
                    name = "Gmail";
                    uriTemplate = "https://mail.google.com/mail/?extsrc=mailto&url=%s";
                  }
                ];
              };
              "org-protocol" = { action = 4; };
              "tg" = { action = 4; };
            };
          };
        };
      };
    };
    programs.chromium = {
      enable = true;
      extensions = [
        "gfbliohnnapiefjpjlpjnehglfpaknnc" # Surfingkeys
        "ignpacbgnbnkaiooknalneoeladjnfgb" # Url in title
        "poahndpaaanbpbeafbkploiobpiiieko" # Display anchors
        # "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
        # "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
        # "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
        # "naepdomgkenhinolocfifgehidddafch" # Browserpass
        # "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix
      ];
    };
    home.file = {
      ".mozilla/firefox/profile.default/browser-extension-data/{d47d18bc-d6ba-4f96-a144-b3016175f3a7}/storage.js".text =
        builtins.toJSON {
          protocol = false;
          path = true;
          delimiter = " // ";
        };
      ".mozilla/native-messaging-hosts/me.f1u77y.web_media_controller.json".text = builtins.toJSON {
        name = "me.f1u77y.web_media_controller";
        description = "Allows controlling embedded players (YT, etc) via MPRIS";
        path = "${pkgs.wmc-mpris}/bin/web-media-controller";
        type = "stdio";
        allowed_extensions = [ "web-media-controller@f1u77y.me" ];
      };
      ".mozilla/native-messaging-hosts/passff.json".text = builtins.toJSON {
        name = "passff";
        description = "Host for communicating with zx2c4 pass";
        path = "${pkgs.passff-host}/share/passff-host/passff.py";
        type = "stdio";
        allowed_extensions = [ "passff@invicem.pro" ];
      };
    };
    xdg.configFile."tridactyl/tridactylrc".text = ''
      set storageloc local

      set historyresults 100

      colorscheme dark

      guiset_quiet tabs autohide
      guiset_quiet navbar autohide
      guiset_quiet hoverlink top-right

      " Comment toggler for Reddit and Hacker News
      bind ;c hint -c [class*="expand"],[class="togg"]

      bind qnt composite js javascript:location.href="org-protocol:///capture?template=nt&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qnc composite js javascript:location.href="org-protocol:///capture?template=nc&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qet composite js javascript:location.href="org-protocol:///capture?template=et&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qec composite js javascript:location.href="org-protocol:///capture?template=ec&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qxt composite js javascript:location.href="org-protocol:///capture?template=xt&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qxc composite js javascript:location.href="org-protocol:///capture?template=xc&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qd composite js javascript:location.href="org-protocol:///capture?template=d&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qjt composite js javascript:location.href="org-protocol:///capture?template=jt&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qjc composite js javascript:location.href="org-protocol:///capture?template=jc&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qjr composite js javascript:location.href="org-protocol:///capture?template=jr&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())
      bind qm composite js javascript:location.href="org-protocol:///capture?template=m&url=" + encodeURIComponent(location.href) + "&title=" + encodeURIComponent(document.title) + "&body=" + encodeURIComponent(window.getSelection())

      bind <A-y> clipboard yank

      "
      " Misc settings
      "

      " set editorcmd to emacsclient, or use the defaults on other platforms
      js tri.browserBg.runtime.getPlatformInfo().then(os=>{const editorcmd = os.os=="linux" ? "emacsclient" : "auto"; tri.config.set("editorcmd", editorcmd)})

      " Sane hinting mode
      set hintfiltermode vimperator-reflow
      set hintchars 4327895610
      set hintuppercase false
      set hintnames numeric

      set tabopenpos last

      set yankto both
      set putfrom clipboard

      " Make Tridactyl work on more sites at the expense of some security
      set csp clobber
      fixamo_quiet

      " Make quickmarks for the sane Tridactyl issue view
      quickmark T https://github.com/cmcaine/tridactyl/issues?utf8=%E2%9C%93&q=sort%3Aupdated-desc+
      quickmark t https://github.com/tridactyl/tridactyl

      " Map keys between layouts
      keymap ё `
      keymap й q
      keymap ц w
      keymap у e
      keymap к r
      keymap е t
      keymap н y
      keymap г u
      keymap ш i
      keymap щ o
      keymap з p
      keymap х [
      keymap ъ ]
      keymap ф a
      keymap ы s
      keymap в d
      keymap а f
      keymap п g
      keymap р h
      keymap о j
      keymap л k
      keymap д l
      keymap ж ;
      keymap э '
      keymap я z
      keymap ч x
      keymap с c
      keymap м v
      keymap и b
      keymap т n
      keymap ь m
      keymap б ,
      keymap ю .
      keymap Ё ~
      keymap Й Q
      keymap Ц W
      keymap У E
      keymap К R
      keymap Е T
      keymap Н Y
      keymap Г U
      keymap Ш I
      keymap Щ O
      keymap З P
      keymap Х {
      keymap Ъ }
      keymap Ф A
      keymap Ы S
      keymap В D
      keymap А F
      keymap П G
      keymap Р H
      keymap О J
      keymap Л K
      keymap Д L
      keymap Ж :
      keymap Э "
      keymap Я Z
      keymap Ч X
      keymap С C
      keymap М V
      keymap И B
      keymap Т N
      keymap Ь M
      keymap Б <
      keymap Ю >

      keymap <C-х> <C-[>
      keymap пш gi
      keymap пп gg
      keymap нн yy
      keymap нс yc

      keymap . /
    '';
    xdg.configFile."xsuspender.conf".text = genIni {
      Default = {
        suspend_delay = 10;
        resume_every = 50;
        resume_for = 5;
        only_on_battery = true;
        auto_suspend_on_battery = true;
        send_signals = true;
      };
      Firefox = { match_wm_class_contains = "Firefox"; };
      Chromium = { match_wm_class_contains = "Chromium-browser"; };
    };
    home.file = {
      ".local/share/applications/defaults.list" = {
        text = ''
          [Default Applications]
          application/pdf=zathura.desktop
        '';
      };
      "${snippetsFile}".text = ''
        ${lib.concatStringsSep "\n" snippetsInventory}
      '';
      ".mpv/config".text = ''
        hwdec=vdpau
        hwdec-codecs=all

        vo=gpu,xv
        ao=pulse

        af=scaletempo
        audio-samplerate=48000

        slang = en
        alang = en,eng,us

        volume-max=200
        cache-default = 250000 # Use 250MB cache for network streams

        # Always use 1080p+ or 60 fps where available. Prefer VP9
        # over AVC and VP8 for high-resolution streams.
        ytdl=yes
        ytdl-format=(bestvideo[ext=webm]/bestvideo[height>720]/bestvideo[fps=60])[tbr<13000]+(bestaudio[acodec=opus]/bestaudio[ext=webm]/bestaudio)/best
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
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
          ==> tag project:$1-$2,
        current window ($program == "Alacritty" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
          ==> tag project:$1-$2,

        -- personal projects
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/!)
          ==> tag project:$1-$2,
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.xmonad/!) ==> tag project:xmonad-config,
        current window ($program == "Alacritty" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.xmonad/!) ==> tag project:xmonad-config,
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.emacs.d/!) ==> tag project:emacs-config,
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
    };
    xdg.configFile."mc/mc.ext".text = ''
      regex/\.([pP][dD][fF])$
          Include=ebook

      regex/\.([dD][jJ][vV][uU])$
          Include=ebook

      # modify the include/video section
      include/ebook
          Open=(zathura %f >/dev/null 2>&1 &)
    '';
    # IDEA: make PoC XMonad module
    xdg.configFile."xmobar/xmobarrc".text = ''
      Config { ${lib.optionalString (config.attributes.fonts.xmobar != "") ''font = "${config.attributes.fonts.xmobar}"''}
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
                          , Run Com "${custom.wifi-status}/bin/wifi-status" [] "wifi" 60
                          , Run Kbd [ ("us", "<fc=#ee9a00>us</fc>")
                                    , ("ru", "<fc=green>ru</fc>")
                                    ]
                          ]
             , sepChar = "%"
             , alignSep = "}{"
             , template = "%StdinReader% }{| %battery% | %wifi% | <fc=#ee9a00>%date%</fc> |%kbd%"
             }
    '';
    xdg.configFile."xkeysnail/config.py".text = ''
      # -*- coding: utf-8 -*-

      import re
      from xkeysnail.transform import *

      define_conditional_modmap(re.compile(r'Emacs'), {
          Key.RIGHT_CTRL: Key.ESC,
      })

      define_keymap(re.compile("Firefox"), {
          K("C-j"): K("C-f6"), # Type C-j to focus to the content
          K("C-g"): K("f5"),
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
    gtk = {
      enable = true;
      theme.name = "Adwaita-dark";
      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome3.adwaita-icon-theme;
      };
      gtk2.extraConfig = ''
        gtk-cursor-theme-name = capitaine-cursors;
      '';
      gtk3.extraConfig = { gtk-cursor-theme-name = lib.mkForce "capitaine-cursors"; };
    };
    services.dunst = {
      enable = true;
      settings = {
        global = {
          alignment = "left";
          always_run_script = "true";
          bounce_freq = 0;
          browser = "${pkgs.firefox-unwrapped}/bin/firefox -new-tab";
          dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst:";
          ellipsize = "middle";
          follow = "keyboard";
          force_xinerama = "false";
          format = "<span foreground='#F3F4F5'><b>%s %p</b></span>\\n%b";
          frame_color = "#232323";
          frame_width = 3;
          geometry = "300x5-15+15";
          hide_duplicates_count = "false";
          history_length = 20;
          horizontal_padding = 10;
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
        };
      };
    };
    programs.feh.enable = true;
    programs.zathura = {
      enable = true;
      options = {
        pages-per-row = 1;
      };
    };
    services.compton = {
      enable = true;
      fade = true;
      fadeDelta = 5;
      fadeSteps = [ "0.04" "0.04" ];
      backend = "glx";
      vSync = "opengl-swc";
      package = pkgs.compton-git;
      opacityRule = [ "70:class_g = 'Alacritty'" ];
      extraOptions = ''
        clear-shadow = true;
        glx-no-rebind-pixmap = true;
        glx-no-stencil = true;
        paint-on-overlay = true;
        xrender-sync-fence = true;
      '';
    };
    services.redshift = {
      enable = true;
      latitude = "${redshiftLatitude}";
      longitude = "${redshiftLongitude}";
      temperature.day = 5500;
      temperature.night = 3100;
      brightness.day = "1.0";
      brightness.night = "0.7";
      extraOptions = [ "-v" "-m randr" ];
    };
    programs.rofi = {
      enable = true;
      fullscreen = false;
      borderWidth = 1;
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
      theme = "gruvbox-dark-hard";
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
        rofi.ssh-command:                    ${pkgs.tmux}/bin/tmux new-window '{ssh-client} {host}'

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
    services.udiskie = {
      enable = true;
      automount = true;
      notify = true;
      tray = "never";
    };
    xresources.properties = {
      "Xmessage*Buttons" = "Quit";
      "Xmessage*defaultButton" = "Quit";
      "Xmessage*international" = true;

      "Emacs.FontBackend" = "xft,x";
      "Emacs.menuBar" = "0";
      "Emacs.toolBar" = "0";
      "Emacs.verticalScrollBars" = false;

      "urgentOnBell" = true;
      "visualBell" = true;

      "Xft.antialias" = true;
      "Xft.autohint" = false;
      "Xft.dpi" = "120.0";
      "Xft.hinting" = true;
      "Xft.hintstyle" = "hintslight";
      "Xft.lcdfilter" = "lcddefault";
      "Xft.rgba" = "none";
    };
    xsession.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    };
  };
  services.unclutter-xfixes = {
    enable = true;
    timeout = 2;
    threshold = 15;
    extraOptions = [ "exclude-root" "fork" "ignore-scrolling" ];
  };
}
