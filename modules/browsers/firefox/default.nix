{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.browsers.firefox;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos;
  standardDesktopID = "firefox";
  windowedDesktopID = "org.custom.firefox.windowed";
in
{
  options = {
    browsers.firefox = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable firefox.";
      };
      traits = mkOption {
        type = types.submodule (import ../../workstation/systemtraits/xapp-traits.nix);
        description = "Firefox application traits";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set firefox as default browser.";
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = "Set firefox as fallback browser.";
      };
      suspendInactive = mkOption {
        type = types.bool;
        default = true;
        description = "Suspend when inactive (using xsuspender)";
      };
      keyboardCentric = mkOption {
        type = types.bool;
        default = true;
        description = "Enable keyboard-centric controls (tridactyl, etc.)";
      };
      desktopID = mkOption {
        type = types.enum [ standardDesktopID windowedDesktopID ];
        default = windowedDesktopID;
        description = "Desktop entry name";
      };
      emacs.browseUrlSetup = mkOption {
        type = types.lines;
        default = ''
          (use-package browse-url
            :config
            (setq browse-url-browser-function 'browse-url-firefox)
            (setq browse-url-firefox-program "${cfg.traits.command.binary}")
            ${optionalString (cfg.traits.command.parameters != [ ]) "(setq browse-url-firefox-arguments '(${appCmdParametersQuotedSpaced cfg.traits}))"})
        '';
        visible = false;
        internal = true;
        description = "Specialized Firefox-aware `browse-url` package setup";
      };
      sessions.backup.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to backup browser session.";
      };
      sessions.saveFrequency = mkOption {
        type = types.str;
        default = "30min";
        description = ''
          How often browser sessions should be saved.
          Systemd timer notation is used.
        '';
      };
      sessions.sizeThreshold = mkOption {
        type = types.int;
        default = 10;
        description = "Maximum session size (in URLs), in which case it would be loaded completely.";
      };
      sessions.path = mkOption {
        type = types.str;
        default = homePrefix user "docs/org/browser-sessions/firefox";
        description = "Where to save plaintext Firefox session contents.";
      };
      sessions.keepMinutes = mkOption {
        type = types.int;
        default = 180;
        description = "Keep sessions age under this amount";
      };
      sessions.historyLength = mkOption {
        type = types.int;
        default = 10;
        description = "How many recent sessions to keep.";
      };
      sessions.nameTemplate = mkOption {
        type = types.str;
        default = "firefox-session-auto";
        description = "Filename template for Firefox session files.";
      };
      sessions.keepHistory = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to also backup tabs navigation history";
      };
      staging.enableSettings = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable staging settings.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Firefox-related bookmarks";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      browsers.firefox.traits = rec {
        command = {
          binary = "${pkgs.firefox-unwrapped}/bin/firefox";
          parameters = [ "--new-window" ];
        };
        wmClass = [ "Navigator" "firefox" ];
        suspensionRule = {
          Firefox = {
            suspendDelay = 10;
            matchWmClassContains = lib.last wmClass;
            suspendSubtreePattern = binaryFromCmd (appCmdFull cfg.traits);
          };
        };
      };
      workstation.input.keyboard.xremap.config = {
        keymap = [
          {
            name = "${appName cfg.traits}";
            application = { only = "${appWindowClass cfg.traits}"; };
            remap = {
              "C-j" = "C-f6"; # NOTE: Type C-j to focus to the content
              "C-g" = "f5";
              "C-n" = "C-g";
              "C-Shift-n" = "C-Shift-g";
              "M-comma" = "M-Left";
              "M-dot" = "M-Right";
              "C-x" = {
                remap = {
                  "b" = "b";
                  "k" = "C-w";
                  "u" = "C-Shift-t";
                  "C-s" = "C-s";
                  "C-c" = "C-q";
                };
              };
            };
          }
        ];
      };
      workstation.input.keyboard.xkeysnail.rc = ''
        define_keymap(re.compile("${appWindowClass cfg.traits}"), {
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
        }, "${appName cfg.traits}")
      '';

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive suspensionRule;

      home-manager.users."${user}" = {
        home.packages = with pkgs;
          [
            xsel # for firefox native clients
            (makeDesktopItem {
              name = windowedDesktopID;
              type = "Application";
              exec = "${appCmdFull cfg.traits} %U";
              comment = "Firefox that opens links preferably in new windows";
              desktopName = "Firefox";
              categories = [ "Network" "WebBrowser" ];
            })
          ];
        programs.firefox = {
          enable = true;
          profiles = {
            default = {
              name = "profile.default";
              path = "profile.default";
              settings = {
                "browser.ctrlTab.recentlyUsedOrder" = false;
                "browser.download.dir" = config.attributes.downloadPath.browser;
                "browser.link.open_newwindow" = 2;
                "browser.sessionstore.restore_on_demand" = true;
                "browser.sessionstore.restore_tabs_lazily" = true;
                "browser.shell.checkDefaultBrowser" = true;
                "browser.startup.page" = 3;
                "browser.urlbar.decodeURLsOnCopy" = true;
                "extensions.autoDisableScopes" = 0;
                "extensions.pocket.enabled" = false;
                "extensions.update.autoUpdateDefault" = false;
                "extensions.update.background.url" = "";
                "extensions.update.enabled" = false;
                "extensions.update.url" = "";
                "lightweightThemes.selectedThemeID" = "firefox-compact-dark@mozilla.org";
                "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
              } // lib.optionalAttrs cfg.staging.enableSettings {
                # entries picked from https://github.com/ilya-fedin/user-js/blob/master/user.js
                "browser.cache.disk.enable" = false;
                "browser.cache.memory.enable" = false;
                "browser.cache.offline.enable" = false;
                "browser.contentblocking.category" = "custom";
                "browser.discovery.enabled" = false;
                "browser.download.autohideButton" = false;
                "browser.newtab.preload" = false;
                "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
                "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
                "browser.newtabpage.activity-stream.feeds.snippets" = false;
                "browser.newtabpage.activity-stream.feeds.telemetry" = false;
                "browser.newtabpage.activity-stream.prerender" = false;
                "browser.newtabpage.activity-stream.showSponsored" = false;
                "browser.newtabpage.activity-stream.telemetry" = false;
                "browser.newtabpage.enabled" = false;
                "browser.ping-centre.telemetry" = false;
                "browser.safebrowsing.blockedURIs.enabled" = false;
                "browser.safebrowsing.downloads.enabled" = false;
                "browser.safebrowsing.malware.enabled" = false;
                "browser.safebrowsing.phishing.enabled" = false;
                "browser.sessionhistory.max_total_viewers" = 0;
                "browser.tabs.drawInTitlebar" = true;
                "browser.tabs.remote.separateFileUriProcess" = false;
                "browser.tabs.remote.separatePrivilegedContentProcess" = false;
                "browser.tabs.warnOnClose" = false;
                "browser.uidensity" = 1;
                "browser.urlbar.matchBuckets" = "general:5,suggestion:Infinity";
                "datareporting.healthreport.uploadEnabled" = false;
                "dom.ipc.processCount" = 1;
                "dom.largeAllocationHeader.enabled" = false;
                "dom.push.enabled" = false;
                "dom.serviceWorkers.enabled" = false;
                "dom.webnotifications.enabled" = false;
                "extensions.webextensions.remote" = false;
                "general.aboutConfig.showWarning" = false;
                "geo.enabled" = false;
                "gfx.webrender.all" = true;
                "intl.accept_languages" = "ru,ru-ru,en-us,en";
                "intl.locale.requested" = "ru";
                "layers.gpu-process.enabled" = false;
                "layout.css.devPixelsPerPx" = "1"; # "1.25"
                "media.autoplay.block-event.enabled" = true;
                "media.autoplay.block-webaudio" = true;
                "media.autoplay.default" = 5;
                "media.navigator.enabled" = false;
                "media.peerconnection.enabled" = false;
                "network.cookie.cookieBehavior" = 4;
                "network.dns.disablePrefetch" = true;
                "network.predictor.enabled" = false;
                "network.prefetch-next" = false;
                "network.proxy.type" = 0;
                "network.tcp.tcp_fastopen_enable" = true;
                "network.trr.mode" = 5; # NOTE: see https://wiki.mozilla.org/Trusted_Recursive_Resolver for reference
                # "network.security.esni.enabled" = true; (RKN hates ESNI)
                "privacy.donottrackheader.enabled" = true;
                "privacy.resistFingerprinting" = true;
                "privacy.trackingprotection.cryptomining.enabled" = true;
                "privacy.trackingprotection.enabled" = true;
                "privacy.trackingprotection.fingerprinting.enabled" = true;
                "security.sandbox.content.level" = 2;
                "urlclassifier.trackingTable" = ""; # because 2md layer list blocks google captcha, use 1st layer
              };
              extensions = with nurpkgs.rycee.firefox-addons;
                [
                  browserpass
                  clearurls
                  greasemonkey

                  # nurpkgs.wiedzmin.firefox-addons.url-in-title # NOTE: derivation is temporarily broken at nur-packages
                ] ++ optionals cfg.keyboardCentric [ tridactyl ];
              userChrome = ''
                #TabsToolbar { visibility: collapse !important; }
                #titlebar { visibility: collapse !important; }
              '';
            };
          };
        };
        xdg.configFile."tridactyl/tridactylrc".text = ''
          set storageloc local

          set historyresults 100

          colorscheme dark

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
          set hintchars ${config.workstation.input.core.hints.alphabet}
          set hintuppercase false
          set hintnames uniform

          set tabopenpos last

          set yankto both
          set putfrom clipboard

          " Make Tridactyl work on more sites at the expense of some security

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

          set keytranslatemodes.ignoremaps true
          set keytranslatemodes.hintmaps true
        '';
        home.file = {
          ".mozilla/firefox/profile.default/browser-extension-data/{d47d18bc-d6ba-4f96-a144-b3016175f3a7}/storage.js".text =
            builtins.toJSON {
              protocol = false;
              path = true;
              delimiter = " // ";
            };
          ".mozilla/native-messaging-hosts/passff.json".text = builtins.toJSON {
            name = "passff";
            description = "Host for communicating with zx2c4 pass";
            path = "${pkgs.passff-host}/share/passff-host/passff.py";
            type = "stdio";
            allowed_extensions = [ "passff@invicem.pro" ];
          };
          ".mozilla/native-messaging-hosts/tridactyl.json".text = builtins.toJSON {
            name = "tridactyl";
            description = "Tridactyl native command handler";
            path = "${pkgs.tridactyl-native}/bin/native_main";
            type = "stdio";
            allowed_extensions = [
              "tridactyl.vim@cmcaine.co.uk"
              "tridactyl.vim.betas@cmcaine.co.uk"
              "tridactyl.vim.betas.nonewtab@cmcaine.co.uk"
            ];
          };
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: firefox: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.chromium.isDefault && !config.browsers.nyxt.isDefault
            && !config.browsers.qutebrowser.isDefault;
          message = "browsers: firefox: there should be exactly one default.";
        }
      ];

      home-manager.users."${user}" = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "${cfg.desktopID}.desktop";
      };
      services.xserver.displayManager.sessionCommands = ''
        ${pkgs.xdg-utils}/bin/xdg-settings set default-web-browser ${cfg.desktopID}.desktop
      '';
      attributes.browser.default.traits = cfg.traits;

      browsers.ext.emacs.browseUrlSetup = cfg.emacs.browseUrlSetup;

      shell.core.variables = [{
        TB_DEFAULT_BROWSER = appCmdFull cfg.traits;
        TB_DEFAULT_BROWSER_SESSIONS_STORE = cfg.sessions.path;
        TB_FIREFOX_SESSIONS_KEEP_MINUTES = builtins.toString cfg.sessions.keepMinutes;
        global = true;
      }];

      workstation.performance.warmup.paths = [ (homePrefix user ".mozilla") ];

      navigation.bookmarks.entries = {
        "firefox/sessions/exported" = { local.path = homePrefix user "docs/org/browser-sessions/firefox"; };
      };
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: firefox: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.chromium.isFallback && !config.browsers.nyxt.isFallback
            && !config.browsers.qutebrowser.isFallback;
          message = "browsers: firefox: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback.traits = cfg.traits;

      shell.core.variables = [{ TB_FALLBACK_BROWSER = appCmdFull cfg.traits; global = true; }];
    })
    (mkIf (cfg.enable && cfg.sessions.backup.enable) {
      home-manager.users."${user}" = {
        home.activation.ensureFirefoxExportedSessionsPath = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.sessions.path}";
        };
      };
      systemd.user.services."backup-current-session-firefox" = {
        description = "Backup current firefox session (tabs)";
        path = [ pkgs.xkb-switch ];
        serviceConfig = {
          Type = "oneshot";
          Environment = [ "TB_FIREFOX_SESSIONS_KEEP_MINUTES=${builtins.toString cfg.sessions.keepMinutes}" ];
          ExecStart = ''${nurpkgs.wiedzmin.toolbox}/bin/ffsessions --dumps-path ${cfg.sessions.path} dump ${
              if cfg.sessions.keepHistory then "-k" else ""}'';
          ExecStopPost = ''${nurpkgs.wiedzmin.toolbox}/bin/ffsessions --dumps-path ${cfg.sessions.path} rotate'';
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."backup-current-session-firefox" =
        renderTimer "Backup current firefox session (tabs)" cfg.sessions.saveFrequency cfg.sessions.saveFrequency ""
          false "";
    })
    (mkIf (cfg.enable && cfg.sessions.backup.enable && cfg.isDefault) {
      wmCommon.keybindings.entries = [
        {
          key = [ "s" ];
          cmd = "${nurpkgs.wiedzmin.toolbox}/bin/ffsessions --dumps-path ${cfg.sessions.path} dump ${
              if cfg.sessions.keepHistory then "-k" else ""}";
          mode = "browser";
        }
        {
          key = [ "e" ];
          cmd = "${nurpkgs.wiedzmin.toolbox}/bin/ffsessions --dumps-path ${cfg.sessions.path} edit";
          mode = "browser";
        }
        {
          key = [ "d" ];
          cmd = "${nurpkgs.wiedzmin.toolbox}/bin/ffsessions --dumps-path ${cfg.sessions.path} remove";
          mode = "browser";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "addons/firefox" = {
          desc = "Firefox addons";
          remote = {
            url = "https://addons.mozilla.org/en-US/firefox/";
            searchSuffix = "search/?cat=all&x=0&y=0&q=";
          };
        };
        "about/config" = {
          desc = "Firefox configuration options";
          remote.url = "about:config";
        };
        "about/memory" = {
          desc = "Firefox addons reference";
          remote.url = "about:memory";
        };
      };
    })
  ];
}
