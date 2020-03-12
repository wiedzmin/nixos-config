{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.browsers;
  firefox-addons = pkgs.recurseIntoAttrs (pkgs.callPackage ../../../nix/firefox-addons { });
in {
  # TODO: extract options
  options = {
    custom.browsers = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable some browsers'.
        '';
      };
      downloadPath = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/Downloads";
        description = ''
          Common downloads path'.
        '';
      };
      firefox.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Firefox.
        '';
      };
      chromium.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Chromium.
        '';
      };
      chromium.extraOpts = mkOption {
        type = types.attrs;
        description = ''
          Extra chromium policy options, see
          <link xlink:href="https://www.chromium.org/administrators/policy-list-3">https://www.chromium.org/administrators/policy-list-3</link>
          for a list of available options
        '';
        default = { };
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
        description = ''
          Maximum session size (in URLs), in which case it would be loaded completely.
        '';
      };
      sessions.firefox.backup.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to backup Firefox session.
        '';
      };
      sessions.firefox.path = mkOption {
        type = types.str;
        default = "/home/${config.attributes.mainUser.name}/docs/org/firefox";
        description = ''
          Where to save plaintext Firefox session contents.
        '';
      };
      sessions.firefox.historyLength = mkOption {
        type = types.int;
        default = 10;
        description = ''
          How many recent sessions to keep.
        '';
      };
      sessions.firefox.nameTemplate = mkOption {
        type = types.str;
        default = "firefox-session-auto";
        description = ''
          Filename template for Firefox session files.
        '';
      };
      aux.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Next, w3m and such.
        '';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Emacs browsers-related setup.
        '';
      };
      staging.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable staging settings.";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${config.attributes.mainUser.name}" = { programs.browserpass.enable = true; };
    })
    (mkIf (cfg.enable && cfg.firefox.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs;
          [
            xsel # for firefox native clients
          ];
        home.activation.ensureFirefoxHandlers = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          data = "rm -f /home/${config.attributes.mainUser.name}/.mozilla/firefox/profile.default/handlers.json";
        };
        programs.firefox = {
          enable = true;
          extensions = with firefox-addons; [
            display-anchors
            ghosttext
            passff
            tridactyl # TODO: review and maybe redo manually csp/fixamo customizations
            url-in-title
            web_media_controller
          ];
          profiles = {
            default = {
              name = "profile.default";
              path = "profile.default";
              settings = {
                "browser.ctrlTab.recentlyUsedOrder" = false;
                "browser.download.dir" = cfg.downloadPath;
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
              } // lib.optionalAttrs (cfg.staging.enable) {
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
                "network.trr.mode" = 2; # "network.security.esni.enabled" = true; (RKN hates ESNI)
                "privacy.donottrackheader.enabled" = true;
                "privacy.resistFingerprinting" = true;
                "privacy.trackingprotection.cryptomining.enabled" = true;
                "privacy.trackingprotection.enabled" = true;
                "privacy.trackingprotection.fingerprinting.enabled" = true;
                "security.sandbox.content.level" = 2;
                "urlclassifier.trackingTable" = ""; # because 2md layer list blocks google captcha, use 1st layer
              };
              userChrome = ''
                /* hides the native tabs */
                #TabsToolbar {
                  visibility: collapse;
                }

                #titlebar {
                  visibility: collapse;
                }

                #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
                  visibility: collapse !important;
                }
              '';
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
          set hintchars 4327895610
          set hintuppercase false
          set hintnames numeric

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
          ".mozilla/native-messaging-hosts/tridactyl.json".text = builtins.toJSON {
            name = "tridactyl";
            description = "Tridactyl native command handler";
            path = "${pkgs.tridactyl-native}/share/tridactyl/native_main.py";
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
    (mkIf (cfg.enable && cfg.chromium.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
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
      };
      environment.etc."chromium/policies/managed/extra.json".text = builtins.toJSON cfg.chromium.extraOpts;
      # chrome-export
    })
    (mkIf (cfg.enable && cfg.firefox.enable && cfg.sessions.firefox.backup.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        dump_firefox_session = pkgs.writeShellScriptBin "dump_firefox_session" (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./dump_firefox_session.sh; })));
        rotate_firefox_session_dumps = pkgs.writeShellScriptBin "rotate_firefox_session_dumps" (builtins.readFile
          (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./rotate_firefox_session_dumps.sh; })));
        collect_links_on_page = writePythonScriptWithPythonPackages "collect_links_on_page" [
          pkgs.python3Packages.beautifulsoup4
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.notify2
        ] (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./collect_links_on_page.py; })));
        manage_firefox_sessions = writePythonScriptWithPythonPackages "manage_firefox_sessions" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.notify2
          pkgs.dump_firefox_session
        ] (builtins.readFile (pkgs.substituteAll
          ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./manage_firefox_sessions.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.activation.ensureFirefoxSessionsPath = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.sessions.firefox.path}";
        };
        home.packages = with pkgs; [ dump_firefox_session manage_firefox_sessions rotate_firefox_session_dumps ];
      };
      wm.xmonad.keybindings = lib.optionalAttrs (config.wm.xmonad.enable) {
        "M-b s s" = ''spawn "${pkgs.manage_firefox_sessions}/bin/manage_firefox_sessions --save"'';
        "M-b s o" = ''spawn "${pkgs.manage_firefox_sessions}/bin/manage_firefox_sessions --open"'';
        "M-b s e" = ''spawn "${pkgs.manage_firefox_sessions}/bin/manage_firefox_sessions --edit"'';
        "M-b s d" = ''spawn "${pkgs.manage_firefox_sessions}/bin/manage_firefox_sessions --delete"'';
        "M-b s c" = ''spawn "${pkgs.collect_links_on_page}/bin/collect_links_on_page"'';
      };
      systemd.user.services."backup-current-session-firefox" = {
        description = "Backup current firefox session (tabs)";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.dump_firefox_session}/bin/dump_firefox_session";
          ExecStopPost = "${pkgs.rotate_firefox_session_dumps}/bin/rotate_firefox_session_dumps";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."backup-current-session-firefox" =
        renderTimer "Backup current firefox session (tabs)" cfg.sessions.saveFrequency cfg.sessions.saveFrequency "";
    })
    (mkIf (cfg.enable && cfg.aux.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ w3m-full webmacs ] ++ lib.optionals (cfg.staging.enable) [ next ];
        programs.emacs.extraPackages = epkgs: [ epkgs.atomic-chrome ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./browsers.el; }));
    })
  ];
}
