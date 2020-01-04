{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.browsers;
  firefox-addons = pkgs.recurseIntoAttrs (pkgs.callPackage ../../nix/firefox-addons { });
  manage_firefox_sessions = writePythonScriptWithPythonPackages "manage_firefox_sessions" [
    pkgs.python3Packages.dmenu-python
    pkgs.python3Packages.notify2
  ] ''
    import argparse
    import glob
    import os
    import subprocess
    import time

    import dmenu
    import notify2
    from notify2 import URGENCY_NORMAL


    def collect_sessions():
        return [os.path.basename(session) for session in glob.glob("${cfg.sessions.firefox.path}/*.org")]


    parser = argparse.ArgumentParser(description="Manage Firefox stored sessions.")
    parser.add_argument("--save", dest="save_session", action="store_true",
                       default=False, help="Save current session")
    parser.add_argument("--open", dest="open_session", action="store_true",
                       default=False, help="Open stored session")
    parser.add_argument("--edit", dest="edit_session", action="store_true",
                       default=False, help="Edit stored session")
    parser.add_argument("--delete", dest="delete_session", action="store_true",
                       default=False, help="Delete stored session")

    args = parser.parse_args()
    notify2.init("manage_firefox_sessions")

    if args.save_session:
        session_name = dmenu.show([], prompt="save as",
                                  case_insensitive=True, lines=1)
        if session_name:
            subprocess.Popen(
                "${dump_firefox_session}/bin/dump_firefox_session {0}".format(session_name),
                shell=True, stdout=subprocess.PIPE)
    elif args.open_session:
        session_name = dmenu.show(sorted(collect_sessions()), prompt="open",
                                  case_insensitive=True, lines=15)
        if session_name:
            urls = None
            with open("${cfg.sessions.firefox.path}/{0}".format(session_name), "r") as session:
                urls = [url.strip()[2:] for url in session.readlines() if url.startswith("* http")]
            if len(urls) <= ${builtins.toString cfg.sessions.sizeThreshold}:
                subprocess.Popen(
                    "${pkgs.firefox-unwrapped}/bin/firefox --new-window {0}".format(urls[0]),
                    shell=True, stdout=subprocess.PIPE)
                time.sleep(0.5)
                urls_remainder = " --new-tab ".join(urls[1:])
                if len(urls_remainder):
                    subprocess.Popen(
                        "${pkgs.firefox-unwrapped}/bin/firefox --new-tab {0}".format(urls_remainder),
                        shell=True, stdout=subprocess.PIPE)
            else:
                emacsclient_task = subprocess.Popen(
                    "${pkgs.emacs}/bin/emacsclient -c ${cfg.sessions.firefox.path}/{0}".format(session_name),
                    shell=True, stdout=subprocess.PIPE)
                assert emacsclient_task.wait() == 0
    elif args.edit_session:
        session_name = dmenu.show(sorted(collect_sessions()), prompt="edit",
                                  case_insensitive=True, lines=15)
        if session_name:
            emacsclient_task = subprocess.Popen(
                "${pkgs.emacs}/bin/emacsclient -c ${cfg.sessions.firefox.path}/{0}".format(session_name),
                shell=True, stdout=subprocess.PIPE)
            assert emacsclient_task.wait() == 0
    elif args.delete_session:
        session_name = dmenu.show(sorted(collect_sessions()), prompt="delete",
                                  case_insensitive=True, lines=15)
        if session_name:
            subprocess.Popen(
                "${pkgs.coreutils}/bin/rm ${cfg.sessions.firefox.path}/{0}".format(session_name),
                shell=True, stdout=subprocess.PIPE)
            n = notify2.Notification("[Firefox]", "Removed {0}".format(session_name))
            n.set_urgency(URGENCY_NORMAL)
            n.set_timeout(5000)
            n.show()
  '';
  dump_firefox_session = pkgs.writeShellScriptBin "dump_firefox_session" ''
    DUMP_NAME=$1
    PROFILE_NAME="${config.home-manager.users."${config.attributes.mainUser.name}".programs.firefox.profiles.default.path}"
    SESSIONSTORE_PATH=/home/${config.attributes.mainUser.name}/.mozilla/firefox/$PROFILE_NAME/sessionstore-backups
    TS=$(${pkgs.coreutils}/bin/date '+%Y-%m-%d_%H-%M-%S')

    SESSION_DBFILE="$SESSIONSTORE_PATH/previous.jsonlz4"
    if [ -f "$SESSIONSTORE_PATH/recovery.jsonlz4" ]; then
      SESSION_DBFILE="$SESSIONSTORE_PATH/recovery.jsonlz4"
    fi

    if [ -z "$DUMP_NAME" ]; then
      SESSION_DUMPFILE="${cfg.sessions.firefox.nameTemplate}-$TS.org"
    else
      SESSION_DUMPFILE="$DUMP_NAME.org"
    fi

    TAB_COUNT=$(${pkgs.dejsonlz4}/bin/dejsonlz4 $SESSION_DBFILE | \
      ${pkgs.jq}/bin/jq -j '.windows[].tabs[].entries[] | .url, "\n"' | \
      ${pkgs.gnused}/bin/sed 's/^/\* /g' | ${pkgs.coreutils}/bin/tee \
      ${cfg.sessions.firefox.path}/$SESSION_DUMPFILE | ${pkgs.coreutils}/bin/wc -l)

    ${pkgs.dunst}/bin/dunstify -u normal "[Firefox] Saved session ($TAB_COUNT tabs)."
  '';
  rotate_firefox_session_dumps = pkgs.writeShellScriptBin "rotate_firefox_session_dumps" ''
    ls -a ${cfg.sessions.firefox.path} | ${pkgs.gnugrep}/bin/grep ${cfg.sessions.firefox.nameTemplate} | \
    ${pkgs.coreutils}/bin/sort -n -r | (i=0; while read -r f; do
      if [ $i -lt ${builtins.toString cfg.sessions.firefox.historyLength} ]; then
          ((i++))
          continue
      else
          rm -f "${cfg.sessions.firefox.path}/$f"
      fi
    done)
  '';
  emacsBrowsersSetup = ''
    (use-package atomic-chrome
      :ensure t
      :defer 2
      :custom
      (atomic-chrome-buffer-open-style 'frame)
      (atomic-chrome-server-ghost-text-port 4001)
      (atomic-chrome-url-major-mode-alist
       '(("reddit\\.com" . markdown-mode)
         ("github\\.com" . gfm-mode)
         ("redmine" . textile-mode))
       "Major modes for URLs.")
      :config
      (atomic-chrome-start-server))

    (use-package browse-url
      :if (and (eq system-type 'gnu/linux)
               (eq window-system 'x))
      :defer 5
      :preface
      (defun custom/buffer-urls--candidates ()
        (save-excursion
          (save-restriction
            (let ((urls))
              (goto-char (point-min))
              (while (re-search-forward org-plain-link-re nil t)
                (push (thing-at-point 'url) urls))
              (remove nil urls)))))
      (defun custom/open-url-current-buffer ()
        (interactive)
        (ivy-read "URLs: "
                  (funcall #'custom/buffer-urls--candidates)
                  :action #'(lambda (candidate)
                              (browse-url candidate))
                  :require-match t
                  :caller 'custom/open-url-current-buffer))
      :custom
      (browse-url-browser-function 'browse-url-firefox) ;; TODO: sync at module level
      (browse-url-generic-program "${config.attributes.defaultCommands.browser}"))

    (use-package eww
      :defer 6
      :preface
      (defun eww-more-readable () ;;TODO: add to appropriate hook
        "Makes eww more pleasant to use. Run it after eww buffer is loaded."
        (interactive)
        (setq eww-header-line-format nil) ;; removes page title
        (setq mode-line-format nil) ;; removes mode-line
        (set-window-margins (get-buffer-window) 20 20) ;; increases size of margins
        (redraw-display) ;; apply mode-line changes
        (eww-reload 'local))
      :custom
      (eww-search-prefix "https://duckduckgo.com/html/?kd=-1&q="))
  '';
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
        default = {};
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
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.browserpass.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.firefox.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          xsel # for firefox native clients
        ];
        programs.firefox = {
          enable = true;
          extensions = with firefox-addons; [
            dark_reader
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
                "extensions.autoDisableScopes" = 0;
                "extensions.pocket.enabled" = false;
                "extensions.update.autoUpdateDefault" = false;
                "extensions.update.background.url" = "";
                "extensions.update.enabled" = false;
                "extensions.update.url" = "";
                "lightweightThemes.selectedThemeID" = "firefox-compact-dark@mozilla.org";
                "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
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
          ".mozilla/firefox/profile.default/browser-extension-data/{d47d18bc-d6ba-4f96-a144-b3016175f3a7}/storage.js".text = builtins.toJSON {
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
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.activation.ensureFirefoxSessionsPath = {
          after = [];
          before = ["linkGeneration"];
          data = "mkdir -p ${cfg.sessions.firefox.path}";
        };
        home.packages = with pkgs; [
          dump_firefox_session
          manage_firefox_sessions
          rotate_firefox_session_dumps
        ];
      };
      wm.xmonad.keybindings = lib.optionalAttrs (config.wm.xmonad.enable) {
        "M-C-s" = ''spawn "${manage_firefox_sessions}/bin/manage_firefox_sessions --save"'';
        "M-C-o" = ''spawn "${manage_firefox_sessions}/bin/manage_firefox_sessions --open"'';
        "M-C-e" = ''spawn "${manage_firefox_sessions}/bin/manage_firefox_sessions --edit"'';
        "M-C-d" = ''spawn "${manage_firefox_sessions}/bin/manage_firefox_sessions --delete"'';
      };
      systemd.user.services."backup-current-session-firefox" = {
        description = "Backup current firefox session (tabs)";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${dump_firefox_session}/bin/dump_firefox_session";
          ExecStopPost = "${rotate_firefox_session_dumps}/bin/rotate_firefox_session_dumps";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
      };
      systemd.user.timers."backup-current-session-firefox" = {
        description = "Backup current firefox session (tabs)";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnUnitActiveSec = cfg.sessions.saveFrequency;
        };
      };
    })
    (mkIf (cfg.enable && cfg.aux.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          next
          w3m-full
          webmacs
        ];
        programs.emacs.extraPackages = epkgs: [
          epkgs.atomic-chrome
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.config = ''${emacsBrowsersSetup}'';
    })
  ];
}
