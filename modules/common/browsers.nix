{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.custom.browsers;
  firefox-addons = pkgs.recurseIntoAttrs (pkgs.callPackage ../../pkgs/firefox-addons { });
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
                "browser.ctrlTab.recentlyUsedOrder" = false;
                "browser.download.dir" = "${cfg.downloadPath}";
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
      # chrome-export
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
