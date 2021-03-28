{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.browsers.qutebrowser;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  scriptsPathPrefix = "${pkgs.qutebrowser}/share/qutebrowser/scripts";
  suspensionRule = {
    qutebrowser = {
      suspendDelay = 10;
      matchWmClassContains = "qutebrowser";
      suspendSubtreePattern = "qtwebengine";
    };
  };
in {
  options = {
    browsers.qutebrowser = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable qutebrowser.";
      };
      downloadPath = mkOption {
        type = types.str;
        default = homePrefix "Downloads";
        description = "Downloads path.";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set qutebrowser as default browser.";
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = "Set qutebrowser as fallback browser.";
      };
      suspendInactive = mkOption {
        type = types.bool;
        default = true;
        description = "Suspend when inacive (using xsuspender)";
      };
      command = mkOption {
        type = types.str;
        default = "${pkgs.qutebrowser}/bin/qutebrowser --target window";
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "qutebrowser" ];
        visible = false;
        internal = true;
        description = "Qutebrowser default window class.";
      };
      darkmode.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable darkmode";
      };
      darkmode.algorithm = mkOption {
        type = types.enum [ "lightness-cielab" "lightness-hsl" "brightness-rgb" ];
        default = "lightness-hsl";
        description = "Darkmode algorithm to use";
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
        default = homePrefix "docs/org/browser-sessions/qutebrowser";
        description = "Where to save plaintext Qutebrowser session contents.";
      };
      sessions.historyLength = mkOption {
        type = types.int;
        default = 10;
        description = "How many recent sessions to keep.";
      };
      sessions.nameTemplate = mkOption {
        type = types.str;
        default = "qutebrowser-session-auto";
        description = "Filename template for Qutebrowser session files.";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        session-save = mkShellScriptWithDeps "session-save" (with pkgs; [ coreutils socat ])
          (readSubstituted ../../subst.nix ./scripts/session-save.sh);
        yank-image = mkShellScriptWithDeps "yank-image" (with pkgs; [ wget xclip ])
          (readSubstituted ../../subst.nix ./scripts/yank-image.sh);
        qb-fix-session =
          mkPythonScriptWithDeps "qb-fix-session" (with pkgs; [ nurpkgs.pystdlib python3Packages.pyyaml ])
          (readSubstituted ../../subst.nix ./scripts/qb-fix-session.py);
        qb-dump-session =
          mkPythonScriptWithDeps "qb-dump-session" (with pkgs; [ nurpkgs.pystdlib python3Packages.pyyaml ])
          (readSubstituted ../../subst.nix ./scripts/qb-dump-session.py);
        manage-qb-sessions = mkPythonScriptWithDeps "manage-qb-sessions" (with pkgs; [ nurpkgs.pystdlib ])
          (readSubstituted ../../subst.nix ./scripts/manage-qb-sessions.py);
      };
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("qutebrowser"), {
            K("C-g"): K("f5"),
            K("C-n"): K("C-g"),
            K("M-comma"): K("Shift-h"),
            K("M-dot"): K("Shift-l"),
            K("C-x"): {
                K("b"): K("b"),
                K("k"): [K("Esc"), K("d")],
                K("u"): K("u"),
                K("C-s"): [K("Esc"), K("Shift-semicolon"), K("w"), K("enter")],
                K("C-c"): [K("Esc"), K("Shift-semicolon"), K("w"), K("q"), K("enter")],
            },
        }, "qutebrowser")
      '';
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          yank-image
          qb-fix-session

          (makeDesktopItem {
            name = "org.custom.qutebrowser.windowed";
            type = "Application";
            exec = "${cfg.command} %U";
            comment = "Qutebrowser that opens links preferably in new windows";
            desktopName = "QuteBrowser";
            categories = pkgs.lib.concatStringsSep ";" [ "Network" "WebBrowser" ];
          })
        ];
        programs.qutebrowser = {
          enable = true;
          package = pkgs.qutebrowser;
          loadAutoconfig = false;
          aliases = {
            jsd = "set content.javascript.enabled false";
            jse = "set content.javascript.enabled true";
          };
          settings = {
            auto_save = {
              interval = 15000;
              session = true;
            };
            editor.command = [
              "${config.ide.emacs.core.package}/bin/emacsclient"
              "-c"
              "-s /run/user/${builtins.toString config.users.extraUsers."${user}".uid}/emacs/server"
              "+{line}:{column}"
              "{}"
            ];
            zoom.levels = [
              "25%"
              "33%"
              "50%"
              "67%"
              "75%"
              "90%"
              "100%"
              "110%"
              "125%"
              "150%"
              "175%"
              "200%"
              "250%"
              "300%"
              "400%"
              "500%"
            ];
            colors = {
              statusbar.url.success.https.fg = "white";
              tabs = rec {
                even.bg = "silver";
                even.fg = "#666666";
                odd.bg = "gainsboro";
                odd.fg = even.fg;
              };
              webpage = {
                prefers_color_scheme_dark = true;
                # TODO: play with toggling darkmode, example just below:
                # if c.colors.webpage.darkmode.enabled:
                #     config.bind('\\d', 'set colors.webpage.darkmode.enabled False ;; restart')
                # else:
                #     config.bind('\\d', 'set colors.webpage.darkmode.enabled True ;; restart')
                # C-d: set colors.webpage.darkmode.enabled false
                # ===============================================================================
                # TODO: play with commented out values below
                darkmode = {
                  enabled = cfg.darkmode.enable;
                  algorithm = cfg.darkmode.algorithm;
                  policy.images = "smart"; # "never"
                  policy.page = "smart"; # "always"
                  threshold = {
                    # With selective inversion of non-image elements:
                    # Set `colors.webpage.darkmode.threshold.text` to 150 and
                    #     `colors.webpage.darkmode.threshold.background` to 205.
                    # Set `background` to 256 to never invert the color or to 0 to always invert it.
                    background = 128; # 0 100 200 205 70
                    text = 128; # 200 256
                  };
                  grayscale.images = 0; # 0.0 0.5
                } // optionalAttrs
                  (cfg.darkmode.algorithm == "lightness-hsl" || cfg.darkmode.algorithm == "brightness-rgb") {
                    contrast = -2.5e-2; # 0.0 0.5 0.9 1.0
                    grayscale.all = true; # false
                  };
              };
            };
            completion = {
              height = "20%";
              quick = false;
              show = "auto";
              shrink = true;
              timestamp_format = "%d-%m-%Y";
              use_best_match = false;
            };
            confirm_quit = [ "downloads" ];
            content = {
              autoplay = false;
              cache = {
                appcache = true;
                size = 5242880;
              };
              canvas_reading = true;
              cookies.store = true;
              geolocation = "ask";
              javascript.enabled = true;
              mute = true;
              notifications = true;
              pdfjs = true;
              plugins = true;
              proxy = "none";
              register_protocol_handler = true;
              ssl_strict = true;
              webgl = true;
            };
            downloads = {
              location = {
                directory = config.browsers.qutebrowser.downloadPath;
                prompt = true;
                remember = true;
                suggestion = "both";
              };
            };
            hints = {
              hide_unmatched_rapid_hints = true;
              leave_on_load = true;
              min_chars = 1;
              mode = "number";
              next_regexes = [
                "\\\\bnext\\\\b"
                "\\\\bmore\\\\b"
                "\\\\bnewer\\\\b"
                "\\\\b[>→≫]\\\\b"
                "\\\\b(>>|»)\\\\b"
                "\\\\bcontinue\\\\b"
              ];
              prev_regexes =
                [ "\\\\bprev(ious)?\\\\b" "\\\\bback\\\\b" "\\\\bolder\\\\b" "\\\\b[<←≪]\\\\b" "\\\\b(<<|«)\\\\b" ];
              scatter = false;
              uppercase = false;
            };
            history_gap_interval = 30;
            input = {
              insert_mode = {
                auto_leave = true;
                auto_load = true;
                plugins = false;
              };
              links_included_in_focus_chain = true;
              partial_timeout = 2000;
              spatial_navigation = true;
            };
            fileselect = {
              handler = "external";
              single_file.command = config.attributes.defaultVTCommand
                ++ [ "${pkgs.ranger}/bin/ranger" "--choosefile={}" ];
              multiple_files.command = config.attributes.defaultVTCommand
                ++ [ "${pkgs.ranger}/bin/ranger" "--choosefiles={}" ];
            };
            keyhint.delay = 20;
            new_instance_open_target = "tab";
            new_instance_open_target_window = "last-focused";
            prompt.filebrowser = true;
            scrolling = {
              bar = "always";
              smooth = true;
            };
            search.ignore_case = "smart";
            session.lazy_restore = false; # NOTE: see qute://warning/sessions for details
            statusbar = { widgets = [ "keypress" "url" "history" "tabs" "progress" ]; };
            tabs = {
              background = true;
              last_close = "close";
              new_position = {
                related = "next";
                unrelated = "last";
              };
              position = "right";
              select_on_remove = "next";
              show = "multiple";
              tabs_are_windows = false;
              title = {
                format = "{audio}{current_title}";
                format_pinned = "{audio}";
              };
            };
            url = {
              auto_search = "never";
              default_page = "about:blank";
              incdec_segments = [ "path" "query" ];
              yank_ignored_parameters = [ "ref" "utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content" ];
            };
            window.title_format = "{private}{perc}{current_title}{title_sep}qutebrowser | {current_url}";
          };
          keyBindings = {
            normal = { # TODO: create helper function(s) for cross layout bindings generation at Nix level
              # TODO: play with "set completion.open_categories [<something>] ;; set-cmd-text -s :<cmd>" and "completion-item-focus next-category"
              # "e" = "edit-text"; # TODO: bind appropriately
              "xb" = "config-cycle statusbar.hide";
              "чи" = "config-cycle statusbar.hide";
              "<Alt-,>" = "back";
              "<Alt-б>" = "back";
              "<Alt-.>" = "forward";
              "<Alt-ю>" = "forward";
              "<ctrl+shift+tab>" = "tab-prev";
              "<ctrl+tab>" = "tab-next";
              "b" = "set-cmd-text -s :tab-select";
              "и" = "set-cmd-text -s :tab-select";
              "t" = "set-cmd-text -s :open -t";
              "е" = "set-cmd-text -s :open -t";
              "<Ctrl-F5>" = "reload -f";
              "<Ctrl-Return>" = "selection-follow -t";
              "<Ctrl-Shift-T>" = "undo";
              "<Ctrl-Shift-Е>" = "undo";
              "u" = "undo";
              "г" = "undo";
              "<Ctrl-W>" = "tab-close";
              "<Ctrl-Ц>" = "tab-close";
              "<Ctrl-g>" = "stop";
              "<Ctrl-п>" = "stop";
              "<Ctrl-p>" = "tab-pin";
              "<Ctrl-з>" = "tab-pin";
              "<F12>" = "inspector";
              "<F5>" = "reload";
              "<Return>" = "selection-follow";
              "Ctrl-r" = "reload";
              "Ctrl-к" = "reload";
              "Sh" = "open qute://history";
              "Ыр" = "open qute://history";
              "ct" = "open -t -- {clipboard}";
              "се" = "open -t -- {clipboard}";
              "cw" = "open -w -- {clipboard}";
              "сц" = "open -w -- {clipboard}";
              "d" = "tab-close";
              "в" = "tab-close";
              "g$" = "tab-focus last";
              "п;" = "tab-focus last";
              "g0" = "tab-focus 1";
              "п0" = "tab-focus 1";
              "gc" = "tab-clone";
              "пс" = "tab-clone";
              "gj" = "tab-move +";
              "по" = "tab-move +";
              "gk" = "tab-move -";
              "пл" = "tab-move -";
              "go" = "spawn ${config.attributes.browser.fallback.cmd} {url}";
              "пщ" = "spawn ${config.attributes.browser.fallback.cmd} {url}";
              "gs" = "view-source";
              "пы" = "view-source";
              "gw" = "set-cmd-text -s :tab-give";
              "пц" = "set-cmd-text -s :tab-give";
              "pt" = "open -t -- {primary}";
              "зе" = "open -t -- {primary}";
              "pw" = "open -w -- {primary}";
              "зц" = "open -w -- {primary}";
              "yy" = "yank";
              "нн" = "yank";
              "yd" = "yank domain";
              "нв" = "yank domain";
              "yM" = "spawn mpv {url}";
              "нЬ" = "spawn mpv {url}";
              "yo" = "yank inline [[{url}][{title}]]";
              "нщ" = "yank inline [[{url}][{title}]]";
              "yp" = "yank pretty-url";
              "нз" = "yank pretty-url";
              "yt" = "yank title";
              "не" = "yank title";
              "y;" = ''spawn ${pkgs.org-capture}/bin/org-capture -u "{url}" -t "{title}" -e title'';
              "нж" = ''spawn ${pkgs.org-capture}/bin/org-capture -u "{url}" -t "{title}" -e title'';
              "y'" = ''spawn ${pkgs.org-capture}/bin/org-capture -u "{url}" -t "{title}" -b "{primary}" -e title'';
              "нэ" = ''spawn ${pkgs.org-capture}/bin/org-capture -u "{url}" -t "{title}" -b "{primary}" -e title'';
              "ym" = "spawn ${pkgs.mpc_cli}/bin/mpc add yt:{url}";
              "нь" = "spawn ${pkgs.mpc_cli}/bin/mpc add yt:{url}";
              ";;" = "hint links download";
              "жж" = "hint links download";
              ";I" = "hint images tab";
              "жШ" = "hint images tab";
              ";O" = "hint links fill :open -t -r {hint-url}";
              "жЩ" = "hint links fill :open -t -r {hint-url}";
              ";R" = "hint --rapid links window";
              "жК" = "hint --rapid links window";
              ";b" = "hint all tab-bg";
              "жи" = "hint all tab-bg";
              ";d" = "hint links download";
              "жв" = "hint links download";
              ";f" = "hint all tab-fg";
              "жа" = "hint all tab-fg";
              ";h" = "hint all hover";
              "жр" = "hint all hover";
              ";i" = "hint images";
              "жш" = "hint images";
              ";c" = "hint images spawn yank-image {hint-url}";
              "жс" = "hint images spawn yank-image {hint-url}";
              ";o" = "hint links fill :open {hint-url}";
              "жщ" = "hint links fill :open {hint-url}";
              ";r" = "hint --rapid links tab-bg";
              "жк" = "hint --rapid links tab-bg";
              ";t" = "hint inputs";
              "же" = "hint inputs";
              ";v" = ''
                hint links spawn --detach mpv --ytdl-format="bestvideo[height<=1000][vcodec!=vp9]+bestaudio/best" --force-window yes {hint-url}'';
              "жм" = ''
                hint links spawn --detach mpv --ytdl-format="bestvideo[height<=1000][vcodec!=vp9]+bestaudio/best" --force-window yes {hint-url}'';
              ";y" = "hint links yank";
              "жн" = "hint links yank";
              "ad" = "download-cancel";
              "фв" = "download-cancel";
              "cd" = "download-clear";
              "св" = "download-clear";
              "gd" = "download";
              "пв" = "download";
              # TODO: review and rework pass setup
              ",P" = "spawn --userscript qute-pass --dmenu-invocation dmenu --password-only";
              "бЗ" = "spawn --userscript qute-pass --dmenu-invocation dmenu --password-only";
              ",p" = "spawn --userscript qute-pass --dmenu-invocation dmenu";
              "бз" = "spawn --userscript qute-pass --dmenu-invocation dmenu";
              "zl" = "spawn --userscript qute-pass";
              "яд" = "spawn --userscript qute-pass";
              "zpl" = "spawn --userscript qute-pass --password-only";
              "язд" = "spawn --userscript qute-pass --password-only";
              "zul" = "spawn --userscript qute-pass --username-only";
              "ягд" = "spawn --userscript qute-pass --username-only";
              "@" = "macro-run";
              "\\\"" = "macro-run";
              "AD" = "adblock-update";
              "ФВ" = "adblock-update";
              "CH" = "history-clear";
              "СР" = "history-clear";
              "cr" = "config-source";
              "ск" = "config-source";
              "sf" = "save";
              "ыа" = "save";
              "ws" = "config-write-py --force --defaults config.current.py";
              "цы" = "config-write-py --force --defaults config.current.py";
              "i" = "mode-enter insert";
              "ш" = "mode-enter insert";
            };
            insert = {
              "<Ctrl-y>" = "insert-text -- {clipboard}";
              "<Shift-y>" = "insert-text -- {primary}";
            };
          };
          extraConfig = ''
            config.set('content.javascript.enabled', True, 'chrome://*/*')
            config.set('content.javascript.enabled', True, 'file://*')
            config.set('content.javascript.enabled', True, 'qute://*/*')

            c.statusbar.padding['top'] = 4
            c.statusbar.padding['bottom'] = 4
            c.statusbar.padding['right'] = 4
            c.statusbar.padding['left'] = 4
            c.tabs.padding['top'] = 0
            c.tabs.padding['bottom'] = 1
            c.tabs.padding['right'] = 5
            c.tabs.padding['left'] = 5
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: qutebrowser: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.chromium.isDefault && !config.browsers.nyxt.isDefault
            && !config.browsers.firefox.isDefault;
          message = "browsers: qutebrowser: there should be exactly one default.";
        }
      ];

      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications =
          mapMimesToApp config.attributes.mimetypes.browser "org.custom.qutebrowser.windowed.desktop";
        home.activation.ensureQutebrowserIsDefault = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "${pkgs.xdg-utils}/bin/xdg-settings set default-web-browser org.custom.qutebrowser.windowed.desktop";
        };
      };
      attributes.browser.default.cmd = cfg.command;
      attributes.browser.default.windowClass = cfg.windowClass;

      workstation.performance.appsSuspension.rules = optionalAttrs (cfg.suspendInactive) suspensionRule;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: qutebrowser: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.chromium.isFallback && !config.browsers.nyxt.isFallback
            && !config.browsers.firefox.isFallback;
          message = "browsers: qutebrowser: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback.cmd = cfg.command;
      attributes.browser.fallback.windowClass = cfg.windowClass;

      workstation.performance.appsSuspension.rules = optionalAttrs (cfg.suspendInactive) suspensionRule;
    })
    (mkIf (cfg.enable && cfg.sessions.backup.enable) {
      home-manager.users.${user} = {
        home.activation.ensureQutebrowserSessionsPath = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "mkdir -p ${cfg.sessions.path}";
        };
      };
      systemd.user.services."backup-current-session-qutebrowser" = {
        description = "Backup current qutebrowser session (tabs)";
        serviceConfig = {
          Type = "oneshot";
          ExecStartPre = [ "${pkgs.procps}/bin/pgrep qutebrowser" "${pkgs.session-save}/bin/session-save" ];
          ExecStart = "${pkgs.qb-dump-session}/bin/qb-dump-session";
          ExecStopPost =
            "${pkgs.manage-qb-sessions}/bin/manage-qb-sessions --rotate --path ${cfg.sessions.path} --history-length ${
              builtins.toString cfg.sessions.historyLength
            }";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."backup-current-session-qutebrowser" =
        renderTimer "Backup current qutebrowser session (tabs)" cfg.sessions.saveFrequency cfg.sessions.saveFrequency ""
        false "";
    })
    (mkIf (cfg.enable && cfg.sessions.backup.enable && cfg.isDefault) {
      wmCommon.keys = [
        {
          key = [ "s" ];
          cmd = "${pkgs.manage-qb-sessions}/bin/manage-qb-sessions --save --path ${cfg.sessions.path}";
          mode = "browser";
        }
        {
          key = [ "o" ];
          cmd = "${pkgs.manage-qb-sessions}/bin/manage-qb-sessions --open --path ${cfg.sessions.path}";
          mode = "browser";
        }
        {
          key = [ "d" ];
          cmd = "${pkgs.manage-qb-sessions}/bin/manage-qb-sessions --delete --path ${cfg.sessions.path}";
          mode = "browser";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ session-save yank-image qb-fix-session qb-dump-session manage-qb-sessions ];
      };
    })
  ];
}
