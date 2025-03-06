{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: review *all* settings

let
  cfg = config.browsers.qutebrowser;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  standardDesktopID = "org.qutebrowser.qutebrowser";
  windowedDesktopID = "org.custom.qutebrowser.windowed";
  suspensionRule = {
    qutebrowser = {
      suspendDelay = 15;
      matchWmClassContains = lib.last cfg.windowClass;
      suspendSubtreePattern = "qtwebengine";
      downclockOnBattery = 0;
      resumeEvery = 60;
      resumeFor = 3;
      sendSignals = true;
    };
  };
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    browsers.qutebrowser = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable qutebrowser.";
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
        description = "Suspend when inactive (using xsuspender)";
      };
      command = mkOption {
        type = types.str;
        default = "${pkgs.qutebrowser}/bin/qutebrowser --target window";
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "qutebrowser" "qutebrowser" ];
        visible = false;
        internal = true;
        description = "Qutebrowser default window class.";
      };
      desktopID = mkOption {
        type = types.enum [ standardDesktopID windowedDesktopID ];
        default = windowedDesktopID;
        description = "Desktop entry name";
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
      emacsKeys.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs-like keybindings";
      };
      hints.mode = mkOption {
        type = types.enum [ "letter" "number" "word" ];
        default = "letter";
        description = "Mode to use for hints";
      };
      hints.chars = mkOption {
        type = types.str;
        default = "qweasdzxc";
        description = "Characters used for hint strings";
      };
      emacs.browseUrlSetup = mkOption {
        type = types.lines;
        default = ''
          (use-package browse-url
            :config
            (setq browse-url-browser-function 'browse-url-generic)
            (setq browse-url-generic-program "${builtins.head (splitString " " cfg.command)}")
            (setq browse-url-generic-args '(${concatStringsSep " " (forEach (builtins.tail (splitString " " cfg.command))
              (s: "\"" + s + "\""))})))
        '';
        visible = false;
        internal = true;
        description = "Specialized Qutebrowser-aware `browse-url` package setup";
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
      sessions.path = mkOption {
        type = types.str;
        default = homePrefix user "docs/org/browser-sessions/qutebrowser";
        description = "Where to save plaintext Qutebrowser session contents.";
      };
      sessions.keepMinutes = mkOption {
        type = types.int;
        default = 180;
        description = "Keep sessions age under this amount";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: {
        yank-image = pkgs.writeShellApplication {
          name = "yank-image";
          runtimeInputs = with pkgs; [ wget xclip ];
          text = ''wget "$1" -q -O - | xclip -i -selection primary -t image/jpeg'';
        };
      };
      workstation.input.keyboard.xkeysnail.rc = ''
        define_keymap(re.compile("${lib.last cfg.windowClass}"), {
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

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive suspensionRule;

      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          yank-image
          (makeDesktopItem {
            name = windowedDesktopID;
            type = "Application";
            exec = "${cfg.command} %U";
            comment = "Qutebrowser that opens links preferably in new windows";
            desktopName = "Qutebrowser";
            categories = [ "Network" "WebBrowser" ];
          })
        ];
        xdg.configFile = {
          "qutebrowser/hint-words".text = builtins.readFile ./assets/hint-words;
          "espanso/config/qutebrowser.yml".source = yaml.generate "espanso-config-qutebrowser.yml" {
            filter_class = lib.last cfg.windowClass;
            backend = "Clipboard";
          };
        };
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
            zoom = {
              levels = [
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
              default = "100%";
            };
            colors = {
              statusbar.url.success.https.fg = "white";
              tabs = rec {
                even.bg = "silver";
                even.fg = "#666666";
                odd.bg = "gainsboro";
                odd.fg = even.fg;
              };
              webpage = {
                preferred_color_scheme = "dark";
                darkmode = {
                  inherit (cfg.darkmode) algorithm;
                  enabled = cfg.darkmode.enable;
                  policy.images = "smart";
                  policy.page = "smart";
                  threshold.background = 128;
                  grayscale.images = 0; # 0.0 0.5 # FIXME: removed in 3.1.0
                } // optionalAttrs
                  (cfg.darkmode.algorithm == "lightness-hsl" || cfg.darkmode.algorithm == "brightness-rgb")
                  {
                    contrast = -2.5e-2; # 0.0 0.5 0.9 1.0
                    grayscale.all = true; # false # FIXME: removed in 3.1.0
                  };
              };
            };
            completion = {
              height = "50%";
              quick = true;
              show = "auto";
              shrink = true;
              timestamp_format = "%d-%m-%Y";
              use_best_match = false;
            };
            confirm_quit = [ "downloads" ];
            content = {
              autoplay = false;
              cache = {
                size = 5242880;
              };
              canvas_reading = true;
              cookies.store = true;
              geolocation = "ask";
              headers = {
                do_not_track = true;
                custom = { };
                accept_language = "en-US,en";
              };
              javascript.enabled = true;
              mute = true;
              notifications = {
                enabled = true;
                presenter = "libnotify";
              };
              pdfjs = true;
              plugins = true;
              proxy = "system";
              register_protocol_handler = true;
              tls.certificate_errors = "ask-block-thirdparty";
              webgl = true;
            };
            messages.timeout = 1000;
            downloads = {
              location = {
                directory = config.attributes.browser.downloadPath;
                prompt = true;
                remember = true;
                suggestion = "both";
              };
            };
            hints = {
              hide_unmatched_rapid_hints = true;
              leave_on_load = true;
              min_chars = 1;
              mode = cfg.hints.mode;
              dictionary = xdgConfig "${user}" "/qutebrowser/hint-words";
              chars = cfg.hints.chars;
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
              forward_unbound_keys = "auto";
              insert_mode = {
                auto_leave = true;
                auto_load = true;
                plugins = false;
              };
              links_included_in_focus_chain = true;
              partial_timeout = 2000;
              spatial_navigation = true;
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
            statusbar = {
              position = "bottom";
              widgets = [ "keypress" "url" "history" "tabs" "progress" ];
            };
            tabs = {
              background = true;
              last_close = "close";
              mousewheel_switching = false;
              new_position = {
                related = "next";
                unrelated = "last";
              };
              position = "top";
              select_on_remove = "next";
              show = "multiple";
              tabs_are_windows = false;
              title = {
                format = "{audio}{current_title}";
                format_pinned = "{audio}";
                alignment = "center";
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
          keyBindings =
            let
              qutePassCommonArgs = ''--no-insert-mode --dmenu-invocation dmenu --username-pattern "((?<=^user: ).*|(?<=login: ).*)" --username-target secret'';
            in
            {
              normal = {
                # TODO: play with "set completion.open_categories [<something>] ;; cmd-set-text -s :<cmd>" and "completion-item-focus next-category"
                "<Alt-,>" = "back";
                "<Alt-.>" = "forward";
                "<ctrl+shift+tab>" = "tab-prev";
                "<ctrl+tab>" = "tab-next";
                "b" = "cmd-set-text -s :tab-select";
                "t" = "cmd-set-text -s :open -t";
                "<Ctrl-Return>" = "selection-follow";
                "<Ctrl-Shift-Return>" = "selection-follow -t";
                "<Ctrl-F5>" = "reload -f";
                "Ctrl-r" = "reload";
                "u" = "undo";
                "U" = "undo --window";
                "<F12>" = "inspector";
                "<F5>" = "reload";
                "Sh" = "open qute://history";
                "Ss" = "open qute://settings";
                "ct" = "open -t -- {clipboard}";
                "cw" = "open -w -- {clipboard}";
                "d" = "tab-close";
                "g$" = "tab-focus last";
                "g0" = "tab-focus 1";
                "gc" = "tab-clone";
                "gj" = "tab-move +";
                "gk" = "tab-move -";
                "go" = "spawn ${config.attributes.browser.fallback.cmd} {url}";
                "ge" = "cmd-set-text :open {url}";
                "gl" = "open {url:scheme}://mail.gnu.org{url:path}";
                "gs" = "view-source";
                "gw" = "cmd-set-text -s :tab-give";
                "pt" = "open -t -- {primary}";
                "pw" = "open -w -- {primary}";
                "yy" = "yank";
                "yd" = "yank domain";
                "yM" = "spawn mpv {url}";
                "yo" = "yank inline [[{url}][{title}]]";
                "yp" = "yank pretty-url";
                "yt" = "yank title";
                ";;" = "hint links download";
                ";I" = "hint images tab";
                ";O" = "hint links fill :open -t -r {hint-url}";
                ";R" = "hint --rapid links window";
                ";b" = "hint all tab-bg";
                ";d" = "hint links download";
                ";f" = "hint all tab-fg";
                ";h" = "hint all hover";
                ";i" = "hint images";
                ";c" = "hint images spawn yank-image {hint-url}";
                ";o" = "hint links fill :open {hint-url}";
                ";r" = "hint --rapid links tab-bg";
                ";t" = "hint inputs";
                ";v" = ''
                  hint links spawn --detach mpv --ytdl-format="bestvideo[height<=1000][vcodec!=vp9]+bestaudio/best" --force-window yes {hint-url}'';
                ";y" = "hint links yank";
                "ad" = "download-cancel";
                "cd" = "download-clear";
                "gd" = "download";
                # TODO: fork qute-pass userscript, namely for altering domain search logic, docs - https://qutebrowser.org/doc/help/commands.html#spawn
                "za" = "spawn --userscript qute-pass ${qutePassCommonArgs}";
                "zul" = "spawn --userscript qute-pass ${qutePassCommonArgs} --username-only";
                "zup" = "spawn --userscript qute-pass ${qutePassCommonArgs} --password-only";
                "@" = "macro-run";
                "\\\"" = "macro-run";
                "AD" = "adblock-update";
                "CH" = "history-clear";
                "cr" = "config-source";
                "sf" = "save";
                "ws" = "config-write-py --force --defaults config.current.py";
                "i" = "mode-enter insert";
                "e" = "cmd-set-text \/rror loading http"; # NOTE: for searching url parts in windows, where renderer process was killed
              } // optionalAttrs (cfg.emacsKeys.enable) {
                "<Ctrl-s>" = "cmd-set-text /";
                "<Ctrl-r>" = "cmd-set-text ?";
                "<Ctrl-W>" = "tab-close";
                "<Ctrl-g>" = "stop";
                "<Ctrl-p>" = "tab-pin";
                "<Ctrl-Shift-_>" = "undo";
                "<Ctrl-u><Ctrl-Shift-_>" = "undo --window";
                "<Ctrl-x><Ctrl-r>" = "config-source";
                "<Ctrl-x>k" = "tab-close";
                "<Ctrl-x>1" = "tab-only";
                "<Ctrl-x>r" = "reload";
                "<Alt-w>" = "yank";
                "<Ctrl-u><Alt-w>d" = "yank domain";
                "<Ctrl-u><Alt-w>p" = "yank pretty-url";
                "<Ctrl-u><Alt-w>t" = "yank title";
                "<Alt-x>" = "cmd-set-text :";
                "<Alt-b>" = "back";
                "<Alt-f>" = "forward";
                "<Ctrl-e>" = "tab-next";
                "<Ctrl-a>" = "tab-prev";
                "<Ctrl-x><Ctrl-f>" = "cmd-set-text -s :open";
                "<Ctrl-u><Ctrl-x><Ctrl-f>" = "cmd-set-text -s :open -t";
                "<Alt-Shift-,>" = "scroll-to-perc 0";
                "<Alt-Shift-.>" = "scroll-to-perc";
                "<Ctrl-x><Ctrl-c>" = "quit";
                "<Ctrl-x>e" = "cmd-set-text \/rror loading http"; # NOTE: for searching url parts in windows, where renderer process was killed
              };
              insert = optionalAttrs (cfg.emacsKeys.enable) {
                "<Ctrl-e>" = "edit-text";
                "<Ctrl-y>" = "insert-text -- {clipboard}";
                "<Shift-y>" = "insert-text -- {primary}";
              };
              command = optionalAttrs (cfg.emacsKeys.enable) {
                "<Ctrl-s>" = "search-next";
                "<Ctrl-r>" = "search-prev";
              };
              prompt = optionalAttrs (cfg.emacsKeys.enable) {
                "<Ctrl-s>" = "search-next";
                "<Ctrl-r>" = "search-prev";
              };
            };
          keyMappings = {
            "<Alt-б>" = "<Alt-,>";
            "<Alt-ю>" = "<Alt-.>";
            "<Ctrl-Ц>" = "<Ctrl-W>";
            "<Ctrl-з>" = "<Ctrl-p>";
            "<Ctrl-п>" = "<Ctrl-g>";
            "Ctrl-к" = "Ctrl-r";
            "е" = "t";
            "СР" = "CH";
            "ФВ" = "AD";
            "Ыр" = "Sh";
            "Ыы" = "Ss";
            "в" = "d";
            "жК" = ";R";
            "жШ" = ";I";
            "жЩ" = ";O";
            "жа" = ";f";
            "жв" = ";d";
            "же" = ";t";
            "жж" = ";;";
            "жи" = ";b";
            "жк" = ";r";
            "жм" = ";v";
            "жн" = ";y";
            "жр" = ";h";
            "жс" = ";c";
            "жш" = ";i";
            "жщ" = ";o";
            "зе" = "pt";
            "зц" = "pw";
            "и" = "b";
            "нЬ" = "yM";
            "нв" = "yd";
            "не" = "yt";
            "нз" = "yp";
            "нн" = "yy";
            "нщ" = "yo";
            "п0" = "g0";
            "п;" = "g$";
            "пв" = "gd";
            "пд" = "gl";
            "пл" = "gk";
            "по" = "gj";
            "пс" = "gc";
            "пу" = "ge";
            "пц" = "gw";
            "пщ" = "go";
            "пы" = "gs";
            "св" = "cd";
            "се" = "ct";
            "ск" = "cr";
            "сц" = "cw";
            "фв" = "ad";
            "цы" = "ws";
            "ш" = "i";
            "ыа" = "sf";
            "ягд" = "zul";
            "ягз" = "zup";
            "яф" = "za";
          };
          extraConfig = ''
            config.set('content.javascript.enabled', True, 'chrome://*/*')
            config.set('content.javascript.enabled', True, 'file://*')
            config.set('content.javascript.enabled', True, 'qute://*/*')

            ${optionalString cfg.emacsKeys.enable "config.unbind(\"<Ctrl-u>\")"}

            c.statusbar.padding['top'] = 4
            c.statusbar.padding['bottom'] = 4
            c.statusbar.padding['right'] = 4
            c.statusbar.padding['left'] = 4
            c.tabs.padding['top'] = 0
            c.tabs.padding['bottom'] = 1
            c.tabs.padding['right'] = 5
            c.tabs.padding['left'] = 5
            c.tabs.indicator.padding['top'] = 2
            c.tabs.indicator.padding['bottom'] = 2
            c.tabs.indicator.padding['right'] = 0
            c.tabs.indicator.padding['left'] = 4
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
      home-manager.users."${user}" = {
        xdg.mimeApps.defaultApplications =
          mapMimesToApp config.attributes.mimetypes.browser "${cfg.desktopID}.desktop";
      };
      services.xserver.displayManager.sessionCommands = ''
        ${pkgs.xdg-utils}/bin/xdg-settings set default-web-browser ${cfg.desktopID}.desktop
      '';
      shell.core.variables = [{
        TB_DEFAULT_BROWSER = cfg.command;
        TB_DEFAULT_BROWSER_SESSIONS_STORE = cfg.sessions.path;
        TB_QUTEBROWSER_SESSIONS_KEEP_MINUTES = builtins.toString cfg.sessions.keepMinutes;
        global = true;
      }];

      attributes.browser.default.cmd = cfg.command;
      attributes.browser.default.windowClass = cfg.windowClass;

      browsers.ext.emacs.browseUrlSetup = cfg.emacs.browseUrlSetup;

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive suspensionRule;

      navigation.bookmarks.entries = {
        "qutebrowser/sessions/raw" = { local.path = homePrefix user ".local/share/qutebrowser/sessions"; };
        "qutebrowser/sessions/exported" = { local.path = homePrefix user "docs/org/browser-sessions/qutebrowser"; };
        "qutebrowser_plus" = {
          desc = "qutebrowser + ";
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=qutebrowser+";
          };
        };
        "qutebrowser_help_index" = {
          desc = "qutebrowser help index";
          remote.url = "https://qutebrowser.org/doc/help/index.html";
        };
        "qutebrowser_help_commands" = {
          desc = "qutebrowser commands reference";
          remote.url = "https://qutebrowser.org/doc/help/commands.html";
        };
        "qutebrowser_help_settings" = {
          desc = "qutebrowser settings reference";
          remote.url = "https://qutebrowser.org/doc/help/settings.html";
        };
      };
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

      shell.core.variables = [{ TB_FALLBACK_BROWSER = cfg.command; global = true; }];

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive suspensionRule;
    })
    (mkIf (cfg.enable && cfg.sessions.backup.enable) {
      home-manager.users."${user}" = {
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
          Environment = [ "TB_QUTEBROWSER_SESSIONS_KEEP_MINUTES=${builtins.toString cfg.sessions.keepMinutes}" ];
          ExecStart = "${nurpkgs.toolbox}/bin/qbsessions -save";
          ExecStopPost = "${nurpkgs.toolbox}/bin/qbsessions -rotate";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
      systemd.user.timers."backup-current-session-qutebrowser" =
        renderTimer "Backup current qutebrowser session (tabs)" cfg.sessions.saveFrequency cfg.sessions.saveFrequency ""
          false "";
    })
    (mkIf (cfg.enable && cfg.sessions.backup.enable && cfg.isDefault) {
      wmCommon.keybindings.entries = [
        {
          key = [ "s" ];
          cmd = "${nurpkgs.toolbox}/bin/qbsessions -export";
          mode = "browser";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.exposeScripts) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ yank-image ];
      };
    })
  ];
}
