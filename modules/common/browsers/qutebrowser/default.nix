{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.custom.browsers.qutebrowser;
  user = config.attributes.mainUser.name;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.browsers.qutebrowser = {
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
      command = mkOption {
        type = types.str;
        default = "${inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.qutebrowser}/bin/qutebrowser --target window";
        description = "Default command line to invoke";
      };
      staging.enableSettings = mkOption {
        type = types.bool;
        default = false;
        description = "Enable staging settings.";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        yank-image = mkShellScriptWithDeps "yank-image" (with pkgs; [ wget xclip ])
          (readSubstituted ../../subst.nix ./scripts/yank-image.sh);
        qb-fix-session = mkPythonScriptWithDeps "qb-fix-session" (with pkgs; [ python3Packages.pyyaml ])
          (readSubstituted ../../subst.nix ./scripts/qb-fix-session.py);
      };
      custom.xinput.xkeysnail.rc = ''
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
            categories = stdenv.lib.concatStringsSep ";" [ "Network" "WebBrowser" ];
          })
        ];
        programs.qutebrowser = {
          enable = true;
          package = pkgs.qutebrowser;
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
              "${config.ide.emacs.package}/bin/emacsclient"
              "-c"
              "-s /run/user/${
                builtins.toString config.users.extraUsers."${user}".uid
              }/emacs/server"
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
              webpage.prefers_color_scheme_dark = true;
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
                directory = config.custom.browsers.qutebrowser.downloadPath;
                prompt = false;
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
              # padding = "{'top': 4, 'bottom': 4, 'left': 4, 'right': 4}"; # FIXME: module fails to render dicts
              widgets = [ "keypress" "url" "history" "tabs" "progress" ];
            };
            tabs = {
              background = true;
              last_close = "close";
              new_position = {
                related = "next";
                unrelated = "last";
              };
              # padding = "{'top': 0, 'bottom': 1, 'left': 5, 'right': 5}"; # FIXME: module fails to render dicts
              position = "top";
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
          keyMappings = {
            "<Alt-б>" = "<Alt-,>";
            "<Alt-ю>" = "<Alt-.>";
            "<Ctrl-Shift-Е>" = "<Ctrl-Shift-T>";
            "<Ctrl-Ц>" = "<Ctrl-W>";
            "<Ctrl-з>" = "<Ctrl-p>";
            "<Ctrl-н>" = "<Ctrl-y>";
            "<Ctrl-п>" = "<Ctrl-g>";
            "<Shift-н>" = "<Shift-y>";
            "Ctrl-r" = "Ctrl-r";
            "\"" = "@";
            "СР" = "CH";
            "ФВ" = "AD";
            "Ыр" = "Sh";
            "бЗ" = ",P";
            "бз" = ",p";
            "в" = "d";
            "е" = "t";
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
            "нв" = "yd";
            "не" = "yt";
            "нж" = "y;";
            "нз" = "yp";
            "нн" = "yy";
            "нщ" = "yo";
            "нь" = "ym";
            "нэ" = "y'";
            "п0" = "g0";
            "п;" = "g$";
            "пв" = "gd";
            "пл" = "gk";
            "по" = "gj";
            "пс" = "gc";
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
            "яд" = "zl";
            "язд" = "zpl";
          };
          keyBindings = {
            normal = {
              # "e" = "open-editor"; # TODO: bind appropriately
              "xb" = "config-cycle statusbar.hide";
              "<Alt-,>" = "back";
              "<Alt-.>" = "forward";
              "<ctrl+shift+tab>" = "tab-prev";
              "<ctrl+tab>" = "tab-next";
              "b" = "set-cmd-text -s :buffer";
              "t" = "set-cmd-text -s :open -t";
              "<Ctrl-F5>" = "reload -f";
              "<Ctrl-Return>" = "follow-selected -t";
              "<Ctrl-Shift-T>" = "undo";
              "<Ctrl-W>" = "tab-close";
              "<Ctrl-g>" = "stop";
              "<Ctrl-p>" = "tab-pin";
              "<F12>" = "inspector";
              "<F5>" = "reload";
              "<Return>" = "follow-selected";
              "Ctrl-r" = "reload";
              "Sh" = "open qute://history";
              "ct" = "open -t -- {clipboard}";
              "cw" = "open -w -- {clipboard}";
              "d" = "tab-close";
              "g$" = "tab-focus last";
              "g0" = "tab-focus 1";
              "gc" = "tab-clone";
              "gj" = "tab-move +";
              "gk" = "tab-move -";
              "go" = "spawn ${pkgs.chromium}/bin/chromium {url}";
              "gs" = "view-source";
              "gw" = "set-cmd-text -s :tab-give";
              "pt" = "open -t -- {primary}";
              "pw" = "open -w -- {primary}";
              "yy" = "yank";
              "yd" = "yank domain";
              "yM" = "spawn mpv {url}";
              "yo" = "yank inline [[{url}][{title}]]";
              "yp" = "yank pretty-url";
              "yt" = "yank title";
              "y;" = ''spawn ${pkgs.org-capture}/bin/org-capture -u "{url}" -t "{title}" -e title'';
              "y'" = ''spawn ${pkgs.org-capture}/bin/org-capture -u "{url}" -t "{title}" -b "{primary}" -e title'';
              "ym" = ''spawn ${pkgs.mpc_cli}/bin/mpc add yt:{url}'';
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
              # TODO: review and rework pass setup
              ",P" = "spawn --userscript qute-pass --dmenu-invocation dmenu --password-only";
              ",p" = "spawn --userscript qute-pass --dmenu-invocation dmenu";
              "zl" = "spawn --userscript qute-pass";
              "zpl" = "spawn --userscript qute-pass --password-only";
              "zul" = "spawn --userscript qute-pass --username-only";
              "@" = "run-macro";
              "AD" = "adblock-update";
              "CH" = "history-clear";
              "cr" = "config-source";
              "sf" = "save";
              "ws" = "config-write-py --force --defaults config.current.py";
              "i" = "enter-mode insert";
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
          assertion = !config.custom.browsers.chromium.isDefault && !config.custom.browsers.next.isDefault
            && !config.custom.browsers.firefox.isDefault;
          message = "browsers: qutebrowser: there should be exactly one default.";
        }
      ];

      environment.sessionVariables = { BROWSER = cfg.command; };
      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications =
          mapMimesToApp config.attributes.mimetypes.browser "org.custom.qutebrowser.windowed.desktop";
      };
      attributes.browser.default = cfg.command;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: qutebrowser: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.chromium.isFallback && !config.custom.browsers.next.isFallback
            && !config.custom.browsers.firefox.isFallback;
          message = "browsers: qutebrowser: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback = cfg.command;
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ yank-image qb-fix-session ];
      };
    })
  ];
}
