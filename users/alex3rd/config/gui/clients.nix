{config, pkgs, lib, ...}:

with import <home-manager/modules/lib/dag.nix> { inherit lib; }; # TODO: make more declarative
with import ../../../../pkgs/util.nix {inherit lib config pkgs;};
with import ../../const.nix {inherit config pkgs;};
let
    firefox-addons = pkgs.recurseIntoAttrs (pkgs.callPackage ../../../../pkgs/firefox-addons { });
in
{
    home-manager.users."${userName}" = {
        programs.firefox = {
            enable = true;
            package = pkgs.firefox.overrideAttrs (attrs: {
                enableTridactylNative = true;
            });
            extensions = with firefox-addons; [
                display-anchors
                ghosttext
                tridactyl
                url-in-title
            ];
            profiles = {
                default = {
                    name = "profile.default";
                    path = "profile.default";
                    settings = {
                        "extensions.autoDisableScopes" = 0;
                        "browser.ctrlTab.recentlyUsedOrder" = false;
                        "browser.download.dir" = "/home/${userName}/Downloads";
                        "browser.link.open_newwindow" = 2;
                        "browser.sessionstore.restore_on_demand" = true;
                        "browser.sessionstore.restore_tabs_lazily" = true;
                        "browser.shell.checkDefaultBrowser" = true;
                        "browser.startup.page" = 3;
                        "extensions.pocket.enabled" = false;
                        "lightweightThemes.selectedThemeID" = "firefox-compact-dark@mozilla.org";
                    };
                    handlers = {
                        defaultHandlersVersion = {
                            "en-US" = 4;
                        };
                        mimeTypes = {
                            "application/pdf" = {
                                action = 3;
                            };
                        };
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
                            "org-protocol" = {
                                action = 4;
                            };
                            "tg" = {
                                action = 4;
                            };
                        };
                    };
                };
            };
        };
        programs.chromium = {
            enable = true;
            extensions = [
                "gfbliohnnapiefjpjlpjnehglfpaknnc" # surfingkeys
                "ignpacbgnbnkaiooknalneoeladjnfgb" # url in title
                "poahndpaaanbpbeafbkploiobpiiieko" # display anchors
            ];
        };
        home.file = {
            ".mozilla/firefox/profile.default/browser-extension-data/{d47d18bc-d6ba-4f96-a144-b3016175f3a7}/storage.js".text = builtins.toJSON {
                protocol = false;
                path = true;
                delimiter = " // ";
            };
            ".config/imv/config".text = genIni {
                "options" = {
                    # Suppress built-in key bindings, and specify them explicitly in this
                    # config file.
                    "suppress_default_binds" = true;
                };
                "aliases" = {
                    "q" = "quit";
                    "next" = "select_rel 1";
                    "previous" = "select_rel -1";
                    "n" = "select_rel 1";
                    "p" = "select_rel -1";
                };
                "binds" = {
                    "q" = "quit";

                    # Image navigation
                    "<Left>" = "select_rel -1";
                    "<LeftSquareBracket>" = "select_rel -1";
                    "<Right>" = "select_rel 1";
                    "<RightSquareBracket>" = "select_rel 1";
                    "gg" = "select_abs 0";
                    "<Shift+g>" = "select_abs -1";

                    # Panning
                    "j" = "pan 0 -50";
                    "k" = "pan 0 50";
                    "h" = "pan 50 0";
                    "l" = "pan -50 0";

                    # Zooming
                    "<Up>" = "zoom 1";
                    "i" = "zoom 1";
                    "<Down>" = "zoom -1";
                    "o" = "zoom -1";

                    # Other commands
                    "x" = "close";
                    "f" = "fullscreen";
                    "d" = "overlay";
                    "p" = "exec echo $imv_current_file";
                    "c" = "center";
                    "s" = "scaling_mode next";
                    "a" = "zoom actual";
                    "r" = "reset";

                    # Gif playback
                    "." = "next_frame";
                    "<Space>" = "toggle_playing";

                    # Slideshow control
                    "t" = "slideshow_duration +1";
                    "<Shift+t>" = "slideshow_duration -1";
                };
            };
            ".config/tridactyl/tridactylrc".text = ''
                " Move this to $XDG_CONFIG_DIR/tridactyl/tridactylrc (that's
                " ~/.config/tridactyl/tridactylrc to mere mortals) or ~/.tridactylrc and
                " install the native messenger (:installnative in Tridactyl). Run :source to
                " get it in the browser, or just restart.

                " NB: If you want "vim-like" behaviour where removing a line from
                " here makes the setting disappear, uncomment the line below.
                "sanitise tridactyllocal tridactylsync

                set storageloc local

                set historyresults 100

                colorscheme dark

                guiset_quiet tabs autohide
                guiset_quiet navbar autohide
                guiset_quiet hoverlink top-right

                "
                " Binds
                "

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

                " set editorcmd to suckless terminal, or use the defaults on other platforms
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

                "
                " URL redirects
                "

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
        };
    };
}
