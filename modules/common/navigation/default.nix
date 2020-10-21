{ config, lib, pkgs, inputs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.navigation;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
  dmenu_runapps =
    mkShellScriptWithDeps "dmenu_runapps" (with pkgs; [ coreutils dmenu haskellPackages.yeganesh j4-dmenu-desktop ]) ''
      j4-dmenu-desktop --display-binary --dmenu="(cat ; (stest -flx $(echo $PATH | tr : ' ') | sort -u)) | \
        yeganesh -- -i -l 15 -fn '${config.wmCommon.fonts.dmenu}'"
    '';
  dmenu_select_windows = mkShellScriptWithDeps "dmenu_select_windows" (with pkgs; [ coreutils dmenu wmctrl ]) ''
    wmctrl -a $(wmctrl -l | cut -d" " -f5- | dmenu -i -l 15 -fn '${config.wmCommon.fonts.dmenu}')
  '';
in {
  options = {
    custom.navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable navigation infra.
        '';
      };
      webjumps.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable webjumps.";
      };
      webjumps.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Webjumps entries.";
      };
      webjumps.sep = mkOption {
        type = types.str;
        default = " | ";
        description = "Webjumps field separator.";
      };
      searchengines.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable searchengines.";
      };
      searchengines.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Searchengines entries.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        description = "Whether to enable bookmarks.";
        default = false;
      };
      bookmarks.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Bookmarks data.";
      };
      gmrun.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable gmrun.
        '';
      };
      gmrun.historySize = mkOption {
        type = types.int;
        default = 1024;
        description = ''
          History length.
        '';
      };
      gmrun.terminalApps = mkOption {
        type = types.listOf types.str;
        default = [ "info" "lynx" "man" "mc" "ssh" "vi" "vim" ];
        description = ''
          List of apps to always run in terminal.
        '';
      };
      mc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Midnight Commander.
        '';
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable misc setup.
        '';
      };
      snippets.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable snippets automation.";
      };
      snippets.entries = mkOption {
        type = types.listOf types.attrs;
        description = ''
          Various text snippets, mostly for development automation.
        '';
        default = [ ];
      };
      emacs.ivy.candidatesCount = mkOption {
        type = types.int;
        default = 10;
        description = "Candidates count to display for Ivy completion engine.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable customized navigation for Emacs.
        '';
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        search_prompt = mkPythonScriptWithDeps "search_prompt" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./scripts/search_prompt.py; })));
        search_selection = mkPythonScriptWithDeps "search_selection"
          (with pkgs; [ nurpkgs.pystdlib python3Packages.dmenu-python python3Packages.redis xsel ]) (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./scripts/search_selection.py; })));
        webjumps = mkPythonScriptWithDeps "webjumps" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis vpnctl ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./scripts/webjumps.py; })));
      };

      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ j4-dmenu-desktop ]; };
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        mcpanes = mkPythonScriptWithDeps "mcpanes" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./scripts/mcpanes.py; })));
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/bookmarks ${
          lib.strings.escapeNixString (builtins.toJSON cfg.bookmarks.entries)
        }
      '';
      home-manager.users."${config.attributes.mainUser.name}" = lib.optionalAttrs (cfg.emacs.enable) {
        home.activation.emacsKnownProjects = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = let # FIXME: duplication
            mainUserID = builtins.toString config.users.extraUsers."${config.attributes.mainUser.name}".uid;
            emacsServerSocketPath = "/run/user/${mainUserID}/emacs/server";
            # TODO: consider extracting to a function (for ensuring running emacs instance)
          in "[ -f ${emacsServerSocketPath} ] && ${config.ide.emacs.package}/bin/emacsclient -s /run/user/${mainUserID}/emacs/server -e '(mapcar (lambda (p) (projectile-add-known-project p)) (list ${
            builtins.concatStringsSep " "
            (forEach (lib.attrValues cfg.bookmarks.entries) (entry: ''"'' + entry + ''"''))
          }))' ";
        };
      };
    })
    (mkIf (cfg.enable && cfg.webjumps.enable) {
      assertions = [{
        assertion = cfg.webjumps.enable && cfg.webjumps.entries != { };
        message = "navigation: no webjumps to follow but they are enabled.";
      }];

      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/webjumps ${
          lib.strings.escapeNixString (builtins.toJSON (lib.mapAttrs' (url: meta:
            lib.nameValuePair
            (if lib.hasAttrByPath [ "title" ] meta then (url + cfg.webjumps.sep + meta.title) else url)
            (if lib.hasAttrByPath [ "browser" ] meta then
              (meta.browser + " " + url)
            else
              ("${pkgs.xdg_utils}/bin/xdg-open " + url)))
            (filterAttrs (_: v: (!builtins.hasAttr "enable" v) || ((builtins.hasAttr "enable" v) && v.enable == true))
              cfg.webjumps.entries)))
        }
        ${pkgs.redis}/bin/redis-cli set nav/webjumps_vpn ${
          lib.strings.escapeNixString (builtins.toJSON (lib.mapAttrs' (url: meta:
            lib.nameValuePair
            (if lib.hasAttrByPath [ "title" ] meta then (url + cfg.webjumps.sep + meta.title) else url)
            (if lib.hasAttrByPath [ "vpn" ] meta then meta.vpn else "")) cfg.webjumps.entries))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.searchengines.enable) {
      assertions = [{
        assertion = cfg.searchengines.enable && cfg.searchengines.entries != { };
        message = "navigation: no searchengines to follow but they are enabled.";
      }];

      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/searchengines ${
          lib.strings.escapeNixString (builtins.toJSON cfg.searchengines.entries)
        }
      '';
    })
    (mkIf (cfg.enable && cfg.gmrun.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ gmrun ];
        home.file = {
          ".gmrunrc".text = (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./gmrunrc; })));
        };
      };
    })
    (mkIf (cfg.enable && cfg.mc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ mc ];
        xdg.configFile."mc/mc.ext".text = ''
          regex/\.([pP][dD][fF])$
              Include=shovel
          regex/\.([dD][jJ][vV][uU])$
              Include=shovel

          regex/\.([jJ][pP][gG])$
              Include=shovel
          regex/\.([pP][nN][gG])$
              Include=shovel
          regex/\.([jJ][pP][eE][gG])$
              Include=shovel

          regex/\.([mM][pP]4)$
              Include=shovel
          regex/\.([fF][lL][vV])$
              Include=shovel
          regex/\.([mM][kK][vV])$
              Include=shovel

          regex/\.([dD][oO][cC])$
              Include=shovel
          regex/\.([dD][oO][cC][xX])$
              Include=shovel
          regex/\.([xX][lL][sS])$
              Include=shovel
          regex/\.([xX][lL][sS][xX])$
              Include=shovel
          regex/\.([pP][pP][tT])$
              Include=shovel
          regex/\.([pP][pP][sS])$
              Include=shovel
          regex/\.([pP][pP][tT][xX])$
              Include=shovel
          regex/\.([pP][pP][sS][xX])$
              Include=shovel

          include/shovel
              Open=(${pkgs.xdg_utils}/bin/xdg-open %f >/dev/null 2>&1 &)
        '';
      };
    })
    (mkIf (cfg.enable && cfg.misc.enable) {
      custom.xinput.xkeysnail.rc = lib.mkAfter ''
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
    })
    (mkIf (cfg.enable && cfg.snippets.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        snippets = mkPythonScriptWithDeps "snippets" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis xsel ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./scripts/snippets.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ snippets ]; };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/snippets ${
          lib.strings.escapeNixString (builtins.toJSON (builtins.listToAttrs (forEach cfg.snippets.entries (s:
            nameValuePair
            "${lib.concatStringsSep ":" (maybeAttrList "tags" s "-")} | ${(maybeAttrString "description" s "-")} | ${
              (maybeAttrString "language" s "-")
            } | ${s.code}" s.code))))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ ripgrep ]; };
      ide.emacs.extraPackages = epkgs: [
        epkgs.ace-link
        epkgs.ace-window
        epkgs.avy
        epkgs.avy-flycheck
        epkgs.avy-zap
        epkgs.counsel
        epkgs.counsel-projectile
        epkgs.counsel-tramp
        epkgs.dired-filetype-face
        epkgs.dired-git-info
        epkgs.dired-hide-dotfiles
        epkgs.dired-launch
        epkgs.dired-narrow
        epkgs.dired-quick-sort
        epkgs.imenu-anywhere
        epkgs.ivy
        epkgs.ivy-avy
        epkgs.ivy-historian
        epkgs.ivy-rich
        epkgs.ivy-xref
        epkgs.ivy-yasnippet
        epkgs.link-hint
        epkgs.phi-search
        epkgs.phi-search-mc
        epkgs.polymode
        epkgs.projectile
        epkgs.rg
        epkgs.swiper
      ];
      ide.emacs.config = builtins.readFile (pkgs.substituteAll
        ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./emacs/navigation.el; }));
    } // lib.optionalAttrs (true) {
      ide.emacs.config = builtins.readFile (pkgs.substituteAll
        ((import ../subst.nix { inherit config pkgs lib inputs; }) // { src = ./emacs/browsers.el; }));
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ dmenu_runapps dmenu_select_windows ];
      };
      wmCommon.keys = [
        {
          key = [ prefix "slash" ];
          cmd = "${pkgs.search_selection}/bin/search_selection";
          mode = "root";
        }
        {
          key = [ prefix "Control" "slash" ];
          cmd = "${pkgs.search_prompt}/bin/search_prompt";
          mode = "root";
        }
        {
          key = [ prefix "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps";
          mode = "root";
        }
        {
          key = [ "XF86Launch1" ];
          cmd = "${dmenu_runapps}/bin/dmenu_runapps";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "Return" ];
          cmd = "${config.custom.shell.terminal}";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "p" ];
          cmd = "${dmenu_runapps}/bin/dmenu_runapps";
          mode = "root";
        }
        {
          key = [ "w" ];
          cmd = "${dmenu_select_windows}/bin/dmenu_select_windows";
          mode = "window";
        }
      ] ++ lib.optionals (cfg.snippets.enable) [{
        key = [ "Shift" "s" ];
        cmd = "${pkgs.snippets}/bin/snippets";
        mode = "run";
      }] ++ lib.optionals (cfg.bookmarks.enable) [{
        key = [ "Shift" "m" ];
        cmd = "${pkgs.mcpanes}/bin/mcpanes";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ mcpanes search_prompt search_selection snippets webjumps ];
      };
    })
  ];
}
