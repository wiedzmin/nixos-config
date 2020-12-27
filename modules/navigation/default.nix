{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.navigation;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
  dmenu_runapps = mkShellScriptWithDeps "dmenu_runapps"
    (with pkgs; [ coreutils nurpkgs.dmenu-ng haskellPackages.yeganesh j4-dmenu-desktop ]) ''
      j4-dmenu-desktop --display-binary --dmenu="(cat ; (stest -flx $(echo $PATH | tr : ' ') | sort -u)) | \
        yeganesh -- -i -l 15 -fn '${config.wmCommon.fonts.dmenu}'"
    '';
  dmenu_select_windows =
    mkShellScriptWithDeps "dmenu_select_windows" (with pkgs; [ coreutils nurpkgs.dmenu-ng wmctrl ]) ''
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
      bookmarks.enable = mkOption {
        type = types.bool;
        description = "Whether to enable bookmarks.";
        default = false;
      };
      bookmarks.sep = mkOption {
        type = types.str;
        default = " | ";
        description = "Bookmarks field separator.";
      };
      bookmarks.tagSep = mkOption {
        type = types.str;
        default = ":";
        description = "Bookmarks tags separator.";
      };
      bookmarks.entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Bookmarks data.";
      };
      workspaceRoots = mkOption {
         type = types.attrs;
         default = { };
         description = "Various workspace roots meta.";
      };
      workspaceRootGlobal = mkOption {
         type = types.str;
         default = "";
         description = "Various workspace roots meta.";
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
          (readSubstituted ../subst.nix ./scripts/search_prompt.py);
        search_selection =
          mkPythonScriptWithDeps "search_selection" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis xsel ])
          (readSubstituted ../subst.nix ./scripts/search_selection.py);
        webjumps = mkPythonScriptWithDeps "webjumps" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis vpnctl xsel ])
          (readSubstituted ../subst.nix ./scripts/webjumps.py);
      };

      home-manager.users.${user} = { home.packages = with pkgs; [ j4-dmenu-desktop ]; };

      custom.pim.timeTracking.rules = with config.attributes.browser; ''
        -- TODO: parameterize web resources
        current window $program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] ==> tag activity:web,
        current window ($program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] && $title =~ /Gmail/) ==> tag web:Gmail,
        current window ($program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] && $title =~ /Google/) ==> tag web:Google,
        current window ($program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] && $title =~ /wikipedia/) ==> tag site:wikipedia,
      '';
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        mcpanes = mkPythonScriptWithDeps "mcpanes" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (readSubstituted ../subst.nix ./scripts/mcpanes.py);
      };
      home-manager.users.${user} = lib.optionalAttrs (cfg.emacs.enable) {
        home.activation.emacsKnownProjects = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = let # FIXME: duplication
            mainUserID = builtins.toString config.users.extraUsers."${config.attributes.mainUser.name}".uid;
            emacsServerSocketPath = "/run/user/${mainUserID}/emacs/server";
            # TODO: consider extracting to a function (for ensuring running emacs instance)
          in "[ -f ${emacsServerSocketPath} ] && ${config.ide.emacs.package}/bin/emacsclient -s /run/user/${mainUserID}/emacs/server -e '(mapcar (lambda (p) (projectile-add-known-project p)) (list ${
            builtins.concatStringsSep " " (localEmacsBookmarks cfg.bookmarks.entries)
          }))' ";
        };
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set nav/webjumps ${
          lib.strings.escapeNixString
          (builtins.toJSON (remoteWebjumps (enabledRemotes cfg.bookmarks.entries) cfg.bookmarks.sep cfg.bookmarks.tagSep))
        }
        ${pkgs.redis}/bin/redis-cli set nav/searchengines ${
          lib.strings.escapeNixString (builtins.toJSON (remoteSearchEngines (enabledRemotes cfg.bookmarks.entries)
            cfg.bookmarks.sep cfg.bookmarks.tagSep))
        }
        ${pkgs.redis}/bin/redis-cli set nav/bookmarks ${
          lib.strings.escapeNixString (builtins.toJSON (enabledLocals cfg.bookmarks.entries))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.gmrun.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ gmrun ];
        home.file = { ".gmrunrc".text = readSubstituted ../subst.nix ./gmrunrc; };
      };
    })
    (mkIf (cfg.enable && cfg.mc.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ mc ];
        xdg.configFile."mc/ini".text = ''
          [Midnight-Commander]
          verbose=true
          shell_patterns=true
          auto_save_setup=true
          preallocate_space=false
          auto_menu=false
          use_internal_view=true
          use_internal_edit=true
          clear_before_exec=true
          confirm_delete=true
          confirm_overwrite=true
          confirm_execute=false
          confirm_history_cleanup=true
          confirm_exit=false
          confirm_directory_hotlist_delete=false
          confirm_view_dir=false
          safe_delete=false
          use_8th_bit_as_meta=false
          mouse_move_pages_viewer=true
          mouse_close_dialog=false
          fast_refresh=false
          drop_menus=false
          wrap_mode=true
          old_esc_mode=true
          cd_symlinks=true
          show_all_if_ambiguous=false
          use_file_to_guess_type=true
          alternate_plus_minus=false
          only_leading_plus_minus=true
          show_output_starts_shell=false
          xtree_mode=false
          file_op_compute_totals=true
          classic_progressbar=true
          use_netrc=true
          ftpfs_always_use_proxy=false
          ftpfs_use_passive_connections=true
          ftpfs_use_passive_connections_over_proxy=false
          ftpfs_use_unix_list_options=true
          ftpfs_first_cd_then_ls=true
          ignore_ftp_chattr_errors=true
          editor_fill_tabs_with_spaces=false
          editor_return_does_auto_indent=true
          editor_backspace_through_tabs=false
          editor_fake_half_tabs=true
          editor_option_save_position=true
          editor_option_auto_para_formatting=false
          editor_option_typewriter_wrap=false
          editor_edit_confirm_save=true
          editor_syntax_highlighting=true
          editor_persistent_selections=true
          editor_drop_selection_on_copy=true
          editor_cursor_beyond_eol=false
          editor_cursor_after_inserted_block=false
          editor_visible_tabs=true
          editor_visible_spaces=true
          editor_line_state=false
          editor_simple_statusbar=false
          editor_check_new_line=false
          editor_show_right_margin=false
          editor_group_undo=false
          editor_state_full_filename=false
          editor_ask_filename_before_edit=false
          nice_rotating_dash=true
          mcview_remember_file_position=false
          auto_fill_mkdir_name=true
          copymove_persistent_attr=true
          pause_after_run=1
          mouse_repeat_rate=100
          double_click_speed=250
          old_esc_mode_timeout=1000000
          max_dirt_limit=10
          num_history_items_recorded=60
          vfs_timeout=60
          ftpfs_directory_timeout=900
          ftpfs_retry_seconds=30
          fish_directory_timeout=900
          editor_tab_spacing=8
          editor_word_wrap_line_length=72
          editor_option_save_mode=0
          editor_backup_extension=~
          editor_filesize_threshold=64M
          editor_stop_format_chars=-+*\\,.;:&>
          mcview_eof=
          skin=julia256

          safe_overwrite=false

          filepos_max_saved_entries=1024

          [Layout]
          message_visible=true
          keybar_visible=true
          xterm_title=true
          output_lines=0
          command_prompt=true
          menubar_visible=true
          free_space=true
          horizontal_split=false
          vertical_equal=true
          left_panel_size=85
          horizontal_equal=true
          top_panel_size=27

          [Misc]
          timeformat_recent=%b %e %H:%M
          timeformat_old=%b %e  %Y
          ftp_proxy_host=gate
          ftpfs_password=anonymous@
          display_codepage=UTF-8
          source_codepage=Other_8_bit
          autodetect_codeset=
          clipboard_store=
          clipboard_paste=

          [Colors]
          base_color=
          xterm-256color=
          color_terminals=

          linux=

          screen=

          rxvt-unicode-256color=

          xterm=

          screen-256color=

          [Panels]
          show_mini_info=true
          kilobyte_si=false
          mix_all_files=false
          show_backups=true
          show_dot_files=true
          fast_reload=false
          fast_reload_msg_shown=false
          mark_moves_down=true
          reverse_files_only=true
          auto_save_setup_panels=false
          navigate_with_arrows=false
          panel_scroll_pages=true
          panel_scroll_center=false
          mouse_move_pages=true
          filetype_mode=true
          permission_mode=false
          torben_fj_mode=false
          quick_search_mode=2
          select_flags=6

          [FindFile]
          file_case_sens=true
          file_shell_pattern=true
          file_find_recurs=true
          file_skip_hidden=false
          file_all_charsets=false
          content_case_sens=true
          content_regexp=false
          content_first_hit=false
          content_whole_words=false
          content_all_charsets=false
          ignore_dirs_enable=true
          ignore_dirs=

          [Panelize]
          Изменённые файлы под контролем git=git ls-files --modified
          Найти корректуры, отвергнутые командой patch=find . -name \\*.rej -print
          Найти оригиналы (*.orig) после команды patch=find . -name \\*.orig -print
          Найти программы с установленными SUID/SGID битами=find . \\( \\( -perm -04000 -a -perm /011 \\) -o \\( -perm -02000 -a -perm /01 \\) \\) -print
        '';
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
          (readSubstituted ../subst.nix ./scripts/snippets.py);
      };
      home-manager.users.${user} = { home.packages = with pkgs; [ snippets ]; };
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
      home-manager.users.${user} = { home.packages = with pkgs; [ ripgrep ]; };
      ide.emacs.extraPackages = epkgs: [
        epkgs.ace-link
        epkgs.ace-window
        epkgs.avy
        epkgs.avy-flycheck
        epkgs.avy-zap
        epkgs.block-nav
        epkgs.counsel
        epkgs.counsel-jq
        epkgs.counsel-projectile
        epkgs.counsel-tramp
        epkgs.dired-filetype-face
        epkgs.dired-git-info
        epkgs.dired-hide-dotfiles
        epkgs.dired-launch
        epkgs.dired-narrow
        epkgs.dired-quick-sort
        epkgs.goggles
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
        epkgs.treemacs
        epkgs.treemacs-projectile
      ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/navigation.el;
    } // lib.optionalAttrs (true) { ide.emacs.config = readSubstituted ../subst.nix ./emacs/browsers.el; })
    (mkIf (cfg.enable && cfg.wm.enable) {
      home-manager.users.${user} = { home.packages = with pkgs; [ dmenu_runapps dmenu_select_windows ]; };
      wmCommon.keys = [
        {
          key = [ prefix "slash" ];
          cmd = "${pkgs.search_selection}/bin/search_selection";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "slash" ];
          cmd = "${pkgs.search_selection}/bin/search_selection --fallback";
          mode = "root";
        }
        {
          key = [ prefix "Control" "slash" ];
          cmd = "${pkgs.search_prompt}/bin/search_prompt";
          mode = "root";
        }
        {
          key = [ prefix "Control" "Shift" "slash" ];
          cmd = "${pkgs.search_prompt}/bin/search_prompt --fallback";
          mode = "root";
        }
        {
          key = [ prefix "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps --fallback";
          mode = "root";
        }
        {
          key = [ prefix "Control" "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps --copy";
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
          mode = "select";
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
      home-manager.users.${user} = {
        home.packages = with pkgs; [ mcpanes search_prompt search_selection snippets webjumps ];
      };
    })
  ];
}
