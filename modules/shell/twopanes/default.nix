{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.twopanes;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    shell.twopanes = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable two-panes filesystem navigation";
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
      nixpkgs.config.packageOverrides = _: {
        mcpanes = mkPythonScriptWithDeps pkgs "mcpanes" (with pkgs; [ mc nurpkgs.pystdlib python3Packages.redis ])
          (builtins.readFile ./scripts/mcpanes.py);
      };

      home-manager.users."${user}" = {
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
        # .ini
        xdg.configFile."mc/mc.ext.ini".text = ''
          [mc.ext.ini]
          Version=4.0

          [pdf]
          Regex=\.([pP][dD][fF])$
          Include=shovel

          [djvu]
          Regex=\.([dD][jJ][vV][uU])$
          Include=shovel

          [jpg]
          Regex=\.([jJ][pP][gG])$
          Include=shovel

          [png]
          Regex=\.([pP][nN][gG])$
          Include=shovel

          [jpeg]
          Regex=\.([jJ][pP][eE][gG])$
          Include=shovel

          [mp4]
          Regex=\.([mM][pP]4)$
          Include=shovel

          [flv]
          Regex=\.([fF][lL][vV])$
          Include=shovel

          [mkv]
          Regex=\.([mM][kK][vV])$
          Include=shovel

          [doc]
          Regex=\.([dD][oO][cC])$
          Include=shovel

          [docx]
          Regex=\.([dD][oO][cC][xX])$
          Include=shovel

          [xls]
          Regex=\.([xX][lL][sS])$
          Include=shovel

          [xlsx]
          Regex=\.([xX][lL][sS][xX])$
          Include=shovel

          [ppt]
          Regex=\.([pP][pP][tT])$
          Include=shovel

          [pps]
          Regex=\.([pP][pP][sS])$
          Include=shovel

          [pptx]
          Regex=\.([pP][pP][tT][xX])$
          Include=shovel

          [ppsx]
          Regex=\.([pP][pP][sS][xX])$
          Include=shovel

          [Include/shovel]
          Open=(${pkgs.xdg-utils}/bin/xdg-open %f >/dev/null 2>&1 &)
        '';
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.common = [{
        key = [ "Shift" "m" ];
        cmd = "${pkgs.mcpanes}/bin/mcpanes";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ mcpanes ];
      };
    })
  ];
}
