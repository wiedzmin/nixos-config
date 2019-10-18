{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.navigation;
in {
  options = {
    navigation = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable navigation infra.
        '';
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
        default = [
          "info"
          "lynx"
          "man"
          "mc"
          "ssh"
          "vi"
          "vim"
        ];
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
      rofi.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable rofi.
        '';
      };
      rofi.location = mkOption {
        type = types.str;
        default = "center";
        description = ''
          Rofi window location.
        '';
      };
      rofi.separator = mkOption {
        default = "none";
        type = types.nullOr (types.enum [ "none" "dash" "solid" ]);
        description = "Separator style";
      };
      rofi.theme = mkOption {
        type = types.str;
        default = "gruvbox-dark-hard";
        description = ''
          Rofi theme to use.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.gmrun.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          gmrun
        ];
        home.file = {
          ".gmrunrc".text = ''
            AlwaysInTerm = ${lib.concatStringsSep " " cfg.gmrun.terminalApps}

            Width = 400
            Top = 100
            Left = 200

            History = ${builtins.toString cfg.gmrun.historySize}

            ShowLast = 1
            ShowDotFiles = 1
            TabTimeout = 0

            URL_http = ${config.attributes.defaultCommands.browser} %u
            URL_man = ${config.attributes.defaultCommands.terminal} 'man %s'
            URL_info = ${config.attributes.defaultCommands.terminal} 'info %s'
            URL_readme = ${config.attributes.defaultCommands.terminal} '${config.attributes.defaultCommands.pager} /usr/doc/%s/README'
            URL_info = ${config.attributes.defaultCommands.terminal} 'info %s'
            URL_sh = sh -c '%s'

            EXT:doc,docx,rtf = ${config.attributes.defaultCommands.textProcessor} %s
            EXT:xls,xlsx = ${config.attributes.defaultCommands.spreadsheetEditor} %s
            EXT:txt,cc,cpp,h,java,html,htm,epl,tex,latex,js,css,xml,xsl,am = emacs %s
            EXT:ps = ${config.attributes.defaultCommands.ebookReader} %s
            EXT:pdf = ${config.attributes.defaultCommands.ebookReader} %s
          '';
        };
      };
    })
    (mkIf (cfg.enable && cfg.mc.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          mc
        ];
        xdg.configFile."mc/mc.ext".text = ''
          regex/\.([pP][dD][fF])$
              Include=ebook
          regex/\.([dD][jJ][vV][uU])$
              Include=ebook

          regex/\.([jJ][pP][gG])$
              Include=image
          regex/\.([pP][nN][gG])$
              Include=image
          regex/\.([jJ][pP][eE][gG])$
              Include=image

          regex/\.([mM][pP]4)$
              Include=video
          regex/\.([fF][lL][vV])$
              Include=video

          include/ebook
              Open=(${config.attributes.defaultCommands.ebookReader} %f >/dev/null 2>&1 &)

          include/image
              Open=(${config.attributes.defaultCommands.imageViewer} %f >/dev/null 2>&1 &)

          include/video
              Open=(${config.attributes.defaultCommands.videoPlayer} %f >/dev/null 2>&1 &)
        '';
      };
    })
    (mkIf (cfg.enable && cfg.rofi.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.rofi = {
          enable = true;
          fullscreen = false;
          borderWidth = 1;
          cycle = true;
          rowHeight = 1;
          scrollbar = true;
          lines = 15;
          location = "${cfg.rofi.location}";
          padding = 5;
          separator = "${cfg.rofi.separator}";
          width = 80;
          xoffset = 0;
          yoffset = 0;
          theme = "${cfg.rofi.theme}";
          terminal = "${config.attributes.defaultCommands.terminal}";
          extraConfig = ''
            rofi.line-margin:                    3
            rofi.scroll-method:                  1
            rofi.scrollbar-width:                8
            rofi.hide-scrollbar:                 true

            rofi.modi:                           combi,drun,keys,run,ssh,window
            rofi.combi-modi:                     window,run,ssh
            rofi.matching:                       normal
            rofi.tokenize:                       true
            rofi.disable-history:                false
            rofi.levenshtein-sort:               true
            rofi.threads:                        0

            rofi.run-command:                    {cmd}
            rofi.run-shell-command:              {terminal} {cmd}
            rofi.window-command:                 xkill -id {window}
            rofi.window-format:                  {w}   {c}   {t}
            rofi.window-match-fields:            title,class

            rofi.parse-hosts:                    true
            rofi.parse-known-hosts:              false
            rofi.ssh-command:                    ${pkgs.tmux}/bin/tmux new-window '${pkgs.eternal-terminal}/bin/et {host}'

            rofi.kb-accept-alt:                  Shift+Return
            rofi.kb-accept-custom:               Control+Return
            rofi.kb-accept-entry:                Control+j,Control+m,Return,KP_Enter
            rofi.kb-cancel:                      Escape,Control+g,Control+bracketleft
            rofi.kb-clear-line:                  Control+w
            rofi.kb-custom-10:                   Alt+0
            rofi.kb-custom-11:                   Alt+exclam
            rofi.kb-custom-12:                   Alt+at
            rofi.kb-custom-13:                   Alt+numbersign
            rofi.kb-custom-14:                   Alt+dollar
            rofi.kb-custom-15:                   Alt+percent
            rofi.kb-custom-16:                   Alt+dead_circumflex
            rofi.kb-custom-17:                   Alt+ampersand
            rofi.kb-custom-18:                   Alt+asterisk
            rofi.kb-custom-19:                   Alt+parenleft
            rofi.kb-custom-1:                    Alt+1
            rofi.kb-custom-2:                    Alt+2
            rofi.kb-custom-3:                    Alt+3
            rofi.kb-custom-4:                    Alt+4
            rofi.kb-custom-5:                    Alt+5
            rofi.kb-custom-6:                    Alt+6
            rofi.kb-custom-7:                    Alt+7
            rofi.kb-custom-8:                    Alt+8
            rofi.kb-custom-9:                    Alt+9
            rofi.kb-delete-entry:                Shift+Delete
            rofi.kb-mode-next:                   Shift+Right,Control+Tab
            rofi.kb-mode-previous:               Shift+Left,Control+Shift+Tab
            rofi.kb-move-char-back:              Left,Control+b
            rofi.kb-move-char-forward:           Right,Control+f
            rofi.kb-move-end:                    Control+e
            rofi.kb-move-front:                  Control+a
            rofi.kb-move-word-back:              Alt+b
            rofi.kb-move-word-forward:           Alt+f
            rofi.kb-page-next:                   Page_Down
            rofi.kb-page-prev:                   Page_Up
            rofi.kb-primary-paste:               Control+V,Shift+Insert
            rofi.kb-remove-char-back:            BackSpace,Control+h
            rofi.kb-remove-char-forward:         Delete,Control+d
            rofi.kb-remove-to-eol:               Control+k
            rofi.kb-remove-to-sol:               Control+u
            rofi.kb-remove-word-back:            Control+Alt+h,Control+BackSpace
            rofi.kb-remove-word-forward:         Control+Alt+d
            rofi.kb-row-down:                    Down,Control+n
            rofi.kb-row-first:                   Home,KP_Home
            rofi.kb-row-last:                    End,KP_End
            rofi.kb-row-left:                    Control+Page_Up
            rofi.kb-row-right:                   Control+Page_Down
            rofi.kb-row-select:                  Control+space
            rofi.kb-row-tab:                     Tab
            rofi.kb-row-up:                      Up,Control+p,Shift+Tab,Shift+ISO_Left_Tab
            rofi.kb-screenshot:                  Alt+S
            rofi.kb-secondary-paste:             Control+v,Insert
            rofi.kb-toggle-case-sensitivity:     grave,dead_grave
            rofi.kb-toggle-sort:                 Alt+grave
          '';
        };
      };
    })
  ];
}
