{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.custom.programs.tmux;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};

  pluginName = p: if types.package.check p then p.name else p.plugin.name;

  bindingsModule = types.submodule {
    options = {
      copyMode = mkOption {
        type = types.attrsOf types.str;
        description = "copy-mode bindings.";
        default = { };
      };

      joinPaneMode = mkOption {
        type = types.attrsOf types.str;
        description = "join-pane mode bindings.";
        default = { };
      };

      prefixed = mkOption {
        type = types.attrsOf types.str;
        description = "Bindings used with ${cfg.shortcut}.";
        default = { };
      };

      root = mkOption {
        type = types.attrsOf types.str;
        description = "Unprefixed bindings, used as is.";
        default = { };
      };
    };
  };

  pluginModule = types.submodule {
    options = {
      plugin = mkOption {
        type = types.package;
        description = "Path of the configuration file to include.";
      };

      extraConfig = mkOption {
        type = types.lines;
        description = "Additional configuration for the associated plugin.";
        default = "";
      };
    };
  };

  statusModule = types.submodule {
    options = {
      currentWindowFormat = mkOption {
        default = "";
        type = types.str;
        description = ''
          Current window status formatting.
        '';
      };

      currentWindowStyle = mkOption {
        default = "";
        type = types.str;
        description = ''
          Current window style.
        '';
      };

      justification = mkOption {
        default = "centre";
        type = types.str;
        description = ''
          How to justify statusbar info.
        '';
      };

      keyMode = mkOption {
        default = defaultKeyMode;
        type = types.str;
        description = ''
          VI or Emacs style shortcuts.
        '';
      };

      leftFormat = mkOption {
        default = "";
        type = types.str;
        description = ''
          Left status part formatting.
        '';
      };

      leftLength = mkOption {
        default = 30;
        type = types.int;
        description = ''
          Length of left status part.
        '';
      };

      messageStyle = mkOption {
        default = "";
        type = types.str;
        description = ''
          Message area style.
        '';
      };

      rightFormat = mkOption {
        default = "";
        type = types.str;
        description = ''
          Left status part formatting.
        '';
      };

      rightLength = mkOption {
        default = 140;
        type = types.int;
        description = ''
          Length of right status part.
        '';
      };

      style = mkOption {
        default = "";
        type = types.str;
        description = ''
          Style.
        '';
      };

      updateInterval = mkOption {
        default = 1;
        type = types.int;
        description = ''
          Seconds to pass between statusbar updates.
        '';
      };

      windowFormat = mkOption {
        default = "";
        type = types.str;
        description = ''
          Window status formatting.
        '';
      };

      windowStyle = mkOption {
        default = "";
        type = types.str;
        description = ''
          Current style.
        '';
      };
    };
  };

  defaultKeyMode = "emacs";
  defaultResize = 5;
  defaultShell = "$SHELL";
  defaultShortcut = if versionAtLeast hm.home.stateVersion "19.09" then "C-b" else "b";
  defaultTerminal = "screen";

  boolToStr = value: if value then "on" else "off";

  tmuxConf = ''
    set  -g default-shell "${cfg.shell}"
    set  -g default-terminal "${cfg.terminal}"
    set  -g base-index      ${toString cfg.baseIndex}
    setw -g pane-base-index ${toString cfg.baseIndex}

    ${optionalString cfg.newSession "new-session"}

    ${optionalString cfg.reverseSplit ''
      bind v split-window -h
      bind s split-window -v
    ''}

    set -g status-keys ${cfg.keyMode}
    set -g mode-keys   ${cfg.keyMode}

    ${optionalString (cfg.borderStyle.active != "") "set -g pane-active-border-style ${cfg.borderStyle.active}"}
    ${optionalString (cfg.borderStyle.inactive != "") "set -g pane-border-style ${cfg.borderStyle.inactive}"}

    ${optionalString (cfg.keyMode == "vi" && cfg.customPaneNavigationAndResize) ''
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      bind -r H resize-pane -L ${toString cfg.resizeAmount}
      bind -r J resize-pane -D ${toString cfg.resizeAmount}
      bind -r K resize-pane -U ${toString cfg.resizeAmount}
      bind -r L resize-pane -R ${toString cfg.resizeAmount}
    ''}

    ${if versionAtLeast hm.home.stateVersion "19.09" then ''
      # rebind main key: ${cfg.shortcut}
      unbind ${defaultShortcut}
      set -g prefix ${cfg.shortcut}
      bind ${cfg.nestedShortcut} send-prefix
    '' else ''
      ${optionalString (cfg.shortcut != defaultShortcut) ''
        # rebind main key: C-${cfg.shortcut}
        unbind C-${defaultShortcut}
        set -g prefix C-${cfg.shortcut}
        bind ${cfg.shortcut} send-prefix
        bind C-${cfg.shortcut} last-window
      ''}
    ''}

    ${optionalString cfg.disableConfirmationPrompt ''
      bind-key & kill-window
      bind-key x kill-pane
    ''}

    setw -g aggressive-resize ${boolToStr cfg.aggressiveResize}
    setw -g clock-mode-style  ${if cfg.clock24 then "24" else "12"}
    set  -s escape-time       ${toString cfg.escapeTime}
    set  -g history-limit     ${toString cfg.historyLimit}

    ${if cfg.status != null then ''
      set -g status on

      set -g status-interval ${builtins.toString cfg.status.updateInterval}
      set -g status-justify ${cfg.status.justification}
      set -g status-left-length ${builtins.toString cfg.status.leftLength}
      set -g status-right-length ${builtins.toString cfg.status.rightLength}

      ${optionalString (cfg.status.currentWindowFormat != "")
      "setw -g window-status-current-format '${cfg.status.currentWindowFormat}'"}
      ${optionalString (cfg.status.windowFormat != "") "setw -g window-status-format '${cfg.status.windowFormat}'"}
      ${optionalString (cfg.status.leftFormat != "") "set -g status-left '${cfg.status.leftFormat}'"}
      ${optionalString (cfg.status.rightFormat != "") "set -g status-right '${cfg.status.rightFormat}'"}
      ${optionalString (cfg.status.style != "") "set -g status-style '${cfg.status.style}'"}
      ${optionalString (cfg.status.windowStyle != "") "setw -g window-status-style '${cfg.status.windowStyle}'"}
      ${optionalString (cfg.status.currentWindowStyle != "")
      "setw -g window-status-current-style '${cfg.status.currentWindowStyle}'"}
      ${optionalString (cfg.status.messageStyle != "") "setw -g message-style '${cfg.status.messageStyle}'"}
    '' else ''
      set -g status off
    ''}

    ${optionalString (cfg.hooks != [ ]) builtins.concatStringsSep "\n\n"
    (lib.mapAttrsToList (key: value: "set-hook -g ${key} ${value}") cfg.hooks)}

    ${optionalString (cfg.bindings != null) ''
      ${optionalString (cfg.bindings.copyMode != { }) (builtins.concatStringsSep "\n" (lib.mapAttrsToList
        (key: command: ''
          bind -T ${if cfg.keyMode == "emacs" then "copy-mode" else "copy-mode-vi"} ${key} ${command}
        '') cfg.bindings.copyMode))}
      ${optionalString (cfg.bindings.joinPaneMode != { }) (builtins.concatStringsSep "\n" (lib.mapAttrsToList
        (key: command: ''
          bind -T join-pane ${key} ${command}
        '') cfg.bindings.joinPaneMode))}
      ${optionalString (cfg.bindings.prefixed != { }) (builtins.concatStringsSep "\n\n" (lib.mapAttrsToList
        (key: command: ''
          bind -T prefix ${key} ${command}
        '') cfg.bindings.prefixed))}
      ${optionalString (cfg.bindings.root != { }) (builtins.concatStringsSep "\n\n" (lib.mapAttrsToList
        (key: command: ''
          bind -T root ${key} ${command}
        '') cfg.bindings.root))}

    ''}

    ${cfg.extraConfig}
  '';

in {
  options = {
    custom.programs.tmux = {
      aggressiveResize = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Resize the window to the size of the smallest session for
          which it is the current window.
        '';
      };

      baseIndex = mkOption {
        default = 0;
        example = 1;
        type = types.ints.unsigned;
        description = "Base index for windows and panes.";
      };

      bindings = mkOption {
        default = null;
        type = types.nullOr bindingsModule;
        description = "Custom bindings.";
      };

      borderStyle.active = mkOption {
        default = "";
        type = types.str;
        description = ''
          Active pane border style.
        '';
      };

      borderStyle.inactive = mkOption {
        default = "";
        type = types.str;
        description = ''
          Inactive panes border style.
        '';
      };

      clock24 = mkOption {
        default = false;
        type = types.bool;
        description = "Use 24 hour clock.";
      };

      customPaneNavigationAndResize = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Override the hjkl and HJKL bindings for pane navigation and
          resizing in VI mode.
        '';
      };

      disableConfirmationPrompt = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Disable confirmation prompt before killing a pane or window
        '';
      };

      enable = mkEnableOption "tmux";

      escapeTime = mkOption {
        default = 500;
        example = 0;
        type = types.ints.unsigned;
        description = ''
          Time in milliseconds for which tmux waits after an escape is
          input.
        '';
      };

      extraConfig = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Additional configuration to add to
          <filename>tmux.conf</filename>.
        '';
      };

      historyLimit = mkOption {
        default = 2000;
        example = 5000;
        type = types.ints.positive;
        description = "Maximum number of lines held in window history.";
      };

      hooks = mkOption {
        default = { };
        example = literalExample ''
          {
            "after-select-pane" = "<command>";
          }
        '';
        type = types.attrsOf types.str;
        description = ''
          Various hooks.
        '';
      };

      keyMode = mkOption {
        default = defaultKeyMode;
        example = "vi";
        type = types.enum [ "emacs" "vi" ];
        description = "VI or Emacs style shortcuts.";
      };

      nestedShortcut = mkOption {
        default = defaultShortcut;
        example = "M-a";
        type = types.str;
        description = ''
          This keybinding is used as the nested session shortcut.
        '';
      };

      newSession = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Automatically spawn a session if trying to attach and none
          are running.
        '';
      };

      package = mkOption {
        type = types.package;
        default = pkgs.tmux;
        defaultText = literalExample "pkgs.tmux";
        example = literalExample "pkgs.tmux";
        description = "The tmux package to install";
      };

      reverseSplit = mkOption {
        default = false;
        type = types.bool;
        description = "Reverse the window split shortcuts.";
      };

      resizeAmount = mkOption {
        default = defaultResize;
        example = 10;
        type = types.ints.positive;
        description = "Number of lines/columns when resizing.";
      };

      sensibleOnTop = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Run the sensible plugin at the top of the configuration. It
          is possible to override the sensible settings using the
          <option>programs.tmux.extraConfig</option> option.
        '';
      };

      shortcut = mkOption {
        default = defaultShortcut;
        example = "C-a";
        type = types.str;
        description = ''
          This keybinding is used as the main shortcut.
        '';
      };

      terminal = mkOption {
        default = "screen-256color";
        example = "screen-256color";
        type = types.str;
        description = "Set the $TERM variable.";
      };

      secureSocket = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Store tmux socket under <filename>/run</filename>, which is more
          secure than <filename>/tmp</filename>, but as a downside it doesn't
          survive user logout.
        '';
      };

      shell = mkOption {
        default = defaultShell;
        example = "zsh";
        type = types.str;
        description = "Default shell for new panes.";
      };

      status = mkOption {
        default = null;
        type = types.nullOr statusModule;
        description = "Statusbar settings.";
      };

      tmuxp.enable = mkEnableOption "tmuxp";

      tmuxinator.enable = mkEnableOption "tmuxinator";

      plugins = mkOption {
        type = with types;
          listOf (either package pluginModule) // {
            description = "list of plugin packages or submodules";
          };
        description = ''
          List of tmux plugins to be included at the end of your tmux
          configuration. The sensible plugin, however, is defaulted to
          run at the top of your configuration.
        '';
        default = [ ];
        example = literalExample ''
          with pkgs; [
            tmuxPlugins.cpu
            {
              plugin = tmuxPlugins.resurrect;
              extraConfig = "set -g @resurrect-strategy-nvim 'session'";
            }
            {
              plugin = tmuxPlugins.continuum;
              extraConfig = '''
                set -g @continuum-restore 'on'
                set -g @continuum-save-interval '60' # minutes
              ''';
            }
          ]
        '';
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home-manager.users."${user}" = {
        home.packages = [ cfg.package ] ++ optional cfg.tmuxinator.enable pkgs.tmuxinator
          ++ optional cfg.tmuxp.enable pkgs.tmuxp;
        home.file.".tmux.conf".text = tmuxConf;
      };
    }

    (mkIf cfg.sensibleOnTop {
      home-manager.users."${user}".home.file.".tmux.conf".text = mkBefore ''
        # ============================================= #
        # Start with defaults from the Sensible plugin  #
        # --------------------------------------------- #
        run-shell ${pkgs.tmuxPlugins.sensible.rtp}
        # ============================================= #
      '';
    })

    (mkIf cfg.secureSocket {
      home-manager.users."${user}".home.sessionVariables = {
        TMUX_TMPDIR = ''''${XDG_RUNTIME_DIR:-"/run/user/\$(id -u)"}'';
      };
    })

    (mkIf (cfg.plugins != [ ]) {
      assertions = [
        (let
          hasBadPluginName = p: !(hasPrefix "tmuxplugin" (pluginName p));
          badPlugins = filter hasBadPluginName cfg.plugins;
        in {
          assertion = badPlugins == [ ];
          message = ''Invalid tmux plugin (not prefixed with "tmuxplugins"): ''
            + concatMapStringsSep ", " pluginName badPlugins;
        })
      ];

      home-manager.users."${user}".home.file.".tmux.conf".text = mkAfter ''
        # ============================================= #
        # Load plugins with Home Manager                #
        # --------------------------------------------- #

        ${(concatMapStringsSep "\n" (p: ''
          ${p.extraConfig or ""}
          run-shell ${if types.package.check p then p.rtp else p.plugin.rtp}
        '') cfg.plugins)}
        # ============================================= #
      '';
    })
  ]);
}
