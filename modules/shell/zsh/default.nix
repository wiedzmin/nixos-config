{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.zsh;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  inherit (hm.xdg) dataHome;
in
{
  options = {
    shell.zsh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell tooling.";
      };
      setopt = mkOption {
        type = types.listOf types.str;
        description = "List of options to be set by `setopt`";
        default = [
          "APPEND_HISTORY"
          "AUTO_CD"
          "BRACE_CCL"
          "EXTENDED_GLOB"
          "HIST_FIND_NO_DUPS"
          "HIST_IGNORE_ALL_DUPS"
          "HIST_IGNORE_SPACE"
          "HIST_NO_STORE"
          "HIST_SAVE_NO_DUPS"
          "INC_APPEND_HISTORY"
          "MENU_COMPLETE"
          "braceccl"
          "extendedglob"
          "menucomplete"
        ];
      };
      initExtraPrimary = mkOption {
        type = types.lines;
        description = "initExtra entries to be processed before any others";
        default = "";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.shells = with pkgs; [ "${zsh}/bin/zsh" "/run/current-system/sw/bin/zsh" ];
      home-manager.users."${user}" = {
        # NOTE: play with ydotool client/server arch and respective permissions
        home.packages = with pkgs; [ gdu rtss wmctrl xdotool rargs ydotool ];
        xdg.configFile."espanso/user/zsh.yml".text = ''
          name: zsh
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":ts"
              replace: "$|$ | rtss"

            - trigger: ":rlw"
              replace: "readlink -f `which $|$`"
        '';
        programs.zsh = {
          enable = true;
          enableAutosuggestions = true;
          enableCompletion = true;
          history = {
            size = 10000;
            save = 10000;
            path = "${dataHome}/.histfile";
            ignoreDups = true;
            expireDuplicatesFirst = true;
            extended = true;
            share = true;
          };
          initExtra = ''
            # This script was automatically generated by the broot program
            # More information can be found in https://github.com/Canop/broot
            # This function starts broot and executes the command
            # it produces, if any.
            # It's needed because some shell commands, like `cd`,
            # have no useful effect if executed in a subshell.
            function br {
                f=$(mktemp)
                (
                    set +e
                    broot --outcmd "$f" "$@"
                    code=$?
                    if [ "$code" != 0 ]; then
                        rm -f "$f"
                        exit "$code"
                    fi
                )
                code=$?
                if [ "$code" != 0 ]; then
                    return "$code"
                fi
                d=$(<"$f")
                rm -f "$f"
                eval "$d"
            }

            ${lib.concatMapStrings (opt: ''
              setopt ${opt}
            '') cfg.setopt}

            ${cfg.initExtraPrimary}

            ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

            # review https://github.com/Aloxaf/fzf-tab/wiki/Configuration
            source ${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh

            bindkey '^P' fuzzy-search-and-edit

            fpath=(${inputs.zsh-go-task-completions} $fpath)
          '';
          sessionVariables = { # NOTE: zsh-specific, keep, do not bind to custom module(s)
            HISTFILE = "${dataHome}/.histfile";
            LESSHISTFILE = "${dataHome}/.lesshst";
          };
          shellAliases = {
            cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency
            catb = "${pkgs.bat}/bin/bat -A";

            j = "br -s";

            df = "${pkgs.duf}/bin/duf";
            du = "${pkgs.du-dust}/bin/dust";

            zr = ". ~/.zshrc";

            vg = "EDITOR=emacsclient ${pkgs.vgrep}/bin/vgrep --interactive --show c5";
          };
          plugins = [
            {
              name = "zsh-notify";
              file = "notify.plugin.zsh";
              src = inputs.zsh-notify;
            }
            {
              name = "zsh-nix-shell";
              file = "nix-shell.plugin.zsh";
              src = inputs.zsh-nix-shell;
            }
            {
              name = "you-should-use";
              file = "you-should-use.plugin.zsh";
              src = inputs.zsh-you-should-use;
            }
            {
              name = "pass-zsh-completion";
              file = "pass-zsh-completion.plugin.zsh";
              src = inputs.pass-zsh-completion;
            }
            {
              name = "zsh-async";
              file = "async.plugin.zsh";
              src = inputs.zsh-async;
            }
            {
              name = "git-extra-commands";
              file = "git-extra-commands.plugin.zsh";
              src = inputs.git-extra-commands;
            }
            {
              name = "zsh-reentry-hook";
              file = "zsh-reentry-hook.plugin.zsh";
              src = inputs.zsh-reentry-hook;
            }
            {
              name = "zsh-fuzzy-search-and-edit";
              file = "plugin.zsh";
              src = inputs.zsh-fuzzy-search-and-edit;
            }
          ] ++ [{
            # NOTE: should be last in the list
            name = "zsh-syntax-highlighting";
            file = "zsh-syntax-highlighting.plugin.zsh";
            src = inputs.zsh-syntax-highlighting;
          }];
        };
        programs.direnv.enableZshIntegration = true;
        programs.fzf.enableZshIntegration = true;
      };
    })
  ];
}
