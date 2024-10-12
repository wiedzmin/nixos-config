{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.zsh;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  inherit (hm.xdg) dataHome;
  pluginEnabled = plugin:
    builtins.elem plugin (forEach config.home-manager.users."${user}".programs.zsh.plugins (x: x.name));
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

      users.extraUsers."${user}".shell = pkgs.zsh;
      programs.zsh.enable = true;

      home-manager.users."${user}" = {
        programs.zsh = {
          enable = true;
          autosuggestion.enable = true;
          enableCompletion = true;
          syntaxHighlighting.enable = true;
          history = {
            size = 10000;
            save = 10000;
            path = "${dataHome}/.histfile";
            ignoreDups = true;
            ignoreAllDups = true;
            ignoreSpace = true;
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

            bindkey '\e[3~' delete-char # appropriate action for `delete` key

            ${optionalString (pluginEnabled "zsh-fuzzy-search-and-edit") "bindkey '^P' fuzzy-search-and-edit"}
          '' + optionalString (config.shell.vt.kitty.enable) ''
            # input navigation shim, see https://github.com/kovidgoyal/kitty/issues/2748 for details

            bindkey '^[[1;3C' emacs-forward-word
            bindkey '^[[1;3D' emacs-backward-word

            bindkey '\e[H'  beginning-of-line
            bindkey '\e[F'  end-of-line
          '';
          sessionVariables = {
            # NOTE: zsh-specific, keep, do not bind to custom module(s)
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

            # FIXME: parameterize EDITOR setting according to the daemon enablement status
            # FIXME: review setup and applications, do we really need it, or could enhance workflow in some way?
            # TODO: review usage, consult https://github.com/vrothberg/vgrep for details
            vg = "EDITOR=emacsclient ${pkgs.vgrep}/bin/vgrep --interactive --show c5";
          };
          plugins = [
            {
              name = "zsh-nix-shell";
              file = "nix-shell.plugin.zsh";
              src = pkgs.zsh-nix-shell;
            }
            {
              name = "nix-zsh-completions";
              file = "nix-zsh-completions.plugin.zsh";
              src = pkgs.nix-zsh-completions;
            }
            {
              name = "you-should-use";
              file = "you-should-use.plugin.zsh";
              src = pkgs.zsh-you-should-use;
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
          ];
        };
        programs.direnv.enableZshIntegration = true;
        programs.fzf.enableZshIntegration = true;
      };
    })
  ];
}
