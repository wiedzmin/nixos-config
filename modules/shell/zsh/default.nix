{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.shell.zsh;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
in {
  options = {
    shell.zsh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell tooling.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ gdu rtss wmctrl xdotool rargs ];
        programs.zsh = {
          enable = true;
          enableAutosuggestions = true;
          enableCompletion = true;
          oh-my-zsh = { # TODO: extract option(s)
            enable = true;
            plugins = [ "colored-man-pages" "urltools" ];
          };
          history = {
            size = 10000;
            save = 10000;
            path = ".histfile";
            ignoreDups = true;
            expireDuplicatesFirst = true;
            extended = true;
            share = true;
          };
          initExtra = ''
            setopt APPEND_HISTORY
            setopt BRACE_CCL
            setopt HIST_FIND_NO_DUPS
            setopt HIST_IGNORE_ALL_DUPS
            setopt HIST_IGNORE_SPACE
            setopt HIST_NO_STORE
            setopt HIST_SAVE_NO_DUPS
            setopt AUTO_CD
            setopt EXTENDED_GLOB
            setopt INC_APPEND_HISTORY
            setopt MENU_COMPLETE

            ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

            ${lib.concatMapStrings (opt: ''
              setopt ${opt}
            '') [ "braceccl" "extendedglob" "menucomplete" ]}

            bindkey '^P' fuzzy-search-and-edit
          '';
          sessionVariables = let dataHome = hm.xdg.dataHome;
          in {
            HISTFILE = "${dataHome}/.histfile";
            LESSHISTFILE = "${dataHome}/.lesshst";
            YSU_IGNORED_ALIASES = [ "g" "ll" ];
            YSU_MODE = "ALL";
          };
          shellAliases = { # TODO: extract to option
            cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency
            catb = "${pkgs.bat}/bin/bat -A";

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
      };
    })
  ];
}
