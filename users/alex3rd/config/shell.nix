{ config, pkgs, lib, ... }:
with import ../const.nix { inherit config pkgs; };
with import ../secrets/const.nix { inherit config pkgs lib; };
let
  custom = import ../../../pkgs/custom pkgs config;
  zshOptions = [ "braceccl" "correctall" "extendedglob" "menucomplete" ];
  zshHistFilename = ".histfile";
in {
  imports = [ ../secrets/job.nix ];

  home-manager.users."${userName}" = {
    home.packages = with pkgs; [ libnotify ]; # for zsh-notify plugin
    home.file = {
      "tmuxp/main.yml".text = ''
        session_name: main
        windows:
          - window_name: repl
            panes:
              - nix repl '<nixpkgs/nixos>'
          - window_name: files
            panes:
              - mc
      '';
      "tmuxp/media.yml".text = ''
        session_name: media
        windows:
          - window_name: youtube
            panes:
              - mpsyt
      '';
      "tmuxp/dev.yml".text = ''
        session_name: dev
        windows:
          - window_name: mc
            start_directory: ${devWorkspacePath}/github.com/wiedzmin
            panes:
              - mc
      '';
    };
    programs.htop = {
      enable = true;
      fields = [ "USER" "PRIORITY" "NICE" "M_SIZE" "STATE" "PERCENT_CPU" "PERCENT_MEM" "TIME" "COMM" ];
      meters.left = [ "AllCPUs" "Memory" ];
      colorScheme = 0;
      detailedCpuTime = true;
    };
    programs.command-not-found.enable = true;
    programs.lesspipe.enable = true;
    programs.man.enable = true;
    programs.info.enable = true;
    programs.skim = {
      enable = true;
      historyWidgetOptions = [ "--exact" ];
      defaultOptions = [ "--height 40%" "--prompt ⟫" ];
      fileWidgetCommand = "${pkgs.fd}/bin/fd --type f";
      fileWidgetOptions = [ "--preview 'head {}'" ];
      changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
      changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
      enableZshIntegration = true;
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.z-lua = {
      enable = true;
      enableZshIntegration = true;
      options = [ "fzf" "enhanced" "once" ];
    };
    programs.tmux = {
      enable = true;
      baseIndex = 1;
      clock24 = true;
      escapeTime = 0;
      status = {
        currentWindowFormat = "#[bg=blue,fg=cyan,bold]#I#[bg=blue,fg=cyan]:#[fg=colour230]#T#[fg=dim]#F";
        windowFormat = "#[fg=cyan,dim]#I#[fg=blue]:#[default]#W#[fg=grey,dim]#F";
        leftFormat = "#{prefix_highlight}#[fg=green](#S) #(whoami)@#H";
        rightFormat = "#[fg=blue,bright]%k:%M:%S %d/%m/%Y | #{cpu_fg_color}#{cpu_icon}#{cpu_percentage}";
        style = "fg=white,bg=default,default";
        windowStyle = "fg=cyan,bg=default,dim";
        currentWindowStyle = "fg=colour166,bg=red,bright";
        messageStyle = "fg=white,bg=black,bright";
      };
      borderStyle = {
        active = "fg=colour240,bg=default";
        inactive = "fg=colour235,bg=default";
      };
      hooks = {
        "after-select-pane" =
          "run-shell \\\"tmux set -g window-active-style \"bg='brightblack'\" && sleep .05 && tmux set -g window-active-style ''\\\"";
      };
      bindings = {
        copyMode = {
          "M-e" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture es"'';
          "M-j" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture js"'';
          "M-n" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture ns"'';
          "M-x" = ''run-shell "${custom.shell-org-capture}/bin/shell-org-capture xs"'';
        };
        root = {
          "C-left" = "prev";
          "C-right" = "next";
          "S-left" = "swap-window -t -1";
          "S-right" = "swap-window -t +1";
          "C-y" = ''
            run -b "exec </dev/null; ${pkgs.xsel}/bin/xsel -o --clipboard | tmux load-buffer - ; \
                                                tmux paste-buffer"'';
        };
        prefixed = {
          "*" = "list-clients";
          "l" = "refresh-client";
          "m" = "select-pane -m";
          "|" = ''split-window -h -c "#{pane_current_path}"'';
          "\\" = ''split-window -fh -c "#{pane_current_path}"'';
          "-" = ''split-window -v -c "#{pane_current_path}"'';
          "_" = ''split-window -fv -c "#{pane_current_path}"'';
          "'#'" = ''split-window -h -c "#{pane_current_path}"'';
          "@" = ''split-window -v -c "#{pane_current_path}"'';
          "BSpace" = "last-window";
          "r" = ''source-file ~/.tmux.conf \; display "  Config reloaded..."'';
          "y" = "set-window-option synchronize-panes";
          "T" = ''neww -n "Tmux manual" "exec man tmux"'';
          "s" = ''
            split-window -v "tmux list-sessions | sed -E 's/:.*$//' | \
                                                    grep -v \"^$(tmux display-message -p '#S')\$\" | \
                                                    ${pkgs.skim}/bin/sk --reverse | xargs tmux switch-client -t"'';
          "F12" = ''
            send-key "#############################################################################################"'';
          "F11" = ''new-window "fq; $SHELL"'';
          "S-F11" = ''
            run -b "exec </dev/null; ${pkgs.xsel}/bin/xsel -o --clipboard | \
                                                                ${pkgs.xe}/bin/xe ${pkgs.nq}/bin/nq ${pkgs.you-get}/bin/you-get"'';
          "b" = ''
            split-window -c '#{pane_current_path}' \
                                                -v "${pkgs.git}/bin/git rev-parse --git-dir 2> /dev/null && ${pkgs.git}/bin/git -p show --color=always \
                                                    $(${pkgs.git}/bin/git log --decorate=short --graph --oneline --color=always | \
                                                    ${pkgs.skim}/bin/sk --ansi -m | ${pkgs.gawk}/bin/awk '{print $2}') | less -R"'';
        };
      };
      extraConfig = ''
        set -g renumber-windows on

        set -g bell-action any
        set -g visual-activity off
        set -g visual-bell off
        set -g visual-silence off
        setw -g monitor-activity on
      '';
      historyLimit = 102400;
      keyMode = "emacs";
      nestedShortcut = "C-x";
      sensibleOnTop = false;
      shortcut = "M-x";
      terminal = "screen-256color";
      secureSocket = false;
      shell = "${pkgs.zsh}/bin/zsh";
      tmuxp.enable = true;
      plugins = with pkgs;
        with tmuxPlugins; [
          {
            plugin = fzf-tmux-url-with-history; # patched version, see overlays
            extraConfig = "set -g @fzf-url-bind 'o'";
          }
          battery
          copycat
          cpu
          fpp
          logging
          open # TODO: setup and verify working
          prefix-highlight
          sessionist
          yank
        ];
    };
    programs.alacritty = {
      enable = true;
      settings = {
        env = { TERM = "xterm-256color"; };
        window = {
          padding = {
            x = 2;
            y = 2;
          };
          decorations = "full";
        };
        tabspaces = 8;
        draw_bold_text_with_bright_colors = true;
        font = {
          normal = {
            family = "${fontTermName}";
            style = "${fontTermWeight}";
          };
          bold = {
            family = "${fontTermName}";
            style = "${fontTermWeight}";
          };
          italic = {
            family = "${fontTermName}";
            style = "Italic";
          };
          size = fontSizeAlacritty;
        };
        visual_bell = {
          animation = "EaseOutExpo";
          duration = 1;
        };
        mouse_bindings = [{
          mouse = "Middle";
          action = "PasteSelection";
        }];
        selection = { semantic_escape_chars = '',│`|:"' ()[]{}<>''; };
        dynamic_title = true;
        cursor = { style = "Beam"; };
        live_config_reload = true;
      };
    };
    programs.bat = {
      enable = true;
      config = {
        theme = "TwoDark";
        pager = "less -FR";
      };
    };
    programs.jq = {
      enable = true;
      colors = {
        null = "1;30";
        false = "0;91";
        true = "0;92";
        numbers = "0;36";
        strings = "1;96";
        arrays = "1;94";
        objects = "1;33";
      };
    };
    programs.lsd = {
      enable = true;
      enableAliases = true;
    };
    programs.zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = [ "colored-man-pages" "urltools" ];
        theme = "muse";
      };
      enableAutosuggestions = true;
      enableCompletion = true;
      history = {
        size = 10000;
        save = 10000;
        path = "${zshHistFilename}";
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
        '') zshOptions}

        bindkey '^P' fuzzy-search-and-edit
      '';
      sessionVariables = {
        CSEARCHINDEX = "${devWorkspacePath}/.csearchindex";
        EDITOR = "${pkgs.emacs}/bin/emacsclient";
        HISTFILE = "${zshHistFilename}";
        TMUXP_CONFIGDIR = "/home/${userName}/tmuxp";
        VISUAL = "${pkgs.emacs}/bin/emacsclient";
        WORKON_HOME = "/home/${userName}/.virtualenvs";
        YSU_IGNORED_ALIASES = [ "g" "ll" ]; # TODO: review list
        YSU_MODE = "ALL";
        ZSH_COMMAND_TIME_COLOR = "cyan";
      };
      shellAliases = {
        cat = "${pkgs.bat}/bin/bat"; # use --plain in case of emergency

        df = "${pkgs.dfc}/bin/dfc";
        du = "${pkgs.duc}/bin/duc";

        yg = "${pkgs.you-get}/bin/you-get";

        zz =
          "cd $(z -i | ${pkgs.skim}/bin/sk --nth 2 --reverse --inline-info --tac | ${pkgs.gawk}/bin/awk '{print $2}')";
        zb = "z -b";

        zr = ". ~/.zshrc";
      };
      plugins = [
        {
          name = "zsh-notify";
          file = "notify.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "marzocchi";
            repo = "zsh-notify";
            rev = "853bc9434771b99b028f069b95e13ecdf06901d0";
            sha256 = "0bhmv1xfjzmci9b4dy3mix2s31zj0kayrl44xx5xb8rgzlf0qbvr";
          };
        }
        {
          # TODO: try to integrate with fzf-based/skim utils, expecially commit browser
          name = "browse-commit";
          file = "browse-commit.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "wiedzmin";
            repo = "browse-commit";
            rev = "cf28b2eeba622545ae751ec99532b6b60e58b845";
            sha256 = "15c9qxxa7l47w5r28pazs0gv0016lv52mncn45s6g1d3120k5fx0";
          };
        }
        {
          name = "you-should-use";
          file = "you-should-use.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "MichaelAquilina";
            repo = "zsh-you-should-use";
            rev = "1.1.0";
            sha256 = "0fig5ralagi5jajk7gdm52jvwql17qk9cd6j98qsndvckb26a753";
          };
        }
        {
          name = "pass-zsh-completion";
          file = "pass-zsh-completion.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "ninrod";
            repo = "pass-zsh-completion";
            rev = "e4d8d2c27d8999307e8f34bf81b2e15df4b76177";
            sha256 = "1z83hgdljl7yqd1lqb10an8zkrv7s01khky27mgc1wargkslkxi9";
          };
        }
        {
          name = "zsh-async";
          file = "async.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "mafredri";
            repo = "zsh-async";
            rev = "e6d937228729f934f2033039bb54c3a18f5f1358";
            sha256 = "0f0bqm4245ghx31x30ircfp4njji834495g25wvrd93k2r96a669";
          };
        }
        {
          name = "git-extra-commands";
          file = "git-extra-commands.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "unixorn";
            repo = "git-extra-commands";
            rev = "f03ff8ffce9f3e488b6a0265cb09288cc29899fe";
            sha256 = "1qlbjn0q87jgjir3k7w4m8p6wqgjl2c7jnilczf7c205fgwksdhi";
          };
        }
        {
          name = "zsh-reentry-hook";
          file = "zsh-reentry-hook.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "RobSis";
            repo = "zsh-reentry-hook";
            rev = "8587186df8f08b8a57ae7f87ab0bc7d503909031";
            sha256 = "1jgin1gmw05vxf7vw414zvhq9dg06yzlzxas723f710vs58mf11a";
          };
        }
        {
          name = "zsh-fuzzy-search-and-edit";
          file = "plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "seletskiy";
            repo = "zsh-fuzzy-search-and-edit";
            rev = "4fbb3d351b75f1007df0d5cb09292bb2321f903a";
            sha256 = "1shhmda1iqwz79y2ianmjs5623zabckxfj2hqw4gl2axpkwnj1ib";
          };
        }
        {
          name = "zsh-command-time";
          file = "command-time.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "popstas";
            repo = "zsh-command-time";
            rev = "afb4a4c9ae7ce64ca9d4f334a79a25e46daad0aa";
            sha256 = "1bvyjgz6bhgg1nwr56r50p6fblgah6yiql55pgm5abnn2h876fjq";
          };
        }
        {
          # NOTE: should be last in the list
          name = "zsh-syntax-highlighting";
          file = "zsh-syntax-highlighting.plugin.zsh";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-syntax-highlighting";
            rev = "e900ad8bad53501689afcb050456400d7a8466e5";
            sha256 = "1dfy5wvkmnp2zzk81fhc7qlywgn0j6z0vjch5ak5r3j2kqv61cmi";
          };
        }
      ];
    };
  };
}

# TODO: some impl/binding for ix.io posts
