{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.ext.nix.core;
  user = config.attributes.mainUser.name;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    ext.nix.core = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable core nix setup";
      };
      gc.dates = mkOption {
        type = types.str;
        default = "weekly";
        description = "How often Nix GC should be run.";
      };
      gc.howold = mkOption {
        type = types.str;
        default = "7d";
        description = "How old store content should be to become collected by Nix GC.";
      };
      permittedInsecurePackages = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Insecure packages exceptions list";
      };
      shell.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell completions";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Nix-related Emacs setup";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Nix-related bookmarks";
      };
      # TODO: consider adding system-wide option for enabling heavy resources consumers and AND it here and there
      lsp.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable LSP functionality";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      boot.readOnlyNixStore = true; # TODO: should it be moved to host/machines level?
      nix = {
        settings = {
          cores = lib.mkDefault config.attributes.hardware.cores;
          max-jobs = lib.mkDefault config.attributes.nix.jobs;
          require-sigs = true;
          sandbox = true;
          substituters = [ "https://cache.nixos.org" ];
          trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
          trusted-users = [ "root" user ];
        };
        package = pkgs.nixUnstable;
        extraOptions = ''
          auto-optimise-store = true
          keep-outputs = true
          keep-derivations = true
          http-connections = 10
          experimental-features = nix-command flakes
        '';
        registry = {
          config.flake = inputs.self;
          emacs.flake = inputs.emacs;
          unstable.flake = inputs.unstable;
        };

        optimise.automatic = false;
        gc = {
          automatic = true;
          inherit (cfg.gc) dates;
          options = "--delete-older-than ${cfg.gc.howold}";
        };
      };

      environment.etc.nixpkgs.source = inputs.unstable;
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        inherit (cfg) permittedInsecurePackages;

        packageOverrides = _: rec {
          # TODO: try https://github.com/lf-/nix-doc
          rollback = pkgs.writeShellApplication {
            name = "rollback";
            runtimeInputs = with pkgs; [ fzf ];
            text = ''
              GENERATION=$(pkexec nix-env -p /nix/var/nix/profiles/system --list-generations | fzf --tac)
              if [ -n "$GENERATION" ]; then
                GENERATION_PATH=/nix/var/nix/profiles/system-$(echo "$GENERATION" | cut -d\  -f1)-link
                pkexec nix-env --profile /nix/var/nix/profiles/system --set "$GENERATION_PATH" && pkexec "$GENERATION_PATH/bin/switch-to-configuration switch"
              fi
            '';
          };
          nix-doc-lookup = pkgs.writeShellApplication {
            name = "nix-doc-lookup";
            runtimeInputs = with pkgs; [ fzf gnused manix ];
            text = ''
              manix "" | grep '^# ' | sed 's/^# \(.*\) (.*/\1/;s/ (.*//;s/^# //' | fzf --preview="manix '{}'" | xargs manix
            '';
          };
          # FIXME: parameterize "nixpkgs#" below
          nix-build-offline = pkgs.writeShellApplication {
            name = "nix-build-offline";
            text = ''
              nix eval nixpkgs#"$1".drvPath --raw \
                | xargs nix-store -qR \
                | grep '\.drv$' \
                | xargs -n1 nix show-derivation \
                | jq -s '.[] | select(.[] | .env | has("outputHash")) | keys | .[]' -r \
                | xargs nix build --no-link --print-out-paths
            '';
          };
        };
      };

      programs.nix-ld.enable = false; # enable on-demand, see https://github.com/Mic92/nix-ld for reference

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ cargo /*for unpackaged Rust tools*/ nix-doc-lookup nix-build-offline rollback statix ] ++
          lib.optionals cfg.lsp.enable [ nil ];
        home.sessionPath = [ (homePrefix user ".local/share/cargo/bin") ]; # FIXME: use XDG_DATA_HOME
      };
      shell.core.variables = [{
        CARGO_HOME = "$XDG_DATA_HOME/cargo";
        global = true;
      }];

      systemd.services.nix-daemon = {
        environment.TMPDIR = "/tmp/buildroot";
        preStart = ''
          mkdir -p /tmp/buildroot
        '';
      };

      dev.misc.timeTracking.extensions.dev = { "nix" = "coding:nix"; };
      pim.timetracking.rules = mkArbttProgramTitleRule [ "emacs" ] [ "/nixos-config/" ] "project:nixos-config";
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/nix-core.yml".source = yaml.generate "espanso-nix-core.yml"
          {
            matches = [
              {
                trigger = ":npg";
                replace = "nix shell \"nixpkgs#nix-prefetch-github\" -c nix-prefetch-git --rev {{revision.value}} {{repolink}}";
                vars = [
                  {
                    name = "revision";
                    type = "form";
                    params = { layout = "Prefetch revision: [[value]]"; };
                  }
                  {
                    name = "repolink";
                    type = "clipboard";
                  }
                ];
              }
              {
                trigger = ":npnew";
                replace = "nix shell \"nixpkgs#git\" git log --pretty=oneline ORIG_HEAD..FETCH_HEAD | grep init | grep -v Merge";
              }
              {
                trigger = ":nscptree";
                replace = "nix-store -q --tree ~/.nix-profile";
              }
              {
                trigger = ":ngcr";
                replace = "nix-store --gc --print-roots | cut -d' ' -f1 | uniq | grep -v /proc | grep -v { | grep -v /run | grep ${user} | grep direnv";
              }
              {
                trigger = ":npvim";
                replace = "nix shell \"nixpkgs#vim\" -c vim ";
              }
              {
                trigger = ":nsp";
                replace = "nix shell \"nixpkgs#$|$\"";
              }
              {
                trigger = ":ns2";
                replace = " \"nixpkgs#$|$\"";
              }
              {
                trigger = ":pkgs";
                replace = "inputs.unstable.legacyPackages.x86_64-linux.$|$";
              }
              {
                trigger = ":cfg";
                replace = "nixosConfigurations.laptoptop.config.{{machine.value}}";
                vars = [
                  {
                    name = "machines";
                    type = "shell";
                    params = { cmd = "ls ${configPrefix roots "machines"}"; };
                  }
                  {
                    name = "machine";
                    type = "choice";
                    params = { values = "{{machines}}"; };
                  }
                ];
              }
              {
                trigger = ":elt";
                replace = "builtins.head (inputs.unstable.lib.sublist 1$|$ 1 nixosConfigurations.laptoptop.config.wmCommon.wsMapping.rules)";
              }
              {
                trigger = ":nrep";
                replace = "cd ${wsRoot roots "github"}/wiedzmin/nixos-config && nix repl --file ./flake-repl.nix";
              }
              {
                trigger = ":nsfd";
                replace = "nix shell \"nixpkgs#fd\" -c fd $|$ /nix/store";
              }
              {
                trigger = ":llv";
                replace = "inputs.unstable.legacyPackages.x86_64-linux.linuxPackages.";
              }
              {
                trigger = ":nwda";
                replace = "nix why-depends --all \"nixpkgs#$|$\" \"nixpkgs#\"";
              }
              {
                trigger = ":nwdo";
                replace = "nix why-depends \"nixpkgs#$|$\" \"nixpkgs#\"";
              }
              {
                trigger = ":nwdd";
                replace = "nix why-depends --derivation \"nixpkgs#$|$\" \"nixpkgs#\"";
              }
              {
                trigger = ":nwd?";
                replace = "nix why-depends --help";
              }
              {
                trigger = ":nwtt";
                replace = "nix shell \"nixpkgs#tree\" -c tree /nix/var/nix/{gcroots,profiles}";
              }
            ];
          } // optionalAttrs (config.shell.tmux.enable) {
          filter_title = "\".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*\"";
        };
      };
    })
    (mkIf (cfg.enable && cfg.shell.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ nix-zsh-completions ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.company-nixos-options epkgs.nix-mode ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./elisp/nix.el ];
      completion.emacs.tempel.snippets = ''
        org-mode

        (nixsrc "#+begin_src nix" n> r> n "#+end_src" :post (org-edit-src-code))
      '';
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        home-manager = {
          desc = "home-manager upstream repo";
          local.path = "${wsRoot roots "github"}/rycee/home-manager";
          remote = {
            url = "https://github.com/rycee/home-manager/";
            jump = true;
            searchSuffix = "search?q=";
          };
        };
        nixpkgs = {
          desc = "Nixpkgs upstream repo";
          local.path = "${wsRoot roots "github"}/NixOS/nixpkgs";
          remote = {
            url = "https://github.com/NixOS/nixpkgs/";
            jump = true;
            searchSuffix = "search?q=";
          };
        };
        nix-versions = {
          desc = "Mapping packages version to Nixpkgs repo git history";
          remote = {
            url = "https://lazamar.co.uk/nix-versions/";
            jump = true;
            searchSuffix = "?channel=nixos-unstable&package=";
          };
        };
        nix-versions-blogpost = {
          desc = "Descriptive blog post for `nix-versions` (see alongside)";
          remote.url = "https://lazamar.github.io/download-specific-package-version-with-nix/";
        };
        nixos-hardware = {
          desc = "NixOS hardware presets";
          local.path = "${wsRoot roots "github"}/NixOS/nixos-hardware";
          remote = {
            url = "https://github.com/NixOS/nixos-hardware";
          };
        };
        "nixospkg" = {
          desc = "NixOS packages";
          remote.url = "https://nixos.org/nixos/packages.html";
        };
        "nixos-status" = {
          desc = "NixOS status page";
          remote.url = "https://status.nixos.org/";
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "status nixos";
              desktop = "web";
            }
          ];
        };
        "nix" = {
          desc = "it/nix";
          tags = [ "media" "video" ];
          remote = {
            url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZiPdYVvHZY8Day5RC934CE";
            browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
          };
        };
        "nixos-news" = {
          desc = "NixOS weekly news";
          remote.url = "https://weekly.nixos.org/";
        };
        "nixos/packages" = {
          desc = "Nixpkgs/unstable";
          remote = {
            url = "https://nixos.org/nixos/packages/";
            searchSuffix = "?channel=nixpkgs-unstable&query=";
          };
        };
        "nix/pm/repo" = mkGithubBookmark "NixOS" "nix" roots // { desc = "nix package manager repo"; };
        "ghnix" = {
          desc = "github/lang:nix";
          tags = [ "forge" ];
          remote = {
            url = "https://github.com/";
            jump = false;
            searchSuffix = "search?q=language%3Anix+";
          };
        };
        "nixosr" = {
          desc = "NixOS + ";
          remote = {
            url = "";
            searchSuffix = "https://www.google.ru/?q=nixos+";
          };
        };
        "nixosopt" = {
          desc = "NixOS/options";
          remote = {
            url = "https://nixos.org/nixos/options.html#";
            searchSuffix = "";
          };
        };
        "nixhydra" = {
          desc = "Nixpkgs from Hydra";
          remote = {
            url = "https://hydra.nixos.org/";
            searchSuffix = "search?query=";
          };
        };
        "discourse/nixos" = {
          desc = "NixOS Discourse";
          remote.url = "https://discourse.nixos.org/";
        };
        "HM/documentation" = {
          desc = "Home Manager documentation";
          remote.url = "https://nix-community.github.io/home-manager/index.html";
        };
        "nix-lib" = {
          desc = "Nix library reference";
          remote.url = "https://teu5us.github.io/nix-lib.html";
        };
        "mynixos" = {
          desc = "Build and share reproducible software environments with Nix and NixOS";
          remote.url = "https://mynixos.com";
        };
        "nix.dev" = {
          desc = "An opinionated guide for developers getting things done using the Nix ecosystem.";
          remote.url = "https://nix.dev";
        };
      };
    })
  ];
}
