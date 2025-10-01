{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.ext.nix.core;
  user = config.attributes.mainUser.name;
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
      treesitter.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable treesitter infrastructure for Nix-related Emacs setup";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Nix-related bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nix = {
        nixPath = lib.mkForce [ "nixpkgs=/etc/nixpkgs" ];
        settings = {
          cores = lib.mkDefault config.attributes.hardware.cores;
          max-jobs = lib.mkDefault config.attributes.nix.jobs;
          require-sigs = true;
          sandbox = true;
          substituters = [ "https://cache.nixos.org" ];
          trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
          trusted-users = [ "root" user ];
        };
        package = pkgs.nixVersions.nix_2_29;
        extraOptions = ''
          auto-optimise-store = true
          keep-outputs = true
          keep-derivations = true
          http-connections = 10
          experimental-features = nix-command flakes
          access-tokens = github.com=${config.ext.networking.secrets.accessTokens."github"}
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

      virtualisation.vmVariant = {
        # following configuration is added only when building VM with build-vm
        virtualisation = {
          memorySize = 4096;
          cores = 2;
        };
      };

      environment.etc.nixpkgs.source = inputs.unstable;
      nixpkgs.config = {
        allowUnfree = true;
        allowUnfreeRedistributable = true;

        oraclejdk.accept_license = true;

        inherit (cfg) permittedInsecurePackages;

        packageOverrides = _: {
          # TODO: try https://github.com/lf-/nix-doc
          rollback = mkShellScriptWithDeps "rollback" (with pkgs; [ fzf ]) ''
            GENERATION=$(pkexec nix-env -p /nix/var/nix/profiles/system --list-generations | fzf --tac)
            if [ -n "$GENERATION" ]; then
              GENERATION_PATH=/nix/var/nix/profiles/system-$(echo $GENERATION | cut -d\  -f1)-link
              pkexec nix-env --profile /nix/var/nix/profiles/system --set $GENERATION_PATH && pkexec $GENERATION_PATH/bin/switch-to-configuration switch
            fi
          '';

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
        home.packages = with pkgs; [
          cargo /*for unpackaged Rust tools*/
          git-crypt /*is needed but not accessible under devenv for some reason*/
          nix-doc-lookup
          nix-build-offline
          nix-init
          nix-melt
          rollback
          statix
          flake-checker
        ];
        home.sessionPath = [ ''''${XDG_DATA_HOME}/cargo/bin'' ];
      };
      shell.core.variables = [{
        CARGO_HOME = ''''${XDG_DATA_HOME}/cargo'';
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
      completion.expansions.espanso.matches = {
        nix-core = {
          matches = [
            {
              trigger = ":npg";
              replace = "nix-prefetch-git --rev refs/heads/{{revision.value}} {{repolink}}";
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
              replace = "git log --pretty=oneline ORIG_HEAD..FETCH_HEAD | grep init | grep -v Merge";
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
              replace = "fd $|$ /nix/store";
            }
            {
              trigger = ":llv";
              replace = "inputs.unstable.legacyPackages.x86_64-linux.linuxPackages.";
            }
            {
              trigger = ":slv";
              replace = "grep -e '^  linuxPackages' /etc/nixpkgs/pkgs/top-level/all-packages.nix";
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
              replace = "tree /nix/var/nix/{gcroots,profiles}";
            }
            {
              trigger = ":nnd";
              replace = "find . -name \"*.nix\" -exec nil diagnostics {} \\;";
            }
            {
              trigger = ":nvdln";
              replace = "nvd --color always list /run/booted-system /run/current-system";
            }
            {
              trigger = ":nvdlcu";
              replace = "nvd --color always list {{profile1.value}} {{profile2.value}}";
              vars = [
                {
                  name = "profiles";
                  type = "shell";
                  params = { cmd = "find /nix/var/nix/profiles/per-user/${user} -name '*link' | grep -v channels"; };
                }
                {
                  name = "profile1";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
                {
                  name = "profile2";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
              ];
            }
            {
              trigger = ":nvdlcs";
              replace = "nvd --color always list {{profile1.value}} {{profile2.value}}";
              vars = [
                {
                  name = "profiles";
                  type = "shell";
                  params = { cmd = "find /nix/var/nix/profiles/ -maxdepth 1 -name '*link'"; };
                }
                {
                  name = "profile1";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
                {
                  name = "profile2";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
              ];
            }
            {
              trigger = ":nvddn";
              replace = "nvd --color always diff /run/booted-system /run/current-system";
            }
            {
              trigger = ":nvddcu";
              replace = "nvd --color always diff {{profile1.value}} {{profile2.value}}";
              vars = [
                {
                  name = "profiles";
                  type = "shell";
                  params = { cmd = "find /nix/var/nix/profiles/per-user/${user} -name '*link' | grep -v channels"; };
                }
                {
                  name = "profile1";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
                {
                  name = "profile2";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
              ];
            }
            {
              trigger = ":nvddcs";
              replace = "nvd --color always diff {{profile1.value}} {{profile2.value}}";
              vars = [
                {
                  name = "profiles";
                  type = "shell";
                  params = { cmd = "find /nix/var/nix/profiles/ -maxdepth 1 -name '*link'"; };
                }
                {
                  name = "profile1";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
                {
                  name = "profile2";
                  type = "choice";
                  params = { values = "{{profiles}}"; };
                }
              ];
            }
          ];
        };
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          # NOTE: expansions deps
          fd
          findutils
          git
          nil
          nix-prefetch-github
          nvd
          tree
        ];
      };
    })
    (mkIf (cfg.enable && cfg.shell.enable) {
      home-manager.users."${user}" = { home.packages = with pkgs; [ nix-zsh-completions ]; };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: optionals (config.ide.emacs.core.treesitter.enable && cfg.treesitter.enable) [ epkgs.nix-ts-mode ]
        ++ optionals (!config.ide.emacs.core.treesitter.enable || !cfg.treesitter.enable) [ epkgs.nix-mode ];
      ide.emacs.core.config =
        if (config.ide.emacs.core.treesitter.enable && cfg.treesitter.enable) then ''
          (use-package nix-ts-mode
            :mode (("\\.nix$" . nix-ts-mode)
                  ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-ts-mode))
            :hook
            (nix-mode-hook . (lambda () (setq-local tab-width 2)))
            :config
            (setq-default comment-start "#"))
        '' else ''
          (use-package nix-mode
            :mode (("\\.nix$" . nix-mode)
                   ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode))
            :hook
            (nix-mode-hook . (lambda () (setq-local tab-width 2)))
            :config
            (when (boundp 'company-backends)
              (add-to-list 'company-backends 'company-tabnine)
              (add-to-list 'company-backends 'company-capf))
            (add-to-list 'completion-at-point-functions #'pcomplete-completions-at-point))
        '';
      ide.emacs.core.treesitter.grammars = optionalAttrs (config.ide.emacs.core.treesitter.enable && cfg.treesitter.enable) {
        nix = "https://github.com/nix-community/tree-sitter-nix";
      };
      ide.emacs.core.treesitter.modeRemappings = optionalAttrs (config.ide.emacs.core.treesitter.enable && cfg.treesitter.enable) {
        nix-mode = "nix-ts-mode";
      };
      ide.emacs.completion.tempel.snippets = ''
        org-mode

        (nixsrc "#+begin_src nix" n> r> n "#+end_src" :post (org-edit-src-code))
      '';
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        home-manager = {
          desc = "home-manager upstream repo";
          path = "${wsRoot roots "github"}/rycee/home-manager";
          url = "https://github.com/rycee/home-manager/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          jump = true;
          searchSuffix = "search?q=";
        };
        nixpkgs = {
          desc = "Nixpkgs upstream repo";
          path = "${wsRoot roots "github"}/NixOS/nixpkgs";
          url = "https://github.com/NixOS/nixpkgs/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          jump = true;
          searchSuffix = "search?q=";
        };
        nix-versions = {
          desc = "Mapping packages version to Nixpkgs repo git history";
          url = "https://lazamar.co.uk/nix-versions/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          jump = true;
          searchSuffix = "?channel=nixos-unstable&package=";
        };
        nixos-search = {
          desc = "Search NixOS packages";
          url = "https://search.nixos.org/packages";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          jump = true;
          searchSuffix = "?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=";
        };
        nix-versions-blogpost = {
          desc = "Descriptive blog post for `nix-versions` (see alongside)";
          url = "https://lazamar.github.io/download-specific-package-version-with-nix/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        nixos-hardware = {
          desc = "NixOS hardware presets";
          path = "${wsRoot roots "github"}/NixOS/nixos-hardware";
          url = "https://github.com/NixOS/nixos-hardware";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nixospkg" = {
          desc = "NixOS packages";
          url = "https://nixos.org/nixos/packages.html";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nixos-status" = {
          desc = "NixOS status page";
          url = "https://status.nixos.org/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "status nixos";
              desktop = "main"; # [ref:desktop_main]
            }
          ];
        };
        "nix" = {
          desc = "it/nix";
          tags = [ "media" "video" ];
          url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZiPdYVvHZY8Day5RC934CE";
          browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
        };
        "nixos-news" = {
          desc = "NixOS weekly news";
          url = "https://weekly.nixos.org/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nixos/packages" = {
          desc = "Nixpkgs/unstable";
          url = "https://nixos.org/nixos/packages/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "?channel=nixpkgs-unstable&query=";
        };
        "nix/pm/repo" = mkGithubBookmark "NixOS" "nix" roots // {
          desc = "nix package manager repo";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "ghnix" = {
          desc = "github/lang:nix";
          tags = [ "forge" ];
          url = "https://github.com/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          jump = false;
          searchSuffix = "search?q=language%3Anix+";
        };
        "nixosr" = {
          desc = "NixOS + ";
          url = "";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "https://www.google.ru/?q=nixos+"; # FIXME: ???
        };
        "nixosopt" = {
          desc = "NixOS/options";
          url = "https://nixos.org/nixos/options.html#";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "";
        };
        "nixhydra" = {
          desc = "Nixpkgs from Hydra";
          url = "https://hydra.nixos.org/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "search?query=";
        };
        "discourse/nixos" = {
          desc = "NixOS Discourse";
          url = "https://discourse.nixos.org/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "HM/documentation" = {
          desc = "Home Manager documentation";
          url = "https://nix-community.github.io/home-manager/index.html";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nix-lib" = {
          desc = "Nix library reference";
          url = "https://teu5us.github.io/nix-lib.html";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "mynixos" = {
          desc = "Build and share reproducible software environments with Nix and NixOS";
          url = "https://mynixos.com";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nix.dev" = {
          desc = "An opinionated guide for developers getting things done using the Nix ecosystem.";
          url = "https://nix.dev";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nixlang.wiki" = {
          desc = "An unofficial, maintained wiki for NixOS";
          url = "https://nixlang.wiki/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "nixpkgs-ref-custom" = {
          desc = "Nixpkgs overview and reference by RyanTM";
          url = "https://ryantm.github.io/nixpkgs";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
        "flakes-inputs-syntax" = {
          desc = "Flakes inputs syntax reference";
          url = "https://nixos-and-flakes.thiscute.world/other-usage-of-flakes/inputs";
          browseWith = appCmdFull config.attributes.browser.default.traits;
        };
      };
    })
  ];
}
