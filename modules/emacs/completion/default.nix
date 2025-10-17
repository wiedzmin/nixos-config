{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;
with lib;

let
  cfg = config.ide.emacs.completion;
  user = config.attributes.mainUser.name;
in
{
  options = {
    ide.emacs.completion = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs completion setup";
      };
      backend = mkOption {
        type = types.enum [ "company" "corfu" ];
        default = "company";
        description = "Emacs completion UI to use. Currently, `company` and `corfu` are supported.";
      };
      snippets.backend = mkOption {
        type = types.enum [ "yasnippet" "tempel" ];
        default = "yasnippet";
        description = "Emacs snippets backend to use. Currently, `yasnippet` and `tempel` are supported.";
      };
      tempel.snippets = mkOption {
        type = types.lines;
        description = ''
          Tempel templates contents.

          Refer to https://github.com/minad/tempel?tab=readme-ov-file#template-syntax
          for snippets syntax.
        '';
        default = '''';
      };
      tempel.snippetsPath = mkOption {
        type = types.str;
        description = "Tempel templates contents file path";
        default = homePrefix user ".config/emacs/templates"; # TODO: search/use more specialized solution(s)
      };
      yasnippet.snippetsPaths = mkOption {
        type = types.listOf types.str;
        description = "Yasnippet snippets directories";
        default = [ ];
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.all-the-icons-completion
        epkgs.pos-tip
      ] ++ optionals (cfg.backend == "company") [
        epkgs.company
        epkgs.company-box
        epkgs.company-fuzzy
        epkgs.company-quickhelp
        epkgs.company-restclient
        epkgs.company-statistics
        epkgs.company-try-hard
      ] ++ optionals (cfg.backend == "corfu") [
        epkgs.cape
        epkgs.corfu
        epkgs.kind-icon
      ] ++ optionals (cfg.snippets.backend == "yasnippet") [
        epkgs.yasnippet
      ] ++ optionals (cfg.snippets.backend == "yasnippet" && config.ide.emacs.navigation.collections.backend == "consult") [
        epkgs.consult-yasnippet
      ] ++ optionals (cfg.snippets.backend == "tempel") [
        epkgs.tempel
        epkgs.tempel-collection
      ];
      ide.emacs.core.config = builtins.readFile ./elisp/completion.el +
        (optionalString (cfg.backend == "company") (builtins.readFile ./elisp/company.el)) +
        (optionalString (cfg.backend == "corfu") (builtins.readFile ./elisp/corfu.el)) +
        (optionalString (cfg.backend == "corfu" && config.history.emacs.enable) (builtins.readFile ./elisp/corfu-history.el)) +
        (optionalString (cfg.snippets.backend == "yasnippet")
          (readSubstituted config inputs pkgs [ ./subst/yasnippet.nix ] [ ./elisp/yasnippet.el ])) +
        (optionalString (cfg.snippets.backend == "yasnippet" && config.ide.emacs.navigation.collections.backend == "consult")
          (builtins.readFile ./elisp/consult-yasnippet.el)) +
        (optionalString (cfg.snippets.backend == "tempel")
          (readSubstituted config inputs pkgs [ ./subst/tempel.nix ] [ ./elisp/tempel.el ]));
      home-manager.users."${user}" = {
        home.file = optionalAttrs (cfg.snippets.backend == "tempel")
          {
            "${cfg.tempel.snippetsPath}".text = cfg.tempel.snippets;
          } // optionalAttrs (cfg.snippets.backend == "yasnippet") {
          ".emacs.d/resources/yasnippet" = {
            source = inputs.yasnippet-snippets;
            recursive = true;
          };
        };
      };
      ide.emacs.completion.yasnippet.snippetsPaths = [
        "${inputs.yasnippet-snippets}/snippets"
      ];
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = optionalAttrs (cfg.snippets.backend == "yasnippet")
        {
          yasnippet-snippets = {
            desc = "Yasnippet snippets collection";
            path = "${wsRoot roots "github"}/wiedzmin/yasnippet-snippets";
            url = "https://github.com/wiedzmin/yasnippet-snippets/";
            browseWith = appCmdFull config.attributes.browser.default.traits;
            jump = true;
            searchSuffix = "search?q=";
          };
          "yasnippet/snippet-reference" = {
            desc = "Yasnippet documentation / snippets reference";
            url = "http://joaotavora.github.io/yasnippet/snippet-reference.html";
            browseWith = appCmdFull config.attributes.browser.default.traits;
          };
          "yasnippet/snippet-development" = {
            desc = "Yasnippet documentation / snippets development";
            url = "http://joaotavora.github.io/yasnippet/snippet-development.html";
            browseWith = appCmdFull config.attributes.browser.default.traits;
          };
        } // optionalAttrs (cfg.snippets.backend == "tempel") {
        tempel = {
          desc = "Tempel repo";
          path = "${wsRoot roots "github"}/wiedzmin/minad/tempel";
          url = "https://github.com/minad/tempel";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          jump = true;
          searchSuffix = "search?q=";
        };
      };
    })
  ];
}
