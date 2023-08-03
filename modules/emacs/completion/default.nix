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
        description = "Tempel templates contents";
        default = '''';
      };
      tempel.snippetsPath = mkOption {
        type = types.str;
        description = "Tempel templates contents file path";
        default = homePrefix user ".config/emacs/templates"; # TODO: search/use more specialized solution(s)
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
        epkgs.corfu-doc
        epkgs.kind-icon
      ] ++ optionals (cfg.snippets.backend == "yasnippet") [
        epkgs.yasnippet
      ] ++ optionals (cfg.snippets.backend == "yasnippet" && config.ide.emacs.navigation.collections.backend == "consult") [
        epkgs.consult-yasnippet
      ] ++ optionals (cfg.snippets.backend == "tempel") [
        epkgs.tempel
        epkgs.tempel-collection
      ];
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst.nix ]
        ([ ./elisp/completion.el ] ++ optionals (cfg.backend == "company") [ ./elisp/company.el ]
          ++ optionals (cfg.backend == "corfu") [ ./elisp/corfu.el ]
          ++ optionals (cfg.backend == "corfu" && config.ide.emacs.history.enable) [ ./elisp/corfu-history.el ]
          ++ optionals (cfg.snippets.backend == "yasnippet") [ ./elisp/yasnippet.el ]
          ++ optionals (cfg.snippets.backend == "yasnippet" && config.ide.emacs.navigation.collections.backend == "consult") [ ./elisp/consult-yasnippet.el ]
          ++ optionals (cfg.snippets.backend == "tempel") [ ./elisp/tempel.el ]);
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
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = optionalAttrs (cfg.snippets.backend == "yasnippet")
        {
          yasnippet-snippets = {
            desc = "Yasnippet snippets collection";
            local.path = "${wsRoot roots "github"}/wiedzmin/yasnippet-snippets";
            remote = {
              url = "https://github.com/wiedzmin/yasnippet-snippets/";
              jump = true;
              searchSuffix = "search?q=";
            };
          };
        } // optionalAttrs (cfg.snippets.backend == "tempel") {
        tempel = {
          desc = "Tempel repo";
          local.path = "${wsRoot roots "github"}/wiedzmin/minad/tempel";
          remote = {
            url = "https://github.com/minad/tempel";
            jump = true;
            searchSuffix = "search?q=";
          };
        };
      };
    })
  ];
}
