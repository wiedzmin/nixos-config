{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.dev.ml;
in
{
  options = {
    dev.ml = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable development infra for Emacs";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable relevant bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.emacs.enable {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.tuareg ];
      ide.emacs.core.config = builtins.readFile ./elisp/ml.el;
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "ocaml-file-extensions" = {
          tags = [ "ocaml" "reference" ];
          remote.url = "https://blog.waleedkhan.name/ocaml-file-extensions/";
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "OCaml file extensions";
              activate = true;
            }
          ];
        };
        "parsing-ocamllex-menhir" = {
          tags = [ "ocaml" "reference" "blogpost" ];
          remote.url = "https://mukulrathi.com/create-your-own-programming-language/parsing-ocamllex-menhir/";
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "OCamllex and Menhir";
              activate = true;
            }
          ];
        };
        "ocaml-augmented-search" = {
          desc = "ocaml + ";
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=ocaml+";
          };
        };
      };
    })
  ];
}
