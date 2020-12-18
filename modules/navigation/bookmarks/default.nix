{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.content;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.content = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-related tools.";
      };
      bookmarking.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable bookmarking harness";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.bookmarking.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
        buku_add = mkPythonScriptWithDeps "buku_add" (with pkgs; [ buku nurpkgs.pystdlib xsel ])
          (readSubstituted ../../subst.nix ./scripts/buku_add.py);
        buku_search_tag = mkShellScriptWithDeps "buku_search_tag" (with pkgs; [ coreutils nurpkgs.dmenu-ng gawk buku ])
          (readSubstituted ../../subst.nix ./scripts/buku_search_tag.sh);
        buku_search_url = mkShellScriptWithDeps "buku_search_url" (with pkgs; [ coreutils nurpkgs.dmenu-ng buku ])
          (readSubstituted ../../subst.nix ./scripts/buku_search_url.sh);
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable && cfg.bookmarking.enable) {
      wmCommon.keys = [{
        key = [ "m" ];
        cmd = "${pkgs.buku_add}/bin/buku_add";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ buku_add buku_search_tag buku_search_url ];
      };
    })
  ];
}
