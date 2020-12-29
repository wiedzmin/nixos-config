{ config, inputs, lib, pkgs, ... }:
with import ../../../modules/util.nix { inherit config inputs lib pkgs; };

rec {
  # IDEA: make script for extracting from shell history based on substring
  custom.navigation.bookmarks.entries = {
    gourmet = { local.path = "${wsRoot "github"}/wiedzmin/gourmet"; };
    home-manager = {
      local.path = "${wsRoot "github"}/rycee/home-manager";
      remote = {
        url = "https://github.com/rycee/home-manager/";
        jump = true;
        desc = "home-manager upstream repo";
        searchSuffix = "search?q=";
      };
    };
    nixos = {
      local.path = "/etc/nixos";
      remote = {
        url = "https://github.com/wiedzmin/nixos-config/";
        jump = true;
        desc = "My NixOS configurations";
        searchSuffix = "search?q=";
      };
    };
    nixpkgs = {
      local.path = "${wsRoot "github"}/NixOS/nixpkgs";
      remote = {
        url = "https://github.com/NixOS/nixpkgs/";
        jump = true;
        desc = "Nixpkgs upstream repo";
        searchSuffix = "search?q=";
      };
    };
    emacs-overlay = {
      local.path = "${wsRoot "github"}/nix-community/emacs-overlay";
      remote = {
        url = "https://github.com/nix-community/emacs-overlay/";
        jump = true;
        desc = "nix emacs overlay";
        searchSuffix = "search?q=";
      };
    };
    yasnippet-snippets = {
      local.path = "${wsRoot "github"}/wiedzmin/yasnippet-snippets";
      remote = {
        url = "https://github.com/wiedzmin/yasnippet-snippets/";
        jump = true;
        desc = "Yasnippet snippets collection";
        searchSuffix = "search?q=";
      };
    };
    git-hooks = {
      local.path = "${wsRoot "github"}/wiedzmin/git-hooks";
      remote = {
        url = "https://github.com/wiedzmin/git-hooks";
        jump = true;
        desc = "my custom git hooks for `pre-commit`";
        searchSuffix = "search?q=";
      };
    };
    nur-packages = { local.path = "${wsRoot "github"}/wiedzmin/nur-packages"; };
    postgres = { local.path = "${wsRoot "github"}/postgres/postgres"; };
    "staging/sandbox" = { local.path = homePrefix "workspace/sandbox"; };
    jwt-io = {
      remote = {
        url = "https://jwt.io/";
        desc = "JWT online debugger and libraries reference";
      };
    };
  };
}
