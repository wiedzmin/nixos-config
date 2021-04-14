{ config, inputs, lib, pkgs, ... }:
with import ../../../modules/util.nix { inherit config inputs lib pkgs; };

rec {
  navigation.bookmarks.workspaces.roots = { "stale" = homePrefix "workspace/repos.stale"; };
  navigation.bookmarks.entries = {
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
      local.path = "${wsRoot "github"}/wiedzmin/nixos-config";
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
      myrepos = {
        "${wsRootAbs "github"}/wiedzmin/git-hooks" = {
          checkout = [ "git clone 'https://github.com/wiedzmin/git-hooks.git' 'git-hooks'" ];
        };
      };
    };
    nur-packages = { local.path = "${wsRoot "github"}/wiedzmin/nur-packages"; };
    postgres = { local.path = "${wsRoot "github"}/postgres/postgres"; };
    "staging/sandbox" = { local.path = homePrefix "workspace/sandbox/newpkgs"; };
    jwt-io = {
      remote = {
        url = "https://jwt.io/";
        desc = "JWT online debugger and libraries reference";
      };
    };
    use-package = { remote = { url = "https://github.com/jwiegley/use-package"; }; };
    "rycee/nur-expressions" = {
      local.path = "${config.navigation.bookmarks.workspaces.globalRoot}/gitlab.com/rycee/nur-expressions";
      remote.url = "https://gitlab.com/rycee/nur-expressions/";
    };
    "mastering/vlan" = {
      remote = {
        url = "http://xgu.ru/wiki/VLAN";
        desc = "VLAN article";
      };
    };
    "mastering/networking" = {
      remote = {
        url = "https://linkmeup.gitbook.io/sdsm/0.-planirovanie";
        desc = "Networking beginner book (online)";
      };
    };
  };
}
