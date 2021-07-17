{ config, inputs, lib, pkgs, ... }:
with import ../../../modules/util.nix { inherit config inputs lib pkgs; };

rec {
  navigation.bookmarks.workspaces.roots = { "stale" = homePrefix "workspace/repos.stale"; };
  navigation.bookmarks.entries = {
    home-manager = {
      desc = "home-manager upstream repo";
      local.path = "${wsRoot "github"}/rycee/home-manager";
      remote = {
        url = "https://github.com/rycee/home-manager/";
        jump = true;
        searchSuffix = "search?q=";
      };
    };
    nixos = {
      desc = "My NixOS configurations";
      local.path = "${wsRoot "github"}/wiedzmin/nixos-config";
      remote = {
        url = "https://github.com/wiedzmin/nixos-config/";
        jump = true;
        searchSuffix = "search?q=";
      };
    };
    nixpkgs = {
      desc = "Nixpkgs upstream repo";
      local.path = "${wsRoot "github"}/NixOS/nixpkgs";
      remote = {
        url = "https://github.com/NixOS/nixpkgs/";
        jump = true;
        searchSuffix = "search?q=";
      };
    };
    emacs-overlay = {
      desc = "nix emacs overlay";
      local.path = "${wsRoot "github"}/nix-community/emacs-overlay";
      remote = {
        url = "https://github.com/nix-community/emacs-overlay/";
        jump = true;
        searchSuffix = "search?q=";
      };
    };
    yasnippet-snippets = {
      desc = "Yasnippet snippets collection";
      local.path = "${wsRoot "github"}/wiedzmin/yasnippet-snippets";
      remote = {
        url = "https://github.com/wiedzmin/yasnippet-snippets/";
        jump = true;
        searchSuffix = "search?q=";
      };
    };
    git-hooks = {
      desc = "my custom git hooks for `pre-commit`";
      local.path = "${wsRoot "github"}/wiedzmin/git-hooks";
      remote = {
        url = "https://github.com/wiedzmin/git-hooks";
        jump = true;
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
      desc = "JWT online debugger and libraries reference";
      remote.url = "https://jwt.io/";
    };
    use-package = { remote.url = "https://github.com/jwiegley/use-package"; };
    "rycee/nur-expressions" = {
      local.path = "${config.navigation.bookmarks.workspaces.globalRoot}/gitlab.com/rycee/nur-expressions";
      remote.url = "https://gitlab.com/rycee/nur-expressions/";
    };
    "mastering/vlan" = {
      desc = "VLAN article";
      remote.url = "http://xgu.ru/wiki/VLAN";
    };
    "mastering/networking" = {
      desc = "Networking beginner book (online)";
      remote.url = "https://linkmeup.gitbook.io/sdsm/0.-planirovanie";
    };
    "toolbox" = {
      local.path = "${wsRoot "github"}/wiedzmin/toolbox";
      remote.url = "https://github.com/wiedzmin/toolbox/";
      myrepos = {
        "${wsRootAbs "github"}/wiedzmin/toolbox" = {
          checkout = [ "git clone 'https://github.com/wiedzmin/toolbox.git' 'toolbox'" ];
        };
      };
    };
    "wmtools" = {
      local.path = "${wsRoot "github"}/wiedzmin/wmtools";
      remote.url = "https://github.com/wiedzmin/wmtools/";
      myrepos = {
        "${wsRootAbs "github"}/wiedzmin/wmtools" = {
          checkout = [ "git clone 'https://github.com/wiedzmin/wmtools.git' 'wmtools'" ];
        };
      };
    };
    "libtmux-go" = {
      local.path = "${wsRoot "github"}/wiedzmin/libtmux-go";
      remote.url = "https://github.com/wiedzmin/libtmux-go/";
      myrepos = {
        "${wsRootAbs "github"}/wiedzmin/libtmux-go" = {
          checkout = [ "git clone 'https://github.com/wiedzmin/libtmux-go.git' 'libtmux-go'" ];
        };
      };
    };
  };
}
