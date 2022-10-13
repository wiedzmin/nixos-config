{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;

let
  user = config.attributes.mainUser.name;
in
rec {
  navigation.bookmarks.workspaces.roots = { "stale" = homePrefix user "workspace/repos.stale"; };
  navigation.bookmarks.entries = {
    nixos = {
      desc = "My NixOS configurations";
      local.path = "${wsRoot roots "github"}/wiedzmin/nixos-config";
      remote = {
        url = "https://github.com/wiedzmin/nixos-config/";
        jump = true;
        searchSuffix = "search?q=";
      };
      windowRules = [
        {
          class = "Emacs";
          title = "nixos";
          desktop = "edit";
        }
      ];
    };
    git-hooks = {
      desc = "my custom git hooks for `pre-commit`";
      local.path = "${wsRoot roots "github"}/wiedzmin/git-hooks";
      remote = {
        url = "https://github.com/wiedzmin/git-hooks";
        jump = true;
        searchSuffix = "search?q=";
      };
      batchvcs = {
        "${wsRoot roots "github"}/wiedzmin/git-hooks" = {
          checkout = [ "git clone 'https://github.com/wiedzmin/git-hooks.git' 'git-hooks'" ];
        };
      };
    };
    nur-packages = { local.path = "${wsRoot roots "github"}/wiedzmin/nur-packages"; };
    postgres = { local.path = "${wsRoot roots "github"}/postgres/postgres"; };
    "staging/sandbox" = { local.path = homePrefix user "workspace/sandbox/newpkgs"; };
    jwt-io = {
      desc = "JWT online debugger and libraries reference";
      remote.url = "https://jwt.io/";
    };
    "mastering/vlan" = {
      desc = "VLAN article";
      remote.url = "http://xgu.ru/wiki/VLAN";
    };
    "mastering/networking" = {
      desc = "Networking beginner book (online)";
      remote.url = "https://linkmeup.gitbook.io/sdsm/0.-planirovanie";
    };
    "toolbox" = mkGithubBookmarkWithMyrepos "wiedzmin" "toolbox" roots // {
      windowRules = [
        {
          class = "Emacs";
          title = "toolbox";
          desktop = "edit";
        }
      ];
    };
    "wmtools" = mkGithubBookmarkWithMyrepos "wiedzmin" "wmtools" roots // {
      windowRules = [
        {
          class = "Emacs";
          title = "wmtools";
          desktop = "edit";
        }
      ];
    };
    "libtmux-go" = mkGithubBookmarkWithMyrepos "wiedzmin" "libtmux-go" roots;
    "lumosity" = {
      desc = "Lumosity";
      remote.url = "https://lumosity.com/";
    };
    "relax-fm" = {
      desc = "Relax FM radio";
      tags = [ "media" "fm" "radio" ];
      remote.url = "https://relax-fm.ru";
    };
    "lightnings" = {
      desc = "Lightning map";
      tags = [ "map" "weather" ];
      remote.url = "https://ru.blitzortung.org/live_dynamic_maps.php";
    };
    "marinetraffic" = {
      desc = "Marine vessels map";
      tags = [ "map" "weather" ];
      remote.url = "https://www.marinetraffic.com";
    };
    "chillout" = {
      desc = "Music streams"; # note /chillout
      tags = [ "media" "fm" ];
      remote = {
        url = "https://www.di.fm/";
        searchSuffix = "search?q=";
      };
    };
    "lightnings2" = {
      desc = "Lightning map";
      tags = [ "map" "weather" ];
      remote.url = "https://www.lightningmaps.org/";
    };
    "encodings" = {
      desc = "Encodings translation";
      tags = [ "tools" "online" ];
      remote.url = "https://www.online-decoder.com/ru";
    };
    "youtube" = {
      desc = "Youtube";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/";
        jump = true;
        searchSuffix = "results?search_query=";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "google-photos" = {
      desc = "Google Photos";
      tags = [ "media" "photo" ];
      remote.url = "https://photos.google.com/";
    };
    "bitbucket" = {
      desc = "Bitbucket code hosting";
      tags = [ "forge" ];
      remote.url = "https://bitbucket.org";
    };
    "digitalocean" = {
      desc = "DigitalOcean account";
      tags = [ "servers" ];
      remote.url = "https://cloud.digitalocean.com/projects/";
    };
    "linode" = {
      desc = "Linode account";
      tags = [ "servers" ];
      remote.url = "https://cloud.linode.com/linodes";
    };
    "crontabs" = {
      desc = "Crontab format helpers";
      tags = [ "tools" "online" ];
      remote.url = "https://crontab.guru/";
    };
    "gdocs" = {
      remote = {
        url = "https://docs.google.com/document/u/0/";
        browser = config.attributes.browser.fallback.cmd;
      };
    };
    "gh" = {
      desc = "Github code hosting";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        searchSuffix = "search?&type=code&q=";
      };
    };
    "ghpath" = {
      desc = "Github code hosting by path";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        searchSuffix = "search?type=code&q=in%3Apath+";
      };
    };
    "ghfflake" = {
      desc = "Github search in 'flake.nix' files";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        searchSuffix = "search?q=filename%3Aflake.nix+";
      };
    };
    "ghinpath" = {
      desc = "Github search in file paths";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        searchSuffix = "search?q=in%3Apath+";
      };
    };
    "ghw" = {
      desc = "Github personal account";
      tags = [ "forge" ];
      remote.url = "https://github.com/wiedzmin";
    };
    "gmail" = {
      desc = "GMail account";
      remote.url = "https://mail.google.com/mail/u/0/";
    };
    "repology/projects" = {
      desc = "Repology";
      remote = {
        url = "https://repology.org/projects/";
        searchSuffix = "?search=";
      };
    };
    "repology/maintainers" = {
      desc = "Repology maintainers";
      remote = {
        url = "https://repology.org/maintainers/";
        searchSuffix = "?search=";
      };
    };
    "repology/repos" = {
      desc = "Repology repositories";
      remote.url = "https://repology.org/repositories";
    };
    "torrefacto" = {
      desc = "Torrefacto shop";
      remote.url = "https://torrefacto.ru/";
    };
    "gtrans" = {
      desc = "Google translate service";
      tags = [ "dictionary" ];
      remote.url = "https://translate.google.com/";
    };
    "teleweb" = {
      desc = "Telegram web interface";
      remote.url = "https://web.telegram.org/";
    };
    "fb" = {
      tags = [ "fb" ];
      remote = {
        url = "https://www.facebook.com/";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
        vpn = "proton";
      };
      windowRules = [
        {
          class = mkWSMappingBrowsersRegexp config.attributes.browser;
          title = "http facebook";
          desktop = "ent";
        }
      ];
    };
    "fbmess" = {
      desc = "Facebook Messenger";
      tags = [ "fb" ];
      remote = {
        browser = config.attributes.browser.fallback.cmd;
        url = "https://www.facebook.com/messages/t/";
      };
    };
    "vkontakte" = {
      desc = "VK";
      tags = [ "vk" ];
      remote = {
        # browser = config.attributes.browser.fallback.cmd;
        url = "https://vk.com/";
      };
    };
    "mt" = {
      desc = "Multitran";
      tags = [ "dictionary" ];
      remote = {
        url = "https://www.multitran.com/m.exe?l1=1&l2=2";
        searchSuffix = "&s=";
      };
      windowRules = [
        {
          class = mkWSMappingBrowsersRegexp config.attributes.browser;
          title = "multitran";
          desktop = "web";
        }
      ];
    };
    "ventusky" = {
      desc = "Weather map / temperature";
      tags = [ "map" "weather" ];
      remote.url = "https://www.ventusky.com/?l=temperature";
    };
    "rust" = {
      desc = "it/rust";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZGJn6Q7c6G7NAaiWD5J__z";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "orgmode" = {
      desc = "emacs/orgmode";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZJBJT4i3KvMbAuIicqwSau";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "emacs" = {
      desc = "emacs";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZM2QXMb8eZ8_I6JhfZctWi";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "it/pm" = {
      desc = "it/management";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZT0PdGw9dp_Ic9jYODxqxB";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "lisp" = {
      desc = "lisp";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZioLb2SeloXWPkP2PmxM3t";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "var/it" = {
      desc = "it/var";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGa0RDozpS3WxmUcoYSsOmTU";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "technics" = {
      desc = "ethnic/music/technics";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGaKdF1Ewgg9IWW96UPpxTFi";
        browser = config.attributes.browser.fallback.cmd;
      };
    };
    "var" = {
      desc = "var";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGajeWFlOnp19tSDu7XhWFo2";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "python" = {
      desc = "it/python";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGbTK2Y7VPHoc01tbQrZww5h";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "hl" = {
      desc = "it/conf/highload";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGbiXj0rSMDJEuogTY4y8SQL";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "golang" = {
      desc = "it/golang";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGbtFoRo94tXdDBgjwsTh02r";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "andrewtropin/playlists" = {
      desc = "playlists of IT-related videos by Andrew Tropin";
      tags = [ "it" "nix" "guix" "emacs" "video" ];
      remote = {
        url = "https://www.youtube.com/c/TROP1N/playlists";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "ya" = {
      desc = "Yandex";
      remote = {
        url = "https://yandex.ru/";
        jump = true;
        searchSuffix = "search/?text=";
      };
      windowRules = [
        {
          class = mkWSMappingBrowsersRegexp config.attributes.browser;
          title = "http yandex";
          desktop = "web";
        }
      ];
    };
    "lol" = {
      desc = "LOL paintings";
      tags = [ "paintings" ];
      remote.url = "https://deti-online.com/raskraski/lol";
    };
    "4let" = {
      desc = "var paintings";
      tags = [ "paintings" ];
      remote.url = "https://moi-raskraski.ru/raskraski-dlya-malenkikh/raskraski-dlya-detej-4-let";
    };
    "porosenok" = {
      desc = "porosenok";
      tags = [ "paintings" ];
      remote.url = "https://shutniki.club/wp-content/uploads/2020/04/Raskraska_s_porosenkom_4_15120840.jpg";
    };
    "ml24" = {
      enable = false;
      tags = [ "shop" ];
      remote.url = "https://market-land24.ru/";
    };
    "mealty" = {
      tags = [ "shop" ];
      remote.url = "https://mealty.ru/catalog/";
    };
    "khuur" = {
      tags = [ "music" "culture" ];
      remote.url = "https://stevemorel.info/sharing/morin-huur/";
    };
    "connsp" = {
      desc = "Internet connection speed";
      tags = [ "networking" "internet" "speed" ];
      remote.url = "https://speedtest24net.ru/";
    };
    "dict" = {
      desc = "Dictionary";
      remote = {
        url = "https://dictionary.reference.com/browse/";
        searchSuffix = "";
      };
    };
    "ghs" = {
      desc = "Github";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/search?type=Everything&q=";
        searchSuffix = "";
      };
    };
    "gm" = {
      desc = "Google maps";
      tags = [ "map" ];
      remote = {
        url = "https://maps.google.com/maps?q=";
        searchSuffix = "";
      };
    };
    "goo" = {
      desc = "Google";
      remote = {
        url = "https://www.google.com/";
        searchSuffix = "search?num=100&q=";
      };
    };
    "ling" = {
      desc = "Lingvo";
      tags = [ "dictionary" ];
      remote = {
        url = "https://lingvopro.abbyyonline.com/ru/Search/en-ru/";
        searchSuffix = "";
      };
    };
    "md" = {
      desc = "My Delicious";
      remote = {
        url = "https://www.delicious.com/__ronin__/";
        searchSuffix = "";
      };
    };
    "scode" = {
      desc = "Searchcode";
      tags = [ "dev" ];
      remote = {
        url = "";
        searchSuffix = "https://searchcode.com/?q=";
      };
    };
    "warc" = {
      desc = "Web archive";
      tags = [ "history" ];
      remote = {
        url = "https://web.archive.org/";
        searchSuffix = "web/*/";
      };
    };
    "codeforces" = {
      tags = [ "contest" "code" ];
      remote.url = "https://codeforces.com/";
    };
    "projecteuler" = {
      tags = [ "contest" "code" ];
      remote.url = "https://projecteuler.net/";
    };
    "hackerrank" = {
      tags = [ "contest" "code" ];
      remote.url = "https://www.hackerrank.com/";
    };
    "exercism" = {
      tags = [ "contest" "code" ];
      remote.url = "https://exercism.io/";
    };
    "leetcode" = {
      tags = [ "contest" "code" ];
      remote.url = "https://leetcode.com/";
    };
    "codewars" = {
      tags = [ "contest" "code" ];
      remote.url = "https://www.codewars.com/";
    };
    "projects/loggerhead" = mkGithubBookmark "wiedzmin" "loggerhead" roots; # arbtt analog
    "projects/gourmet" = mkGithubBookmarkWithMyrepos "wiedzmin" "gourmet" roots;
    "mastering/wiedzmin/cl-study" = mkGithubBookmarkWithMyrepos "wiedzmin" "cl-study" roots;
    "mastering/wiedzmin/lisp-koans" = mkGithubBookmarkWithMyrepos "wiedzmin" "lisp-koans" roots;
    "projects/reference/go-org" = mkGithubBookmark "niklasfasching" "go-org" roots;
    "projects/reference/xgb" = mkGithubBookmark "BurntSushi" "xgb" roots;
    "projects/reference/xgbutil" = mkGithubBookmark "BurntSushi" "xgbutil" roots;
    "projects/reference/arbtt" = mkGithubBookmark "nomeata" "arbtt" roots;
    "projects/reference/code-maat" = mkGithubBookmark "adamtornhill" "code-maat" roots // {
      # TODO: https://grahamenos.com/ft-clojure-nix.html - java/clojure packaging
      transient = true;
    };
    "rofi" = mkGithubBookmark "davatorium" "rofi" roots // { transient = true; };
    "cppreference" = {
      tags = [ "c++" "cpp" "reference" ];
      remote = { url = "https://en.cppreference.com/w/"; };
    };
    "libtmux" = mkGithubBookmark "tmux-python" "libtmux" roots // { transient = true; };
    "paintings" = {
      tags = [ "paint" "play" ];
      browser = config.attributes.browser.fallback.cmd;
      remote = { url = "https://yandex.ru/search/?lr=213&text=раскраски"; };
    };
    "paintings/dragons" = {
      tags = [ "paint" "play" ];
      browser = config.attributes.browser.fallback.cmd;
      remote = { url = "https://deti-online.com/raskraski/drakony/"; };
    };
    "paintings/lilo_stitch" = {
      tags = [ "paint" "play" ];
      browser = config.attributes.browser.fallback.cmd;
      remote = { url = "https://raskrasil.com/raskraski-lilo-i-stich/"; };
    };
    "real mongolia" = {
      tags = [ "mongol" "video" "youtube" ];
      remote = {
        url = "https://www.youtube.com/channel/UCtMZD_UHQxCowHltovfEnvQ";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "cider/discord" = {
      tags = [ "emacs" "clojure" "cider" "discord" ];
      remote = { url = "https://discord.com/channels/797747718385696768/797750325179973672"; };
    };
    "ddg/bangs" = {
      tags = [ "duckduckgo" "bangs" ];
      remote = { url = "https://duckduckgo.com/bang"; };
    };
    papers-i-love = {
      desc = "Computer science and computer-adjacent papers";
      local.path = "${wsRoot roots "github"}/fogus/papers-i-love";
      remote = {
        url = "https://github.com/fogus/papers-i-love";
        jump = true;
        searchSuffix = "search?q=";
      };
    };
    agenda = {
      local.path = homePrefix user "docs/org/roam/agenda.org";
      windowRules = [
        {
          class = "Emacs";
          title = "agenda.org";
          desktop = "edit";
          activate = true;
        }
      ];
    };
    goodies = {
      local.path = homePrefix user "docs/org/roam/current_goodies.org";
      windowRules = [
        {
          class = "Emacs";
          title = "current_goodies.org";
          desktop = "edit";
          activate = true;
        }
      ];
    };
  };
  pim.timetracking.rules = mkArbttBrowserTitleRule [ "Facebook" ] "site:facebook" config.attributes.browser;
}
