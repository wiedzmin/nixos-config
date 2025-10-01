{ config, pkgs, ... }:
with pkgs.unstable.commonutils;
with config.navigation.bookmarks.workspaces;

let
  user = config.attributes.mainUser.name;
in
{
  navigation.bookmarks.entries = {
    nixos = {
      desc = "My NixOS configurations";
      local.path = "${wsRoot roots "github"}/wiedzmin/nixos-config";
      remote = {
        url = "https://github.com/wiedzmin/nixos-config/";
        browser = appCmdFull config.attributes.browser.default.traits;
        jump = true;
        searchSuffix = "search?q=";
      };
      windowRules = [
        {
          class = "Emacs";
          title = "nixos";
          desktop = "main"; # [ref:desktop_main]
        }
        {
          class = "kitty";
          title = "nixos";
          desktop = "main"; # [ref:desktop_main]
        }
      ];
    };
    git-hooks = {
      desc = "my custom git hooks for `pre-commit`";
      local.path = "${wsRoot roots "github"}/wiedzmin/git-hooks";
      remote = {
        url = "https://github.com/wiedzmin/git-hooks";
        browser = appCmdFull config.attributes.browser.default.traits;
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
      remote = {
        url = "https://jwt.io/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "mastering/vlan" = {
      desc = "VLAN article";
      remote = {
        url = "http://xgu.ru/wiki/VLAN";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "mastering/networking" = {
      desc = "Networking beginner book (online)";
      remote = {
        url = "https://linkmeup.gitbook.io/sdsm/0.-planirovanie";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "toolbox" = mkGithubBookmarkWithMyrepos "wiedzmin" "toolbox" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
      windowRules = [
        {
          class = "Emacs";
          title = "toolbox";
          desktop = "main"; # [ref:desktop_main]
        }
      ];
    };
    "timeshitting" = mkGithubBookmarkWithMyrepos "wiedzmin" "timeshitting" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
      windowRules = [
        {
          class = "Emacs";
          title = "timeshitting";
          desktop = "main"; # [ref:desktop_main]
        }
      ];
    };
    "wmtools" = mkGithubBookmarkWithMyrepos "wiedzmin" "wmtools" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
      windowRules = [
        {
          class = "Emacs";
          title = "wmtools";
          desktop = "main"; # [ref:desktop_main]
        }
      ];
    };
    "lumosity" = {
      desc = "Lumosity";
      remote = {
        url = "https://lumosity.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "relax-fm" = {
      desc = "Relax FM radio";
      tags = [ "media" "fm" "radio" ];
      remote = {
        url = "https://relax-fm.ru";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "lightnings" = {
      desc = "Lightning map";
      tags = [ "map" "weather" ];
      remote = {
        url = "https://ru.blitzortung.org/live_dynamic_maps.php";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "marinetraffic" = {
      desc = "Marine vessels map";
      tags = [ "map" "weather" ];
      remote = {
        url = "https://www.marinetraffic.com";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "chillout" = {
      desc = "Music streams"; # note /chillout
      tags = [ "media" "fm" ];
      remote = {
        url = "https://www.di.fm/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "search?q=";
      };
    };
    "lightnings2" = {
      desc = "Lightning map";
      tags = [ "map" "weather" ];
      remote = {
        url = "https://www.lightningmaps.org/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "encodings" = {
      desc = "Encodings translation";
      tags = [ "tools" "online" ];
      remote = {
        url = "https://www.online-decoder.com/ru";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "Ricktube" = {
      desc = "Ricktube";
      tags = [ "media" "video" "russian" ];
      remote = {
        url = "https://ricktube.ru/";
        jump = true;
        searchSuffix = "video?q=";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
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
      remote = {
        url = "https://photos.google.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "bitbucket" = {
      desc = "Bitbucket code hosting";
      tags = [ "forge" ];
      remote = {
        url = "https://bitbucket.org";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "digitalocean" = {
      desc = "DigitalOcean account";
      tags = [ "servers" ];
      remote = {
        url = "https://cloud.digitalocean.com/projects/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "linode" = {
      desc = "Linode account";
      tags = [ "servers" ];
      remote = {
        url = "https://cloud.linode.com/linodes";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "crontabs" = {
      desc = "Crontab format helpers";
      tags = [ "tools" "online" ];
      remote = {
        url = "https://crontab.guru/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "gdocs" = {
      remote = {
        url = "https://docs.google.com/document/u/0/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "ydocs" = {
      remote = {
        url = "https://docs.yandex.ru";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "gh" = {
      desc = "Github code hosting";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "search?&type=code&q=";
      };
    };
    "ghpath" = {
      desc = "Github code hosting by path";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "search?type=code&q=in%3Apath+";
      };
    };
    "ghfflake" = {
      desc = "Github search in 'flake.nix' files";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "search?q=filename%3Aflake.nix+";
      };
    };
    "ghinpath" = {
      desc = "Github search in file paths";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "search?q=in%3Apath+";
      };
    };
    "ghw" = {
      desc = "Github personal account";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/wiedzmin";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "cba" = {
      desc = "Codeberg personal account";
      tags = [ "forge" ];
      meta = {
        url = "https://codeberg.org/alex3rd";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "gmail" = {
      desc = "GMail account";
      remote = {
        url = "https://mail.google.com/mail/u/0/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "repology/projects" = {
      desc = "Repology";
      remote = {
        url = "https://repology.org/projects/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "?search=";
      };
    };
    "repology/maintainers" = {
      desc = "Repology maintainers";
      remote = {
        url = "https://repology.org/maintainers/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "?search=";
      };
    };
    "repology/repos" = {
      desc = "Repology repositories";
      remote = {
        url = "https://repology.org/repositories";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "torrefacto" = {
      desc = "Torrefacto shop";
      remote = {
        url = "https://torrefacto.ru/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "gtrans" = {
      desc = "Google translate service";
      tags = [ "dictionary" ];
      remote = {
        url = "https://translate.google.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "teleweb" = {
      desc = "Telegram web interface";
      remote = {
        url = "https://web.telegram.org/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "fb" = {
      enable = false;
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
          desktop = "var"; # [ref:desktop_var]
        }
      ];
    };
    "fbmess" = {
      enable = false;
      desc = "Facebook Messenger";
      tags = [ "fb" ];
      remote = {
        browser = appCmdFull config.attributes.browser.fallback.traits;
        url = "https://www.facebook.com/messages/t/";
      };
    };
    "vkontakte" = {
      desc = "VK";
      tags = [ "vk" ];
      remote = {
        browser = appCmdFull config.attributes.browser.fallback.traits;
        url = "https://vk.com/";
      };
    };
    "mt" = {
      desc = "Multitran";
      tags = [ "dictionary" ];
      remote = {
        url = "https://www.multitran.com/m.exe?l1=1&l2=2";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "&s=";
      };
    };
    "ventusky" = {
      desc = "Weather map / temperature";
      tags = [ "map" "weather" ];
      remote = {
        url = "https://www.ventusky.com/?l=temperature";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
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
    "techniques" = {
      desc = "ethnic/music/techniques";
      tags = [ "media" "video" ];
      remote = {
        url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGaKdF1Ewgg9IWW96UPpxTFi";
        browser = appCmdFull config.attributes.browser.fallback.traits;
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
        browser = appCmdFull config.attributes.browser.default.traits;
        jump = true;
        searchSuffix = "search/?text=";
      };
    };
    "ml24" = {
      enable = false;
      tags = [ "shop" ];
      remote = {
        url = "https://market-land24.ru/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "mealty" = {
      tags = [ "shop" ];
      remote = {
        url = "https://mealty.ru/catalog/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "khuur" = {
      tags = [ "music" "culture" ];
      remote = {
        url = "https://stevemorel.info/sharing/morin-huur/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "connsp" = {
      desc = "Internet connection speed";
      tags = [ "networking" "internet" "speed" ];
      remote = {
        url = "https://speedtest24net.ru/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "dict" = {
      desc = "Dictionary";
      remote = {
        url = "https://dictionary.reference.com/browse/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "";
      };
    };
    "ghs" = {
      desc = "Github";
      tags = [ "forge" ];
      remote = {
        url = "https://github.com/search?type=Everything&q=";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "";
      };
    };
    "gm" = {
      desc = "Google maps";
      tags = [ "map" ];
      remote = {
        url = "https://maps.google.com/maps?q=";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "";
      };
    };
    "goo" = {
      desc = "Google";
      remote = {
        url = "https://www.google.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "search?num=100&q=";
      };
    };
    "ling" = {
      desc = "Lingvo";
      tags = [ "dictionary" ];
      remote = {
        url = "https://lingvopro.abbyyonline.com/ru/Search/en-ru/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "";
      };
    };
    "md" = {
      desc = "My Delicious";
      remote = {
        url = "https://www.delicious.com/__ronin__/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "";
      };
    };
    "scode" = {
      desc = "Searchcode";
      tags = [ "dev" ];
      remote = {
        url = "https://searchcode.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "?q=";
      };
    };
    "warc" = {
      desc = "Web archive";
      tags = [ "history" ];
      remote = {
        url = "https://web.archive.org/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "web/*/";
      };
    };
    "codeforces" = {
      tags = [ "contest" "code" ];
      remote = {
        url = "https://codeforces.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "projecteuler" = {
      tags = [ "contest" "code" ];
      remote = {
        url = "https://projecteuler.net/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "hackerrank" = {
      tags = [ "contest" "code" ];
      remote = {
        url = "https://www.hackerrank.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "exercism" = {
      tags = [ "contest" "code" ];
      remote = {
        url = "https://exercism.io/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "leetcode" = {
      tags = [ "contest" "code" ];
      remote = {
        url = "https://leetcode.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "codewars" = {
      tags = [ "contest" "code" ];
      remote = {
        url = "https://www.codewars.com/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "projects/loggerhead" = mkGithubBookmark "wiedzmin" "loggerhead" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    }; # arbtt analog
    "projects/gourmet" = mkGithubBookmarkWithMyrepos "wiedzmin" "gourmet" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "mastering/wiedzmin/cl-study" = mkGithubBookmarkWithMyrepos "wiedzmin" "cl-study" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "mastering/wiedzmin/lisp-koans" = mkGithubBookmarkWithMyrepos "wiedzmin" "lisp-koans" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/go-org" = mkGithubBookmark "niklasfasching" "go-org" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/xgb" = mkGithubBookmark "BurntSushi" "xgb" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/xgbutil" = mkGithubBookmark "BurntSushi" "xgbutil" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/arbtt" = mkGithubBookmark "nomeata" "arbtt" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/code-maat" = mkGithubBookmark "adamtornhill" "code-maat" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
      transient = true;
    };
    "ft-clojure-nix" = {
      tags = [ "java" "clojure" "packaging" "nix" ];
      remote = {
        url = "https://grahamenos.com/ft-clojure-nix.html";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "rofi" = mkGithubBookmark "davatorium" "rofi" roots // {
      remote.browser = appCmdFull config.attributes.browser.default.traits;
      transient = true;
    };
    "cppreference" = {
      tags = [ "c++" "cpp" "reference" ];
      remote = {
        url = "https://en.cppreference.com/w/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "paintings" = {
      tags = [ "paint" "play" ];
      remote = {
        url = "https://yandex.ru/search/?lr=213&text=раскраски";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "real mongolia" = {
      tags = [ "mongol" "video" "youtube" ];
      remote = {
        url = "https://www.youtube.com/channel/UCtMZD_UHQxCowHltovfEnvQ";
        browser = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      };
    };
    "mongoltoli" = {
      tags = [ "mongol" "dictionary" "reference" ];
      remote = {
        url = "https://mongoltoli.mn";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "cider/discord" = {
      tags = [ "emacs" "clojure" "cider" "discord" ];
      remote = {
        url = "https://discord.com/channels/797747718385696768/797750325179973672";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "ddg/bangs" = {
      tags = [ "duckduckgo" "bangs" ];
      remote = {
        url = "https://duckduckgo.com/bang";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    papers-i-love = {
      desc = "Computer science and computer-adjacent papers";
      local.path = "${wsRoot roots "github"}/fogus/papers-i-love";
      remote = {
        url = "https://github.com/fogus/papers-i-love";
        browser = appCmdFull config.attributes.browser.default.traits;
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
          desktop = "main"; # [ref:desktop_main]
          activate = true;
        }
      ];
    };
    roam-root = {
      desc = "Org Roam root directory";
      tags = [ "roam" "org" ];
      local.path = homePrefix user "docs/org/roam";
    };
    "telegram/downloads" = {
      desc = "Telegram donloads path";
      tags = [ "telegram" ];
      local.path = config.attributes.downloadPath.telegram;
    };
    goodies = {
      local.path = homePrefix user "docs/org/roam/current_goodies.org";
      windowRules = [
        {
          class = "Emacs";
          title = "current_goodies.org";
          desktop = "main"; # [ref:desktop_main]
          activate = true;
        }
      ];
    };
    "shops/ozon" = {
      tags = [ "shop" "ozon" "orders" ];
      remote = {
        url = "https://www.ozon.ru/my/orderlist";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "mongol.su/calendar" = {
      tags = [ "mongol" "reference" "calendar" ];
      remote = {
        url = "http://mongol.su/календарь/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "shops/wildberries" = {
      tags = [ "shop" "wildberries" "orders" ];
      remote = {
        url = "https://www.wildberries.ru/lk/myorders/delivery";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "russia1_hd" = {
      tags = [ "media" "tv" ];
      remote = {
        url = "https://more.tv/online/russia1_hd";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "good-wheels_ru" = {
      tags = [ "shop" "car" ];
      remote = {
        url = "https://good-wheels.ru";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "best-tyres_ru" = {
      tags = [ "shop" "car" ];
      remote = {
        url = "https://www.best-tyres.ru";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "thinkpad/reddit" = {
      tags = [ "laptop" "thinkpad" "reddit" ];
      remote = {
        url = "https://www.reddit.com/r/Thinkpad";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "musicforprogramming" = {
      tags = [ "music" "programming" ];
      remote = {
        url = "https://musicforprogramming.net/latest/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "crackmes" = {
      tags = [ "reveng" "crackme" "contest" ];
      remote = {
        url = "https://crackmes.one/";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "torrent_files_search" = {
      desc = "torrent + ";
      remote = {
        url = "https://www.yandex.ru/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "?q=torrent+";
      };
    };
    "mongol_russian_dict" = {
      desc = "MN - RU dict";
      remote = {
        url = "https://glosbe.com/ru/mn/";
        browser = appCmdFull config.attributes.browser.default.traits;
        searchSuffix = "";
      };
    };
    "likeedownloader" = {
      tags = [ "download" "likee" ];
      remote = {
        url = "https://likeedownloader.com/en";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "koshkin-dom" = {
      tags = [ "masha" ];
      remote = {
        url = "https://mishka-knizhka.ru/skazki-dlay-detey/russkie-skazochniki/skazki-marshaka/koshkin-dom/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "strana-delfiniya" = {
      tags = [ "masha" ];
      remote = {
        url = "https://rustih.ru/novella-matveeva-strana-delfiniya/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "stihi-mihalkova" = {
      tags = [ "masha" ];
      remote = {
        url = "https://mishka-knizhka.ru/stihi-mihalkova/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "prostokvashino-uspenskiy" = {
      tags = [ "masha" ];
      remote = {
        url = "https://mishka-knizhka.ru/skazki-dlay-detey/russkie-skazochniki/skazki-uspenskogo/djadja-fedor-uspenskij-je-n/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "kotyata" = {
      tags = [ "masha" ];
      remote = {
        url = "https://www.culture.ru/poems/45343/kotyata";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "reestrd-dover" = {
      remote = {
        url = "https://reestrd-dover.ru/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "gitflic" = {
      remote = {
        url = "https://gitflic.ru/project";
        browser = appCmdFull config.attributes.browser.default.traits;
      };
    };
    "school.mos.ru/homeworks" = {
      remote = {
        url = "https://school.mos.ru/diary/homeworks/homeworks/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
    "school.mos.ru/profile" = {
      remote = {
        url = "https://school.mos.ru/diary/account/profile/";
        browser = appCmdFull config.attributes.browser.fallback.traits;
      };
    };
  };
  pim.timetracking.rules = mkArbttBrowserTitleRule [ "Facebook" ] "site:facebook" config.attributes.browser;
}
