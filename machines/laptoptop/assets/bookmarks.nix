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
      path = "${wsRoot roots "github"}/wiedzmin/nixos-config";
      url = "https://github.com/wiedzmin/nixos-config/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      jump = true;
      searchSuffix = "search?q=";
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
      path = "${wsRoot roots "github"}/wiedzmin/git-hooks";
      url = "https://github.com/wiedzmin/git-hooks";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      jump = true;
      searchSuffix = "search?q=";
      batchvcs = {
        "${wsRoot roots "github"}/wiedzmin/git-hooks" = {
          checkout = [ "git clone 'https://github.com/wiedzmin/git-hooks.git' 'git-hooks'" ];
        };
      };
    };
    nur-packages = { path = "${wsRoot roots "github"}/wiedzmin/nur-packages"; };
    postgres = { path = "${wsRoot roots "github"}/postgres/postgres"; };
    "staging/sandbox" = { path = homePrefix user "workspace/sandbox/newpkgs"; };
    jwt-io = {
      desc = "JWT online debugger and libraries reference";
      url = "https://jwt.io/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "mastering/vlan" = {
      desc = "VLAN article";
      url = "http://xgu.ru/wiki/VLAN";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "mastering/networking" = {
      desc = "Networking beginner book (online)";
      url = "https://linkmeup.gitbook.io/sdsm/0.-planirovanie";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "toolbox" = mkGithubBookmarkWithMyrepos "wiedzmin" "toolbox" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
      windowRules = [
        {
          class = "Emacs";
          title = "toolbox";
          desktop = "main"; # [ref:desktop_main]
        }
      ];
    };
    "timeshitting" = mkGithubBookmarkWithMyrepos "wiedzmin" "timeshitting" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
      windowRules = [
        {
          class = "Emacs";
          title = "timeshitting";
          desktop = "main"; # [ref:desktop_main]
        }
      ];
    };
    "wmtools" = mkGithubBookmarkWithMyrepos "wiedzmin" "wmtools" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
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
      url = "https://lumosity.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "relax-fm" = {
      desc = "Relax FM radio";
      tags = [ "media" "fm" "radio" ];
      url = "https://relax-fm.ru";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "lightnings" = {
      desc = "Lightning map";
      tags = [ "map" "weather" ];
      url = "https://ru.blitzortung.org/live_dynamic_maps.php";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "marinetraffic" = {
      desc = "Marine vessels map";
      tags = [ "map" "weather" ];
      url = "https://www.marinetraffic.com";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "chillout" = {
      desc = "Music streams"; # note /chillout
      tags = [ "media" "fm" ];
      url = "https://www.di.fm/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "search?q=";
    };
    "lightnings2" = {
      desc = "Lightning map";
      tags = [ "map" "weather" ];
      url = "https://www.lightningmaps.org/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "encodings" = {
      desc = "Encodings translation";
      tags = [ "tools" "online" ];
      url = "https://www.online-decoder.com/ru";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "Ricktube" = {
      desc = "Ricktube";
      tags = [ "media" "video" "russian" ];
      url = "https://ricktube.ru/";
      jump = true;
      searchSuffix = "video?q=";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "youtube" = {
      desc = "Youtube";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/";
      jump = true;
      searchSuffix = "results?search_query=";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "google-photos" = {
      desc = "Google Photos";
      tags = [ "media" "photo" ];
      url = "https://photos.google.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "bitbucket" = {
      desc = "Bitbucket code hosting";
      tags = [ "forge" ];
      url = "https://bitbucket.org";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "digitalocean" = {
      desc = "DigitalOcean account";
      tags = [ "servers" ];
      url = "https://cloud.digitalocean.com/projects/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "linode" = {
      desc = "Linode account";
      tags = [ "servers" ];
      url = "https://cloud.linode.com/linodes";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "crontabs" = {
      desc = "Crontab format helpers";
      tags = [ "tools" "online" ];
      url = "https://crontab.guru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "gdocs" = {
      url = "https://docs.google.com/document/u/0/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "ydocs" = {
      url = "https://docs.yandex.ru";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "gh" = {
      desc = "Github code hosting";
      tags = [ "forge" ];
      url = "https://github.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "search?&type=code&q=";
    };
    "ghpath" = {
      desc = "Github code hosting by path";
      tags = [ "forge" ];
      url = "https://github.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "search?type=code&q=in%3Apath+";
    };
    "ghfflake" = {
      desc = "Github search in 'flake.nix' files";
      tags = [ "forge" ];
      url = "https://github.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "search?q=filename%3Aflake.nix+";
    };
    "ghinpath" = {
      desc = "Github search in file paths";
      tags = [ "forge" ];
      url = "https://github.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "search?q=in%3Apath+";
    };
    "ghw" = {
      desc = "Github personal account";
      tags = [ "forge" ];
      url = "https://github.com/wiedzmin";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "cba" = {
      desc = "Codeberg personal account";
      tags = [ "forge" ];
      url = "https://codeberg.org/alex3rd";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "gmail" = {
      desc = "GMail account";
      url = "https://mail.google.com/mail/u/0/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "repology/projects" = {
      desc = "Repology";
      url = "https://repology.org/projects/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "?search=";
    };
    "repology/maintainers" = {
      desc = "Repology maintainers";
      url = "https://repology.org/maintainers/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "?search=";
    };
    "repology/repos" = {
      desc = "Repology repositories";
      url = "https://repology.org/repositories";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "torrefacto" = {
      desc = "Torrefacto shop";
      url = "https://torrefacto.ru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "gtrans" = {
      desc = "Google translate service";
      tags = [ "dictionary" ];
      url = "https://translate.google.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "teleweb" = {
      desc = "Telegram web interface";
      url = "https://web.telegram.org/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "fb" = {
      enable = false;
      tags = [ "fb" ];
      url = "https://www.facebook.com/";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
      vpn = "proton";
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
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
      url = "https://www.facebook.com/messages/t/";
    };
    "vkontakte" = {
      desc = "VK";
      tags = [ "vk" ];
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
      url = "https://vk.com/";
    };
    "mt" = {
      desc = "Multitran";
      tags = [ "dictionary" ];
      url = "https://www.multitran.com/m.exe?l1=1&l2=2";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "&s=";
    };
    "ventusky" = {
      desc = "Weather map / temperature";
      tags = [ "map" "weather" ];
      url = "https://www.ventusky.com/?l=temperature";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "rust" = {
      desc = "it/rust";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZGJn6Q7c6G7NAaiWD5J__z";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "orgmode" = {
      desc = "emacs/orgmode";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZJBJT4i3KvMbAuIicqwSau";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "emacs" = {
      desc = "emacs";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZM2QXMb8eZ8_I6JhfZctWi";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "it/pm" = {
      desc = "it/management";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZT0PdGw9dp_Ic9jYODxqxB";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "lisp" = {
      desc = "lisp";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGZioLb2SeloXWPkP2PmxM3t";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "var/it" = {
      desc = "it/var";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGa0RDozpS3WxmUcoYSsOmTU";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "techniques" = {
      desc = "ethnic/music/techniques";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGaKdF1Ewgg9IWW96UPpxTFi";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "var" = {
      desc = "var";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGajeWFlOnp19tSDu7XhWFo2";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "python" = {
      desc = "it/python";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGbTK2Y7VPHoc01tbQrZww5h";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "hl" = {
      desc = "it/conf/highload";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGbiXj0rSMDJEuogTY4y8SQL";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "golang" = {
      desc = "it/golang";
      tags = [ "media" "video" ];
      url = "https://www.youtube.com/playlist?list=PLdEMId_A5XGbtFoRo94tXdDBgjwsTh02r";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "andrewtropin/playlists" = {
      desc = "playlists of IT-related videos by Andrew Tropin";
      tags = [ "it" "nix" "guix" "emacs" "video" ];
      url = "https://www.youtube.com/c/TROP1N/playlists";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "ya" = {
      desc = "Yandex";
      url = "https://yandex.ru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      jump = true;
      searchSuffix = "search/?text=";
    };
    "ml24" = {
      enable = false;
      tags = [ "shop" ];
      url = "https://market-land24.ru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "mealty" = {
      tags = [ "shop" ];
      url = "https://mealty.ru/catalog/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "khuur" = {
      tags = [ "music" "culture" ];
      url = "https://stevemorel.info/sharing/morin-huur/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "connsp" = {
      desc = "Internet connection speed";
      tags = [ "networking" "internet" "speed" ];
      url = "https://speedtest24net.ru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "dict" = {
      desc = "Dictionary";
      url = "https://dictionary.reference.com/browse/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "";
    };
    "ghs" = {
      desc = "Github";
      tags = [ "forge" ];
      url = "https://github.com/search?type=Everything&q=";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "";
    };
    "gm" = {
      desc = "Google maps";
      tags = [ "map" ];
      url = "https://maps.google.com/maps?q=";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "";
    };
    "goo" = {
      desc = "Google";
      url = "https://www.google.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "search?num=100&q=";
    };
    "ling" = {
      desc = "Lingvo";
      tags = [ "dictionary" ];
      url = "https://lingvopro.abbyyonline.com/ru/Search/en-ru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "";
    };
    "md" = {
      desc = "My Delicious";
      url = "https://www.delicious.com/__ronin__/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "";
    };
    "scode" = {
      desc = "Searchcode";
      tags = [ "dev" ];
      url = "https://searchcode.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "?q=";
    };
    "warc" = {
      desc = "Web archive";
      tags = [ "history" ];
      url = "https://web.archive.org/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "web/*/";
    };
    "codeforces" = {
      tags = [ "contest" "code" ];
      url = "https://codeforces.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projecteuler" = {
      tags = [ "contest" "code" ];
      url = "https://projecteuler.net/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "hackerrank" = {
      tags = [ "contest" "code" ];
      url = "https://www.hackerrank.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "exercism" = {
      tags = [ "contest" "code" ];
      url = "https://exercism.io/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "leetcode" = {
      tags = [ "contest" "code" ];
      url = "https://leetcode.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "codewars" = {
      tags = [ "contest" "code" ];
      url = "https://www.codewars.com/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/loggerhead" = mkGithubBookmark "wiedzmin" "loggerhead" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    }; # arbtt analog
    "projects/gourmet" = mkGithubBookmarkWithMyrepos "wiedzmin" "gourmet" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "mastering/wiedzmin/cl-study" = mkGithubBookmarkWithMyrepos "wiedzmin" "cl-study" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "mastering/wiedzmin/lisp-koans" = mkGithubBookmarkWithMyrepos "wiedzmin" "lisp-koans" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/go-org" = mkGithubBookmark "niklasfasching" "go-org" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/xgb" = mkGithubBookmark "BurntSushi" "xgb" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/xgbutil" = mkGithubBookmark "BurntSushi" "xgbutil" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/arbtt" = mkGithubBookmark "nomeata" "arbtt" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "projects/reference/code-maat" = mkGithubBookmark "adamtornhill" "code-maat" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
      transient = true;
    };
    "ft-clojure-nix" = {
      tags = [ "java" "clojure" "packaging" "nix" ];
      url = "https://grahamenos.com/ft-clojure-nix.html";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "rofi" = mkGithubBookmark "davatorium" "rofi" roots // {
      browseWith = appCmdFull config.attributes.browser.default.traits;
      transient = true;
    };
    "cppreference" = {
      tags = [ "c++" "cpp" "reference" ];
      url = "https://en.cppreference.com/w/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "paintings" = {
      tags = [ "paint" "play" ];
      url = "https://yandex.ru/search/?lr=213&text=раскраски";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "real mongolia" = {
      tags = [ "mongol" "video" "youtube" ];
      url = "https://www.youtube.com/channel/UCtMZD_UHQxCowHltovfEnvQ";
      browseWith = with config.attributes.browser; maybeDefaultBrowserCmd default fallback;
    };
    "mongoltoli" = {
      tags = [ "mongol" "dictionary" "reference" ];
      url = "https://mongoltoli.mn";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "cider/discord" = {
      tags = [ "emacs" "clojure" "cider" "discord" ];
      url = "https://discord.com/channels/797747718385696768/797750325179973672";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "ddg/bangs" = {
      tags = [ "duckduckgo" "bangs" ];
      url = "https://duckduckgo.com/bang";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    papers-i-love = {
      desc = "Computer science and computer-adjacent papers";
      path = "${wsRoot roots "github"}/fogus/papers-i-love";
      url = "https://github.com/fogus/papers-i-love";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      jump = true;
      searchSuffix = "search?q=";
    };
    agenda = {
      path = homePrefix user "docs/org/roam/agenda.org";
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
      path = homePrefix user "docs/org/roam";
    };
    "telegram/downloads" = {
      desc = "Telegram donloads path";
      tags = [ "telegram" ];
      path = config.attributes.download.path.telegram;
    };
    goodies = {
      path = homePrefix user "docs/org/roam/current_goodies.org";
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
      url = "https://www.ozon.ru/my/orderlist";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "mongol.su/calendar" = {
      tags = [ "mongol" "reference" "calendar" ];
      url = "http://mongol.su/календарь/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "shops/wildberries" = {
      tags = [ "shop" "wildberries" "orders" ];
      url = "https://www.wildberries.ru/lk/myorders/delivery";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "russia1_hd" = {
      tags = [ "media" "tv" ];
      url = "https://more.tv/online/russia1_hd";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "good-wheels_ru" = {
      tags = [ "shop" "car" ];
      url = "https://good-wheels.ru";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "best-tyres_ru" = {
      tags = [ "shop" "car" ];
      url = "https://www.best-tyres.ru";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "thinkpad/reddit" = {
      tags = [ "laptop" "thinkpad" "reddit" ];
      url = "https://www.reddit.com/r/Thinkpad";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "musicforprogramming" = {
      tags = [ "music" "programming" ];
      url = "https://musicforprogramming.net/latest/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "crackmes" = {
      tags = [ "reveng" "crackme" "contest" ];
      url = "https://crackmes.one/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "torrent_files_search" = {
      desc = "torrent + ";
      url = "https://www.yandex.ru/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "?q=torrent+";
    };
    "mongol_russian_dict" = {
      desc = "MN - RU dict";
      url = "https://glosbe.com/ru/mn/";
      browseWith = appCmdFull config.attributes.browser.default.traits;
      searchSuffix = "";
    };
    "likeedownloader" = {
      tags = [ "download" "likee" ];
      url = "https://likeedownloader.com/en";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "koshkin-dom" = {
      tags = [ "masha" ];
      url = "https://mishka-knizhka.ru/skazki-dlay-detey/russkie-skazochniki/skazki-marshaka/koshkin-dom/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "strana-delfiniya" = {
      tags = [ "masha" ];
      url = "https://rustih.ru/novella-matveeva-strana-delfiniya/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "stihi-mihalkova" = {
      tags = [ "masha" ];
      url = "https://mishka-knizhka.ru/stihi-mihalkova/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "prostokvashino-uspenskiy" = {
      tags = [ "masha" ];
      url = "https://mishka-knizhka.ru/skazki-dlay-detey/russkie-skazochniki/skazki-uspenskogo/djadja-fedor-uspenskij-je-n/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "kotyata" = {
      tags = [ "masha" ];
      url = "https://www.culture.ru/poems/45343/kotyata";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "reestrd-dover" = {
      url = "https://reestrd-dover.ru/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "gitflic" = {
      url = "https://gitflic.ru/project";
      browseWith = appCmdFull config.attributes.browser.default.traits;
    };
    "school.mos.ru/homeworks" = {
      url = "https://school.mos.ru/diary/homeworks/homeworks/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "school.mos.ru/profile" = {
      url = "https://school.mos.ru/diary/account/profile/";
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    "old-games-nostalgie" = {
      url = "https://dos.zone/";
      tags = [ "games" ];
      browseWith = appCmdFull config.attributes.browser.fallback.traits;
    };
    xdg-config-path = { path = homePrefix user ".config"; };
  };
  pim.timetracking.rules = mkArbttBrowserTitleRule [ "Facebook" ] "site:facebook" config.attributes.browser;
}
