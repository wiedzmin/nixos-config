{ buildFirefoxXpiAddon, fetchurl, stdenv }:
{
    "cookie-autodelete" = buildFirefoxXpiAddon {
        pname = "cookie-autodelete";
        version = "3.0.1";
        addonId = "CookieAutoDelete@kennydo.com";
        url = "https://addons.mozilla.org/firefox/downloads/file/1209831/cookie_autodelete-3.0.1-an+fx.xpi?src=";
        sha256 = "1d1db3063bdad33a786ed7a171b760cbd801a84c4f56cb005163250a609e8d2a";
        meta = with stdenv.lib; {
            homepage = "https://github.com/mrdokenny/Cookie-AutoDelete";
            description = "Control your cookies! This WebExtension is inspired by Self Destructing Cookies. When a tab closes, any cookies not being used are automatically deleted. Whitelist the ones you trust while deleting the rest. Support for Container Tabs.";
            license = licenses.mit;
            platforms = platforms.all;
        };
    };
    "decentraleyes" = buildFirefoxXpiAddon {
        pname = "decentraleyes";
        version = "2.0.10";
        addonId = "jid1-BoFifL9Vbdl2zQ@jetpack";
        url = "https://addons.mozilla.org/firefox/downloads/file/1705979/decentraleyes-2.0.10-an+fx.xpi?src=";
        sha256 = "68d46f1e1c1e2259b77fd1992d5a5ebb86ca5ec3c519439c011836b9126c55df";
        meta = with stdenv.lib; {
            homepage = "https://decentraleyes.org";
            description = "Protects you against tracking through \"free\", centralized, content delivery. It prevents a lot of requests from reaching networks like Google Hosted Libraries, and serves local files to keep sites from breaking. Complements regular content blockers.";
            license = licenses.mpl20;
            platforms = platforms.all;
        };
    };
    "greasemonkey" = buildFirefoxXpiAddon {
        pname = "greasemonkey";
        version = "4.7";
        addonId = "{e4a8a97b-f2ed-450b-b12d-ee082ba24781}";
        url = "https://addons.mozilla.org/firefox/downloads/file/1058458/greasemonkey-4.7-an+fx.xpi?src=";
        sha256 = "537f187043e362a223155e670b3944d950938f0a7c7f43fd9da9f57fe41f0d6e";
        meta = with stdenv.lib; {
            homepage = "http://www.greasespot.net/";
            description = "Customize the way a web page displays or behaves, by using small bits of JavaScript.";
            license = licenses.mit;
            platforms = platforms.all;
        };
    };
    "https-everywhere" = buildFirefoxXpiAddon {
        pname = "https-everywhere";
        version = "2019.1.31";
        addonId = "https-everywhere@eff.org";
        url = "https://addons.mozilla.org/firefox/downloads/file/1669416/https_everywhere-2019.1.31-an+fx.xpi?src=";
        sha256 = "d8a053e990a41271a770c6eec9fd2134968a4d41eb4acc8708b89c7c6a2e90a6";
        meta = with stdenv.lib; {
            homepage = "https://www.eff.org/https-everywhere";
            description = "Encrypt the web! HTTPS Everywhere is a Firefox extension to protect your communications by enabling HTTPS encryption automatically on sites that are known to support it, even when you type URLs or follow links that omit the https: prefix.";
            platforms = platforms.all;
        };
    };
    "link-cleaner" = buildFirefoxXpiAddon {
        pname = "link-cleaner";
        version = "1.5";
        addonId = "{6d85dea2-0fb4-4de3-9f8c-264bce9a2296}";
        url = "https://addons.mozilla.org/firefox/downloads/file/671858/link_cleaner-1.5-an+fx.xpi?src=";
        sha256 = "1ecec8cbe78b4166fc50da83213219f30575a8c183f7a13aabbff466c71ce560";
        meta = with stdenv.lib; {
            homepage = "https://github.com/idlewan/link_cleaner";
            description = "Clean URLs that are about to be visited:\n- removes utm_* parameters\n- on item pages of aliexpress and amazon, removes tracking parameters\n- skip redirect pages of facebook, steam and reddit";
            license = licenses.gpl3;
            platforms = platforms.all;
        };
    };
    "octotree" = buildFirefoxXpiAddon {
        pname = "octotree";
        version = "2.5.6";
        addonId = "jid1-Om7eJGwA1U8Akg@jetpack";
        url = "https://addons.mozilla.org/firefox/downloads/file/1686911/octotree-2.5.6-fx.xpi?src=";
        sha256 = "8a117c4092b327b4db39aeef39849e47fdccbe1e09a91a82522f40a1b50e3c90";
        meta = with stdenv.lib; {
            homepage = "https://github.com/buunguyen/octotree/";
            description = "Add-on to display GitHub code in tree format";
            license = licenses.mit;
            platforms = platforms.all;
        };
    };
    "privacy-badger" = buildFirefoxXpiAddon {
        pname = "privacy-badger";
        version = "2019.2.19";
        addonId = "jid1-MnnxcxisBPnSXQ@jetpack";
        url = "https://addons.mozilla.org/firefox/downloads/file/1688114/privacy_badger-2019.2.19-an+fx.xpi?src=";
        sha256 = "eebb3c1e71d17ec2e35192aefaa9b0a81441d0f74660d5f1000d226e86af0556";
        meta = with stdenv.lib; {
            homepage = "https://www.eff.org/privacybadger";
            description = "Automatically learns to block invisible trackers.";
            license = licenses.gpl3;
            platforms = platforms.all;
        };
    };
    "reddit-enhancement-suite" = buildFirefoxXpiAddon {
        pname = "reddit-enhancement-suite";
        version = "5.14.5";
        addonId = "jid1-xUfzOsOFlzSOXg@jetpack";
        url = "https://addons.mozilla.org/firefox/downloads/file/1676938/reddit_enhancement_suite-5.14.5-an+fx.xpi?src=";
        sha256 = "239c1954af303cc7b0f0cf2f9ccaa68ccc42cf6d17cf4e3598b4bc75ac59dc85";
        meta = with stdenv.lib; {
            homepage = "https://redditenhancementsuite.com/";
            description = "NOTE: Reddit Enhancement Suite is developed independently, and is not officially endorsed by or affiliated with reddit.\n\nRES is a suite of tools to enhance your reddit browsing experience.";
            license = licenses.gpl3;
            platforms = platforms.all;
        };
    };
    "save-page-we" = buildFirefoxXpiAddon {
        pname = "save-page-we";
        version = "13.3";
        addonId = "savepage-we@DW-dev";
        url = "https://addons.mozilla.org/firefox/downloads/file/1705641/save_page_we-13.3-fx.xpi?src=";
        sha256 = "79c2e9ff8329c651dcb56f309c827005f582b90c15b8749cb53e09686296a1a5";
        meta = with stdenv.lib; {
            description = "Save a complete web page (as curently displayed) as a single HTML file that can be opened in any browser. Choose which items to save. Define the format of the saved filename. Enter user comments.";
            license = licenses.gpl2;
            platforms = platforms.all;
        };
    };
    "stylus" = buildFirefoxXpiAddon {
        pname = "stylus";
        version = "1.5.2";
        addonId = "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}";
        url = "https://addons.mozilla.org/firefox/downloads/file/1176751/stylus-1.5.2-fx.xpi?src=";
        sha256 = "26ea296688e19161f21f935c86687a40679831f197c0b9a0cf8bd323484b9435";
        meta = with stdenv.lib; {
            homepage = "https://add0n.com/stylus.html";
            description = "Redesign your favorite websites with Stylus, an actively developed and community driven userstyles manager. Easily install custom themes from popular online repositories, or create, edit, and manage your own personalized CSS stylesheets.";
            license = licenses.gpl3;
            platforms = platforms.all;
        };
    };
    "swedish-dictionary" = buildFirefoxXpiAddon {
        pname = "swedish-dictionary";
        version = "1.19";
        addonId = "swedish@dictionaries.addons.mozilla.org";
        url = "https://addons.mozilla.org/firefox/downloads/file/1671188/swedish_dictionary-1.19.xpi?src=";
        sha256 = "649ab8b7c7e98e67ce336db47dd7d28b1b57d1db9d065f34957ab67a07376656";
        meta = with stdenv.lib; {
            homepage = "http://www.sfol.se/";
            description = "Swedish spell-check dictionary.";
            license = licenses.lgpl3;
            platforms = platforms.all;
        };
    };
    "ublock-origin" = buildFirefoxXpiAddon {
        pname = "ublock-origin";
        version = "1.18.6";
        addonId = "uBlock0@raymondhill.net";
        url = "https://addons.mozilla.org/firefox/downloads/file/1709472/ublock_origin-1.18.6-an+fx.xpi?src=";
        sha256 = "020911b05c231f0007b166178076652fb571d9486820283296289a472e91dbaf";
        meta = with stdenv.lib; {
             homepage = "https://github.com/gorhill/uBlock#ublock-origin";
             description = "Finally, an efficient blocker. Easy on CPU and memory.";
             license = licenses.gpl3;
             platforms = platforms.all;
        };
    };
    "display-anchors" = buildFirefoxXpiAddon {
        pname = "display-anchors";
        version = "1.3";
        addonId = "display-anchors@robwu.nl";
        url = "https://addons.mozilla.org/firefox/downloads/file/584272/display_anchors-1.3-an+fx.xpi";
        sha256 = "1f761sccxl2wqd174fhzyg36ldkvz062shzkiidj55fi74z19liw";
        meta = with stdenv.lib; {
             homepage = "https://github.com/Rob--W/display-anchors";
             description = ''
                 Display #Anchors

                 Displays anchors for all content in the current web page
                 without breaking the layout.
             '';
             license = licenses.mit;
             platforms = platforms.all;
        };
    };
    "tridactyl" = buildFirefoxXpiAddon rec {
        pname = "tridactyl";
        version = "1.16.1";
        addonId = "tridactyl.vim@cmcaine.co.uk";
        url = "https://addons.mozilla.org/firefox/downloads/file/3027747/tridactyl-${version}-an+fx.xpi";
        sha256 = "11rkvbv1q3qlpjackmlrk82yghb0ly49zd73d6rbc9xl2r02ql0b";
        meta = with stdenv.lib; {
             homepage = "https://github.com/tridactyl/tridactyl";
             description = ''
                 Replace Firefox's control mechanism with one modelled on Vim.

                 This addon is very usable, but is in an early stage of development.
                 We intend to implement the majority of Vimperator's features.
             '';
             license = licenses.asl20;
             platforms = platforms.all;
        };
    };
    "url-in-title" = buildFirefoxXpiAddon {
        pname = "url-in-title";
        version = "1.0";
        addonId = "{d47d18bc-d6ba-4f96-a144-b3016175f3a7}"; # TODO: make variable somehow
        url = "https://addons.mozilla.org/firefox/downloads/file/736244/url_protocolhostnamepath_in_title-1.0-an+fx.xpi";
        sha256 = "1a69ka4044gda6gcf1pvjslhjqgnssh0rgm5bf56azrikkid2x11";
        meta = with stdenv.lib; {
             homepage = "https://github.com/Tormen/url-in-title";
             description = ''URL (protocol, hostname, path) in title'';
             license = licenses.gpl3;
             platforms = platforms.all;
        };
    };
    "midnight-lizard" = buildFirefoxXpiAddon {
        pname = "midnight-lizard";
        version = "10.3.6";
        addonId = "{8fbc7259-8015-4172-9af1-20e1edfbbd3a}"; # TODO: make variable somehow
        url = "https://addons.mozilla.org/firefox/downloads/file/1713409/midnight_lizard-10.3.6-an+fx.xpi";
        sha256 = "1d4rh5d4dx6723z3ldnfrrnx0zzzzz6rcr48jd7gyy79s6pz28c7";
        meta = with stdenv.lib; {
             homepage = "https://midnight-lizard.org/home";
             description = ''Custom color schemes for all websites: night mode, dark themes, blue light filter, screen shader, high contrast, grayscale, etc.'';
             license = licenses.mit;
             platforms = platforms.all;
        };
    };
    "ghosttext" = buildFirefoxXpiAddon {
        pname = "ghosttext";
        version = "17.12.11.910";
        addonId = "ghosttext@bfred.it";
        url = "https://addons.mozilla.org/firefox/downloads/file/807568/ghosttext-17.12.11.910-fx.xpi";
        sha256 = "0rvxh7iq58n6aw9dwaw3w8h51hg8syk50d2pd73k3mflpccpd2pc";
        meta = with stdenv.lib; {
             homepage = "https://midnight-lizard.org/home";
             description = ''Use your text editor to write in your browser. Everything you type in the editor will be instantly updated in the browser.'';
             license = licenses.mit;
             platforms = platforms.all;
        };
    };
    "web_media_controller" = buildFirefoxXpiAddon {
        pname = "web_media_controller";
        version = "0.8.4";
        addonId = "web-media-controller@f1u77y.me";
        url = "https://addons.mozilla.org/firefox/downloads/file/2989653/web_media_controller-0.8.4-fx.xpi";
        sha256 = "10sck4w0llzd3al9rjxgm3ixwmqk25yangjcl56sl058x9n4g13g";
        meta = with stdenv.lib; {
             homepage = "https://github.com/f1u77y/web-media-controller";
             description = ''Allows controlling media player on different sites with Media Player widget on your desktop'';
             license = licenses.unlicense;
             platforms = platforms.unix;
        };
    };
}
