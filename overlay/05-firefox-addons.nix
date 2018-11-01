self: super:

{
    ff-addons.browserpass-ce = super.stdenv.mkDerivation rec {
        name = "browserpass-ce-${version}";
        version = "2.0.22";

        extid = "browserpass@maximbaz.com";

        src = super.fetchurl {
            url = "https://addons.mozilla.org/firefox/downloads/file/1025311/browserpass_ce-${version}-an+fx-linux.xpi";
            sha256 = "0xhv0ffhj1fg1wvfqwdq054ynibddvan6244gbq0n0skvap5fnc7";
        };

        unpackPhase = ":";

        installPhase = ''
            install -m 444 -D $src "$out/$extid.xpi"
        '';
    };

    ff-addons.display-anchors = super.stdenv.mkDerivation rec {
        name = "display-anchors-${version}";
        version = "1.3";

        extid = "display-anchors@robwu.nl";

        src = super.fetchurl {
            url = "https://addons.mozilla.org/firefox/downloads/file/584272/display_anchors-${version}-an+fx.xpi";
            sha256 = "1f761sccxl2wqd174fhzyg36ldkvz062shzkiidj55fi74z19liw";
        };

        unpackPhase = ":";

        installPhase = ''
            install -m 444 -D $src "$out/$extid.xpi"
        '';
    };

    ff-addons.tridactyl = super.stdenv.mkDerivation rec {
        name = "tridactyl-${version}";
        version = "1.14.0";

        extid = "tridactyl.vim@cmcaine.co.uk";

        src = super.fetchurl {
            url = "https://addons.mozilla.org/firefox/downloads/file/1066979/tridactyl-${version}-an+fx.xpi";
            sha256 = "0lz3bj4yjn5p73mzianjnjciy9842q04v5ywa32hn8aiz5njcn78";
        };

        unpackPhase = ":";

        installPhase = ''
            install -m 444 -D $src "$out/$extid.xpi"
        '';
    };

    ff-addons.url-protocolhostnamepath-in-title = super.stdenv.mkDerivation rec {
        name = "url-protocolhostnamepath-in-title-${version}";
        version = "1.0";

        extid = "{d47d18bc-d6ba-4f96-a144-b3016175f3a7}";

        src = super.fetchurl {
            url = "https://addons.mozilla.org/firefox/downloads/file/736244/url_protocolhostnamepath_in_title-${version}-an+fx.xpi";
            sha256 = "1a69ka4044gda6gcf1pvjslhjqgnssh0rgm5bf56azrikkid2x11";
        };

        unpackPhase = ":";

        installPhase = ''
            install -m 444 -D $src "$out/$extid.xpi"
        '';
    };
}

# in addition, borrow from TOR derivations the method of setting FF prefs programmatically
# ...and set, at first pref for opening windows instead of tabs
