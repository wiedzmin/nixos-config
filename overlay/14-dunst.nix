self: super: {

dunst = with super; stdenv.mkDerivation rec {
    name = "dunst-${version}";
    version = "1.3.2";

    src = fetchFromGitHub {
        owner = "dunst-project";
        repo = "dunst";
        rev = "v${version}";
        sha256 = "1kqlshaflp306yrjjmc28pghi1y5p24vdx4bxf8i4n9khdawb514";
    };

    nativeBuildInputs = [ perl pkgconfig which systemd makeWrapper ];

    buildInputs = [
        cairo
        dbus
        gdk_pixbuf
        glib
        libnotify
        librsvg
        libxdg_basedir
        pango
        xorg.libX11
        xorg.libXScrnSaver
        xorg.libXinerama
        xorg.libXrandr
        xorg.xproto
    ];

    outputs = [ "out" "man" ];

    makeFlags = [
        "PREFIX=$(out)"
        "VERSION=$(version)"
        "SERVICEDIR_DBUS=$(out)/share/dbus-1/services"
        "SERVICEDIR_SYSTEMD=$(out)/lib/systemd/user"
    ];

    buildFlags = [ "dunstify" ];

    postInstall = ''
        install -Dm755 dunstify $out/bin

        wrapProgram $out/bin/dunst \
            --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
    '';

    meta = with stdenv.lib; {
        description = "Lightweight and customizable notification daemon";
        homepage = https://dunst-project.org/;
        license = licenses.bsd3;
        platforms = platforms.linux;
    };
};

}
