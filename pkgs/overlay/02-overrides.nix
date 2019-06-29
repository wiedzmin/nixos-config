let
    mkFlag = pfxTrue: pfxFalse: cond: name: "--${if cond then pfxTrue else pfxFalse}-${name}";
    mkEnable = mkFlag "enable" "disable";
in
self: super: {
    dunst = super.dunst.overrideAttrs (attrs: {
        buildFlags = attrs.buildFlags ++ [ "dunstify" ];

        buildInputs = attrs.buildInputs ++ (with super; [
            libxdg_basedir
        ]);
        postInstall = ''
            install -Dm755 dunstify $out/bin

            wrapProgram $out/bin/dunst \
                --set GDK_PIXBUF_MODULE_FILE "$GDK_PIXBUF_MODULE_FILE"
        '';
    });
    pinentry = super.pinentry.overrideAttrs (attrs: {
        configureFlags = attrs.configureFlags ++ [ (mkEnable true "pinentry-emacs") ];
    });
    i3lock-color = super.i3lock-color.overrideAttrs (attrs: {
        patches = [ ./patches/i3lock-color/forcefully-reset-keyboard-layout-group-to-0.patch ];
    });
    vaapiIntel = super.vaapiIntel.overrideAttrs (attrs: {
        enableHybridCodec = true;
    });
}
