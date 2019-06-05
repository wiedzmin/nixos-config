let
    mkFlag = pfxTrue: pfxFalse: cond: name: "--${if cond then pfxTrue else pfxFalse}-${name}";
    mkEnable = mkFlag "enable" "disable";
in
self: super: {
    dunst = super.dunst.overrideAttrs (attrs: {
        buildFlags = attrs.buildFlags ++ [ "dunstify" ];
    });
    pinentry = super.pinentry.overrideAttrs (attrs: {
        configureFlags = attrs.configureFlags ++ [ (mkEnable true "pinentry-emacs") ];
    });
    i3lock-color = super.i3lock-color.overrideAttrs (attrs: {
        patches = [ ./patches/i3lock-color/forcefully-reset-keyboard-layout-group-to-0.patch ];
    });
}
