self: super: {

tdesktop = super.tdesktop.overrideAttrs (attrs: {
    patches = [
        (builtins.fetchurl {
             url = "https://raw.githubusercontent.com/msva/mva-overlay/master/net-im/telegram-desktop/files/patches/1.5.6/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch";
             sha256 = "95800293734d894c65059421d7947b3666e3cbe73ce0bd80d357b2c9ebf5b2e5";
             })
    ] ++ attrs.patches;
});

}
