let
  mkFlag = pfxTrue: pfxFalse: cond: name: "--${if cond then pfxTrue else pfxFalse}-${name}";
  mkEnable = mkFlag "enable" "disable";
in
self: super: {

pinentry = super.pinentry.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ (mkEnable true "pinentry-emacs") ];
});

}
