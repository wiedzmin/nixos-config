self: super: {

img = with super; stdenv.mkDerivation rec {
    name = "img-0.5.6";

    src = fetchurl {
        url = "https://github.com/genuinetools/img/releases/download/v0.5.6/img-linux-amd64";
        sha256 = "1w8vb59f6kla2b4nficjmm758n5gjyavsd26x6bc4qvlb938dmpm";
    };

    phases = [ "installPhase" ];

    dontStrip = true;

    installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/img
        chmod a+x $out/bin/img
    '';

    meta = with stdenv.lib; {
        description = "Standalone, daemon-less, unprivileged Dockerfile and OCI compatible container image builder";
        homepage = "https://github.com/genuinetools/img";
        license = [ pkgs.stdenv.lib.licenses.mit ];
        platforms = with pkgs.stdenv.lib.platforms; unix;
    };
};

}
