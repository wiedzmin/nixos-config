self: super: {

gtm = with super; stdenv.mkDerivation rec {
    name = "gtm-1.3.5";

    src = fetchurl {
        url = "https://github.com/git-time-metric/gtm/releases/download/v1.3.5/gtm.v1.3.5.linux.tar.gz";
        sha256 = "0hq7npnajikpsbxmix8ai9xgnbg0c66gg2msw7hjj282lg9y8hvv";
    };

    # Work around the "unpacker appears to have produced no directories"
    # case that happens when the archive doesn't have a subdirectory.
    setSourceRoot = "sourceRoot=`pwd`";

    buildInputs = [ git makeWrapper ];

    phases = [ "unpackPhase" "installPhase" "postInstall" ];

    dontStrip = true;

    installPhase = ''
        mkdir -p $out/bin
        cp gtm $out/bin
    '';

    postInstall = ''
        patchelf --set-interpreter \
                 ${stdenv.glibc}/lib/ld-linux-x86-64.so.2 $out/bin/gtm
        wrapProgram "$out/bin/gtm" --prefix PATH ":" "${git}/bin"
    '';

    meta = with stdenv.lib; {
        description = "Simple, seamless, lightweight time tracking for Git";
        homepage = "https://github.com/git-time-metric/gtm";
        license = [ pkgs.stdenv.lib.licenses.mit ];
        platforms = with pkgs.stdenv.lib.platforms; all;
    };
};

}
