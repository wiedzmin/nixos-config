self: super: {

  ntangle = with super; stdenv.mkDerivation rec {
    pname = "ntangle";
    version = "0.6.3";

    src = fetchurl {
      # TODO: check available platforms
      url = "https://github.com/OrgTangle/ntangle/releases/download/v${version}/ntangle-v${version}.Linux_64bit_musl.tar.xz";
      sha256 = "0qcqbal9fsgq28jxn0gbpxxxw010cbs9jvnzyqagd1s7cyjh40ih";
    };

    # Work around the "unpacker appears to have produced no directories"
    # case that happens when the archive doesn't have a subdirectory.
    setSourceRoot = "sourceRoot=`pwd`";

    phases = [ "unpackPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp ntangle $out/bin
    '';

    meta = with stdenv.lib; {
      description = "Command-line utility for Tangling of Org documents — programmed in Nim";
      homepage = "https://github.com/OrgTangle/ntangle";
      license = licenses.mit;
      platforms = platforms.linux;
    };
  };

}
