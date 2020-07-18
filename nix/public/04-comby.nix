self: super: {

  comby = super.stdenv.mkDerivation rec {
    pname = "comby";
    version = "0.16.0";

    src = super.fetchurl {
      url = "https://github.com/comby-tools/comby/releases/download/${version}/comby-${version}-x86_64-linux.tar.gz";
      sha256 = "13flbm1h44w8rkvq80sybv0hw2i9s5i2w21q310h6ywa5mf8dgnk";
    };

    # The tarball is just the prebuilt binary, in the archive root.
    sourceRoot = ".";
    dontBuild = true;
    dontConfigure = true;

    nativeBuildInputs = with super; [ autoPatchelfHook ];
    buildInputs = with super; [ pcre.out pkgconfig sqlite.out zlib.out ];

    binary = "comby-${version}-x86_64-linux";
    binary_out = "comby";
    installPhase = ''
      #echo INSTALLING.
      mkdir -p $out/bin
      mkdir -p $out/lib
      cp -r ${super.pcre.out}/lib/* $out/lib
      ln -s $out/lib/libpcre.so.1 $out/lib/libpcre.so.3
      cp -r ${super.sqlite.out}/lib/* $out/lib
      cp -r ${super.zlib.out}/lib/* $out/lib
      mv ${binary} $out/bin/${binary_out}
      chmod a+x $out/bin/${binary_out}
      #ls -la $out/bin
      #echo INSTALLING DONE.
    '';

    meta = with super.stdenv.lib; {
      description = "A tool for changing code across many languages";
      homepage = "https://comby.dev/";
      # license = licenses.gpl;
      platforms = [ "x86_64-linux" ];
    };
  };
}
