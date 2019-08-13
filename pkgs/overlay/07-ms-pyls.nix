self: super: {

ms-pyls = super.stdenv.mkDerivation rec {
    name = "ms-pyls-${version}";
    version = "0.2";

    # Impossible to build anything offline with dotnet
    src = super.fetchurl {
        url = "https://github.com/wiedzmin/python-language-server/releases/download/${version}/python-language-server-${version}.tar.gz";
        sha256 = "1inqab80495z838yrnplp7g3j6vbnvbd46rjbdzcv76qckjpvprr";
    };

    buildInputs = with super; [
        unzip
        makeWrapper
    ];

    propagatedBuildInputs = with super; [
        dotnet-sdk
        sqlite
    ];

    preferLocalBuild = true;

    installPhase = ''
        install -dm 755 "$out/opt/ms-pyls"
        cp -r * "$out/opt/ms-pyls"

        makeWrapper "${super.dotnet-sdk}/bin/dotnet" $out/bin/mspyls \
            --prefix LD_LIBRARY_PATH : "${super.stdenv.lib.makeLibraryPath [
              super.icu
              super.openssl
            ]}" \
            --add-flags "$out/opt/ms-pyls/Microsoft.Python.LanguageServer.dll"
    '';

    meta = with super.stdenv.lib; {
        description = "Microsoft Language Server for Python";
        homepage = https://github.com/Microsoft/python-language-server;
        platforms = platforms.all;
        license = "Apache-2.0";
    };
};

}
