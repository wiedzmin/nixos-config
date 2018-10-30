self: super: {

telega-server = with super; stdenv.mkDerivation rec {
    name = "telega-server";

    src = pkgs.fetchFromGitHub {
        owner = "zevlg";
        repo = "telega.el";
        rev = "bb9bccd1028ec6b90f4426e303ef4899eba9c4e2";
        sha256 = "10wkz9h871bm6r1j549pabr0n8qhlkkiw5i7hmfb9xwwdjq2pk20";
    };

    buildInputs = [ pkgs.tdlib ];

    buildPhase = ''
        cd server && gcc -I/usr/local/include -Wall -g -pthread -o telega-server telega-server.c telega-dat.c -ltdjson
    '';
    installPhase = ''
        mkdir -p $out/bin
        cp telega-server $out/bin
    '';

    meta = with stdenv.lib; {
        description = "Telega.el server part";
        homepage = "https://github.com/zevlg/telega.el";
        license = [ pkgs.stdenv.lib.licenses.gpl3 ];
        platforms = with pkgs.stdenv.lib.platforms; linux;
    };
};

}
