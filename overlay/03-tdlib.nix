self: super: {

tdlib = with super; stdenv.mkDerivation rec {
    version = "2018-03-20";
    name = "tdlib-${version}";

    src = pkgs.fetchFromGitHub {
        owner = "tdlib";
        repo = "td";
        rev = "101aa73f132d5124d8da45bae11a29c423420072";
        sha256 = "1cd56jfixmfiss4cjxwg7r2fkbvzlsmj1kc4w6lh9ha329w1jccd";
    };

    buildInputs = [ pkgs.gperf pkgs.openssl pkgs.readline pkgs.zlib ];
    nativeBuildInputs = [ pkgs.cmake ];

    meta = with stdenv.lib; {
        description = "Cross-platform library for building Telegram clients";
        homepage = "https://core.telegram.org/tdlib/";
        license = [ pkgs.stdenv.lib.licenses.boost ];
        platforms = with pkgs.stdenv.lib.platforms; linux;
    };
};

}
