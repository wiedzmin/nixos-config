self: super: {

pazi = super.rustPlatform.buildRustPackage rec {
    pname = "pazi";
    version = "0.2.0";

    src = super.fetchFromGitHub {
        owner = "euank";
        repo = pname;
        rev = "v${version}";
        sha256 = "12z2vyzmyxfq1krbbrjar7c2gvyq1969v16pb2pm7f4g4k24g0c8";
    };

    cargoSha256 = "1w97jvlamxlxkqpim5iyayhbsqvg3rqds2nxq1fk5imj4hbi3681";

    cargoPatches = [ ./patches/pazi/cargo-lock.patch ];

    meta = {
        description = "An autojump \"zap to directory\" helper";
        homepage = https://github.com/euank/pazi;
        license = super.stdenv.lib.licenses.gpl3;
    };
};

}
