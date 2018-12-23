self: super: {

arion = with super; stdenv.mkDerivation rec {
    name = "arion";

    src = pkgs.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "arion";
        rev = "ebb714c69d03192c1e3a1e42cffae590f6549c63";
        sha256 = "0z5vq2srqyxvw3xhm12d1pgj7aik5kmaqdrbywp8xgq55qz6mk52";
    };

    buildInputs = [ pkgs.makeWrapper ];

    dontBuild = true;

    installPhase = ''
        mkdir -p $out/bin $out/share/arion
        cp -a src/arion $out/bin
        cp -a src/nix $out/share/arion/
        cp -a src/arion-image $out/share/arion/
        substitute src/arion $out/bin/arion --subst-var-by nix_dir $out/share/arion/nix;
        chmod a+x $out/bin/arion
    '';

    wrapperPath = with pkgs.stdenv.lib; makeBinPath [
        pkgs.jq
        pkgs.coreutils
    ];

    fixupPhase = ''
        patchShebangs $out/bin

        wrapProgram $out/bin/arion --prefix PATH : "${wrapperPath}"
    '';

    meta = {
        description = "Run docker-compose without images with Nix";
        homepage = https://github.com/hercules-ci/arion;
        platforms = with pkgs.stdenv.lib.platforms; unix;
    };
};

}
