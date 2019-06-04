self: super: {

shyaml = with super; stdenv.mkDerivation rec {
    name = "shyaml";

    src = pkgs.fetchFromGitHub {
        owner = "luodongseu";
        repo = "shyaml";
        rev = "14939f609931bcff5077e4467e1936a65b030482";
        sha256 = "1h0zc3l0p76hnmif2afasamq2qyna1kcvrl9mvnbgpgaf2xz77n9";
    };

    buildInputs = [ pkgs.makeWrapper ];

    dontBuild = true;

    installPhase = ''
        mkdir -p $out/bin
        cp -a bin/shyaml.sh $out/bin/shyaml
        chmod a+x $out/bin/shyaml
    '';

    wrapperPath = with pkgs.stdenv.lib; makeBinPath [
        pkgs.coreutils
        pkgs.gawk
        pkgs.gnugrep
        pkgs.gnused
    ];

    fixupPhase = ''
        patchShebangs $out/bin

        wrapProgram $out/bin/shyaml --prefix PATH : "${wrapperPath}"
    '';

    meta = {
        description = "A yaml parser tool for bash script use";
        homepage = https://github.com/luodongseu/shyaml;
        platforms = with pkgs.stdenv.lib.platforms; unix;
    };
};

}
