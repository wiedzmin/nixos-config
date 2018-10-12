self: super: {

git-quick-stats = with super; stdenv.mkDerivation rec {
    name = "git-quick-stats-${version}";
    version = "1.0.9";

    src = pkgs.fetchFromGitHub {
        owner = "arzzen";
        repo = "git-quick-stats";
        rev = "ac730dbfbc79d137071eee16fe4d5d975cc6a63d";
        sha256 = "0491ksms2m8jij91xxwqb8qbb9hxz4gncbzxncbgij9h2m9k37mq";
    };

    buildInputs = [ pkgs.makeWrapper ];

    dontBuild = true;

    installPhase = ''
        mkdir -p $out/bin
        cp -a git-quick-stats $out/bin/git-quick-stats
    '';

    wrapperPath = with pkgs.stdenv.lib; makeBinPath [
        pkgs.coreutils
        pkgs.git
        pkgs.gnugrep
        pkgs.gnused
    ];

    fixupPhase = ''
        patchShebangs $out/bin

        wrapProgram $out/bin/git-quick-stats --prefix PATH : "${wrapperPath}"
    '';

    meta = {
        description = "Git quick statistics is a simple and efficient way to access various statistics in git repository";
        homepage = https://github.com/arzzen/git-quick-stats;
        license = pkgs.stdenv.lib.licenses.mit;
        platforms = with pkgs.stdenv.lib.platforms; unix;
    };
};

}
