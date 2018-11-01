self: super: {

git-wip = with super; stdenv.mkDerivation rec {
    name = "git-wip";

    src = pkgs.fetchFromGitHub {
        owner = "bartman";
        repo = "git-wip";
        rev = "dd89ba0cb1082d54778ddcfb11898d64ecc53dd9";
        sha256 = "02ywrq3574zys7zd9x5wzfs9xn05yrppckjf9n1172gvqswf388r";
    };

    buildInputs = [ pkgs.makeWrapper ];

    dontBuild = true;

    installPhase = ''
        mkdir -p $out/bin
        cp -a git-wip $out/bin/git-wip
    '';

    wrapperPath = with pkgs.stdenv.lib; makeBinPath [
        # pkgs.coreutils
        pkgs.git
        # pkgs.gnugrep
        # pkgs.gnused
    ];

    fixupPhase = ''
        patchShebangs $out/bin

        wrapProgram $out/bin/git-wip --prefix PATH : "${wrapperPath}"
    '';

    meta = {
        description = "Help track git Work In Progress branches";
        homepage = https://github.com/bartman/git-wip;
        platforms = with pkgs.stdenv.lib.platforms; unix;
    };
};

}
