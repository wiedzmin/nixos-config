let
  name = "stgit-${version}";
  version = "0.18";
in
self: super: {

stgit = with super; stdenv.mkDerivation rec {
    inherit name;

    src = fetchFromGitHub {
        owner = "ctmarinas";
        repo = "stgit";
        rev = "v${version}";
        sha256 = "0ydgg744m671nkhg7h4q2z3b9vpbc9914rbc0wcgimqfqsxkxx2y";
    };

    buildInputs = [ python2 git makeWrapper];

    makeFlags = "prefix=$$out";

    postInstall = ''
        mkdir -p "$out/etc/bash_completion.d/"
        ln -s ../../share/stgit/completion/stgit-completion.bash "$out/etc/bash_completion.d/"

        for i in "$out/bin/"*
        do
            wrapProgram $i --prefix PATH ":" "${git}/bin"
            true
        done
    '';

    doCheck = false;
    checkTarget = "test";

    meta = with stdenv.lib; {
        description = "A patch manager implemented on top of Git";
        homepage = http://procode.org/stgit/;
        license = licenses.gpl2;
        maintainers = with maintainers; [ the-kenny ];
        platforms = platforms.unix;
    };
};

}
