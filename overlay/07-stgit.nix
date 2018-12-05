let
  name = "stgit-${version}";
  version = "0.19";
in
self: super: {

stgit = with super; stdenv.mkDerivation rec {
    inherit name;

    src = fetchFromGitHub {
        owner = "ctmarinas";
        repo = "stgit";
        rev = "v${version}";
        sha256 = "1dzl6cnyzwbzysp82x7w1yc03g25kwan3h0zpnzhhfhg6c904sis";
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
