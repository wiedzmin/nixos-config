p: c:

with p; builtins.mapAttrs (name: value:
python3Packages.buildPythonPackage rec {
    pname = name;
    version = "unstable";
    src = value;
    format = "other";
    unpackPhase = "true";
    buildInputs = [ makeWrapper ];
    propagatedBuildInputs = [
        python3Full
        python3Packages.papis-python-rofi
        python3Packages.GitPython
    ];
    buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${name}";
    installPhase = "true";
}) {
    pkgsctl = ./pkgsctl;
    confctl = ./confctl;
}
