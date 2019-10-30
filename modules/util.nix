{ config, lib, pkgs, ... }:

rec {
    addBuildInputs = pkg: inputs: pkg.overrideAttrs (attrs: {
        buildInputs = attrs.buildInputs ++ inputs;
    });
    withPatches = pkg: patches:
        lib.overrideDerivation pkg (attrs: { inherit patches; });
    writePythonScriptWithPythonPackages = name: packages: text:
      pkgs.python3Packages.buildPythonPackage rec {
        pname = name;
        version = "unstable";
        src = pkgs.writeTextFile {
          name = "${name}.py";
          text = "#!${pkgs.python3}/bin/python3\n${text}";
          executable = true;
        };
        format = "other";
        unpackPhase = "true";
        buildInputs = with pkgs; [ makeWrapper ];
        propagatedBuildInputs = with pkgs; packages;
        buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${name}";
        installPhase = "true";
        postInstall = ''
          chmod a+x $out/bin/${name}
        '';
      };
}
