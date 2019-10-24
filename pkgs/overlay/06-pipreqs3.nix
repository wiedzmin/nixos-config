self: super: {

  pipreqs3 = super.python3Packages.buildPythonApplication rec {
    pname = "pipreqs";
    version = "0.4.9";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "cec6eecc4685967b27eb386037565a737d036045f525b9eb314631a68d60e4bc";
    };

    patchPhase = ''
      substituteInPlace setup.py \
        --replace "pipreqs=pipreqs.pipreqs:main" "pipreqs3=pipreqs.pipreqs:main"
    '';

    propagatedBuildInputs = with super.python3Packages; [ yarg docopt ];

    # Tests requires network access. Works fine without sandboxing
    doCheck = false;

    meta = with super.stdenv.lib; {
      description = "Generate requirements.txt file for any project based on imports";
      homepage = https://github.com/bndr/pipreqs;
      license = licenses.asl20;
      maintainers = with maintainers; [ psyanticy ];
    };
  };
}
