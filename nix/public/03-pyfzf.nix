self: super: {

  pyfzf = super.python3Packages.buildPythonPackage rec {
    pname = "pyfzf";
    version = "0.2.1";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0inbc96m9k4yfn7bxqwxpk9kx00axl8sckql5f7r3c1kpxaymxyq";
    };

    propagatedBuildInputs = with super; [ fzf python3Packages.plumbum ];

    doCheck = false;

    meta = with super.stdenv.lib; {
      description = "A python wrapper for fzf";
      homepage = "https://github.com/nk412/pyfzf";
      license = licenses.mit;
    };
  };
}
