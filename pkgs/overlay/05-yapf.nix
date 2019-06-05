self: super: {

yapf = super.python3Packages.buildPythonPackage rec {
  pname = "yapf";
  version = "0.26.0";

  src = super.python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "06w9jgkyk9qn75fpnxv65kbgn992l8cz297yflq6zjjn1blppd7d";
  };

  meta = with super.stdenv.lib; {
    description = "A formatter for Python code.";
    homepage    = "https://github.com/google/yapf";
    license     = licenses.asl20;
    maintainers = with maintainers; [ siddharthist ];
  };
};

}
