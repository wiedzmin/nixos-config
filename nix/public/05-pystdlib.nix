self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "5bb1c74561039adec326b9e328118066a2411ab5";
      sha256 = "0lkfssynwfrnnawrmbbg8cani3vw6q5jdv6fqiv4iiyzvw35z834";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python libtmux notify2 pyfzf xlib ];

    doCheck = false;
  };
}
