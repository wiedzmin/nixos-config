self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "20c43d181d97b9503669eab5d74fd61fa1b48c98";
      sha256 = "0vzd91r2zxznh7y2mcsa5pl84d3r3jgps4aav743187pcn0bp754";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python libtmux notify2 pyfzf xlib ];

    doCheck = false;
  };
}
