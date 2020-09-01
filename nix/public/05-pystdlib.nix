self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "cac555a66c5bf01e165ec19b45ad24bb9b86f341";
      sha256 = "12ixf5mkx3g01mdhk66v0kpn26x36i62bplxbaw2v94aqwfbdpfm";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python libtmux notify2 pyfzf xlib ];

    doCheck = false;
  };
}
