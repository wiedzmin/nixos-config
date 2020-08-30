self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "b4f95d45743196c9801fa4ffa8f4273cd7b2141e";
      sha256 = "01lkad2dz4msn4y1qpwapdnww9gjn7f08wgab4f03ggha7sxhhql";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python libtmux notify2 pyfzf xlib ];

    doCheck = false;
  };
}
