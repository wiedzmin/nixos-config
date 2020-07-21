self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "a99dc08a7ca853c4ff526bad85ac0be1de1b826f";
      sha256 = "0j6739casllgfydc8z8qc5jy9njcr7h93rv11jsb2lf6c3m2dp3z";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python notify2 pyfzf xlib ];

    doCheck = false;
  };
}
