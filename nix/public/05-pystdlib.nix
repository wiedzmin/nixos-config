self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "6028cf4ba0b8be0ec126003968ff39657612e7c1";
      sha256 = "0mqv08kjpdmrp4nx104qq425crfhqal9s4qbzf9yx2qmgs2i2q89";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python libtmux notify2 pyfzf xlib ];

    doCheck = false;
  };
}
