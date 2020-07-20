self: super: {

  pystdlib = super.python3Packages.buildPythonPackage rec {
    pname = "pystdlib";
    version = "unstable";

    src = super.fetchFromGitHub {
      owner = "wiedzmin";
      repo = "pystdlib";
      rev = "b981d3b56e0d5170fb6186abf52f3c3248c50fa3";
      sha256 = "1pj3dx7f1975ihll4frwq7w2zljzzxmawr8wqrdcx8izhmqjlm5b";
    };

    propagatedBuildInputs = with super; with python3Packages; [ dmenu-python notify2 pyfzf xlib ];

    doCheck = false;
  };
}
