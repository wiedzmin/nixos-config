self: super: {

kaptan = super.pythonPackages.buildPythonPackage rec {
    pname = "kaptan";
    version = "0.5.8";

    src = super.pythonPackages.fetchPypi {
        inherit pname version;
        sha256 = "1b8r86yyvdvyxd6f10mhkl6cr2jhxm80jjqr4zch96w9hs9rh5vq";
    };

    propagatedBuildInputs = [ super.pythonPackages.pyyaml ];

    # doCheck = false;

    meta = with super.stdenv.lib; {
        description = "Configuration manager for python applications";
        homepage = https://emre.github.io/kaptan/;
        license = licenses.bsd3;
        platforms = platforms.linux;
    };
};

}
