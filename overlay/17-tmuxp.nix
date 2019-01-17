self: super: {

tmuxp = with super; pythonPackages.buildPythonApplication rec {
    pname = "tmuxp";
    version = "1.4.2";

    src = pythonPackages.fetchPypi {
        inherit pname version;
        sha256 = "087icp1n1qdf53f1314g5biz16sigrnpqr835xqlr6vj85imm2dm";
    };

    postPatch = ''
        sed -i 's/==.*$//' requirements/base.txt requirements/test.txt
    '';

    checkInputs = with pythonPackages; [
        pytest
        pytest-rerunfailures
    ];

    # No tests in archive
    doCheck = false;

    propagatedBuildInputs = with pythonPackages; [
        self.click colorama self.kaptan libtmux
    ];

    meta = with stdenv.lib; {
        description = "Manage tmux workspaces from JSON and YAML";
        homepage = http://tmuxp.readthedocs.io;
        license = licenses.bsd3;
        platforms = platforms.linux;
    };
};

}
