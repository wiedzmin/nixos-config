self: super: {

xkeysnail = super.python3Packages.buildPythonPackage rec {
    pname = "xkeysnail";
    version = "0.1.0";

    src = super.python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "4f20afcbebd533ca691f1c3672db6f27caa9a336556abc2a5799676851942062";
    };

    patches = [ ./patches/xkeysnail/setup-fixed-encoding-issue.patch ];

    propagatedBuildInputs = with self; with python3Packages; [ xlib evdev six ];

    doCheck = false;

    meta = with super.stdenv.lib; {
        description = "Yet another keyboard remapping tool for X environment";
        homepage = https://github.com/mooz/xkeysnail;
        license = licenses.gpl3;
        platforms = platforms.unix;
    };
};

}
