self: super: {

prompt_toolkit = super.python3Packages.buildPythonPackage rec {
    pname = "prompt_toolkit";
    version = "2.0.7";

    src = super.python3Packages.fetchPypi {
        inherit pname version;
        sha256 = "0fgacqk73w7s932vy46pan2yp8rvjmlkag20xvaydh9mhf6h85zx";
    };
    checkPhase = ''
        py.test -k 'not test_pathcompleter_can_expanduser'
    '';

    checkInputs = [ super.python3Packages.pytest ];
    propagatedBuildInputs = with super.python3Packages; [ six wcwidth ];

    meta = {
        description = "Python library for building powerful interactive command lines";
        longDescription = ''
            prompt_toolkit could be a replacement for readline, but it can be
            much more than that. It is cross-platform, everything that you build
            with it should run fine on both Unix and Windows systems. Also ships
            with a nice interactive Python shell (called ptpython) built on top.
        '';
        homepage = https://github.com/jonathanslenders/python-prompt-toolkit;
        license = super.stdenv.lib.licenses.bsd3;
    };
};

mycli = super.python3Packages.buildPythonApplication rec {
  pname = "mycli";
  version = "1.19.0";

  src = super.python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "0x5vzl4vvirqy03fnjwkamhzrqkknlajamwz1rmbnqh4bfmijh9m";
  };

  propagatedBuildInputs = with super.python3Packages; [
    pymysql
    configobj
    sqlparse
    self.prompt_toolkit
    pygments
    click
    pycrypto
    cli-helpers
  ];

  checkInputs = with super.python3Packages; [ pytest mock super.glibcLocales ];

  checkPhase = ''
    export HOME=.
    export LC_ALL="en_US.UTF-8"

    py.test
  '';

  meta = {
    inherit version;
    description = "Command-line interface for MySQL";
    longDescription = ''
      Rich command-line interface for MySQL with auto-completion and
      syntax highlighting.
    '';
    homepage = http://mycli.net;
    license = super.stdenv.lib.licenses.bsd3;
  };
};

pgcli = super.python3Packages.buildPythonApplication rec {
  pname = "pgcli";
  version = "2.0.1";

  src = super.python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "149naq3gp1n922vag7vixs0hd114bpbmbmv70k4kzc1q7jz748l2";
  };

  propagatedBuildInputs = with super.python3Packages; [
    cli-helpers
    click
    configobj
    humanize
    pgspecial
    self.prompt_toolkit
    psycopg2
    pygments
    setproctitle
    sqlparse
    # removed keyring, not really needed in nixos setup
  ];

  checkInputs = with super.python3Packages; [ pytest mock ];

  checkPhase = ''
    py.test
  '';

  meta = with super.stdenv.lib; {
    description = "Command-line interface for PostgreSQL";
    longDescription = ''
      Rich command-line interface for PostgreSQL with auto-completion and
      syntax highlighting.
    '';
    homepage = https://pgcli.com;
    license = licenses.bsd3;
  };
};

litecli = super.python3Packages.buildPythonApplication rec {
  pname = "litecli";
  version = "1.0.0";

  # Python 2 won't have prompt_toolkit 2.x.x
  # See: https://github.com/NixOS/nixpkgs/blob/f49e2ad3657dede09dc998a4a98fd5033fb52243/pkgs/top-level/python-packages.nix#L3408
  disabled = super.python3Packages.isPy27;

  src = super.python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "0s5a6r5q09144cc5169snwis5i2jrh3z2g4mw9wi2fsjxyhgpwq5";
  };

  # fixes tests https://github.com/dbcli/litecli/pull/53
  postPatch = ''
    substituteInPlace litecli/main.py \
      --replace 'except FileNotFoundError:' 'except (FileNotFoundError, OSError):'
  '';

  propagatedBuildInputs = with super.python3Packages; [
    cli-helpers
    click
    configobj
    self.prompt_toolkit
    pygments
    sqlparse
  ];

  checkInputs = with super.python3Packages; [
    pytest
    mock
  ];

  preCheck = ''
    export XDG_CONFIG_HOME=$TMP
    # add missing file
    echo "litecli is awesome!" > tests/test.txt
  '';

  doCheck = false;

  meta = with super.stdenv.lib; {
    description = "Command-line interface for SQLite";
    longDescription = ''
      A command-line client for SQLite databases that has auto-completion and syntax highlighting.
    '';
    homepage = https://litecli.com;
    license = licenses.bsd3;
  };
};

}
