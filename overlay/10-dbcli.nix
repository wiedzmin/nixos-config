self: super: {

mycli = super.python3Packages.buildPythonApplication rec {
  pname = "mycli";
  version = "1.19.0";

  src = super.python3Packages.fetchPypi {
    inherit pname version;
    sha256 = "0x5vzl4vvirqy03fnjwkamhzrqkknlajamwz1rmbnqh4bfmijh9m";
  };

  propagatedBuildInputs = with super.python3Packages; [
    pymysql configobj sqlparse prompt_toolkit pygments click pycrypto cli-helpers
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
    cli-helpers click configobj humanize prompt_toolkit psycopg2
    pygments sqlparse pgspecial setproctitle
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

}
