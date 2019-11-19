self: super: {

  django-jenkins = super.python3Packages.buildPythonPackage rec {
    pname = "django-jenkins";
    version = "0.110.0";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0mhrnki73sk705aldj4k1ssl13qw6j2fbz41wxvxr41w6a3i98d8";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
    ];

    meta = with super.stdenv.lib; {
      homepage = "http://github.com/kmmbvnr/django-jenkins";
      license = licenses.lgpl2;
      description = "Plug and play continuous integration with django and jenkins";
    };
  };

  django-jwt-auth = super.python3Packages.buildPythonPackage rec {
    pname = "django-jwt-auth";
    version = "0.0.2";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0cj5n0zkggj930zzrc6k268bjxnyvkv7vzbi2gimjfdqgy90sl0l";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      pyjwt
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/jpadilla/django-jwt-auth";
      license = licenses.mit;
      description = "JSON Web Token based authentication for Django";
    };
  };

  django-modern-rpc = super.python3Packages.buildPythonPackage rec {
    pname = "django-modern-rpc";
    version = "0.11.1";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "1wcb3rfsjlg59w9lfnndrz5bif0wnka884pljpj60klz2gjbzw0r";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/alorence/django-modern-rpc";
      license = licenses.mit;
      description = "Simple and modern JSON-RPC and XML-RPC server implementation for Django";
    };
  };

  apply-defaults = super.python3Packages.buildPythonPackage rec {
    name = "apply-defaults-0.1.4";

    src = super.fetchurl {
      url = "https://files.pythonhosted.org/packages/f6/da/badf3e4dbefdcfd911c07c542782e7f31fcd232a7d081ea78261561c73e5/apply_defaults-0.1.4.tar.gz";
      sha256 = "1ce26326a61d8773d38a9726a345c6525a91a6120d7333af79ad792dacb6246c";
    };

    doCheck = false;

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/bcb/apply_defaults";
      license = "UNKNOWN";
      description = "Apply values to optional params";
    };
  };

  click6 = super.python3Packages.buildPythonPackage rec {
    pname = "click";
    version = "6.7";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "02qkfpykbq35id8glfgwc38yc430427yd05z1wc5cnld8zgicmgi";
    };

    patches = with super; stdenv.lib.optional (stdenv.lib.versionAtLeast version "6.7") (substituteAll {
      src = ./patches/click6/fix-paths.patch;
      locale = "${super.locale}/bin/locale";
    });

    buildInputs = with super.python3Packages; [ pytest ];

    checkPhase = ''
      py.test tests
    '';

    # https://github.com/pallets/click/issues/823
    doCheck = false;

    meta = with super.stdenv.lib; {
      homepage = http://click.pocoo.org/;
      description = "Create beautiful command line interfaces in Python";
      longDescription = ''
        A Python package for creating beautiful command line interfaces in a
        composable way, with as little code as necessary.
      '';
      license = licenses.bsd3;
    };
  };

  jsonrpcclient = super.python3Packages.buildPythonPackage rec {
    pname = "jsonrpcclient";
    version = "3.3.4";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "1a8b0p8y6gbqqy52lzwkmkaq1bxlwfm9qhs8nsa9zbvkkd060265";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      self.apply-defaults
      self.click6
      jsonschema
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/bcb/jsonrpcclient";
      license = licenses.mit;
      description = "Send JSON-RPC requests";
    };
  };

  python-dateutil = super.python3Packages.buildPythonPackage rec {
    pname = "python-dateutil";
    version = "2.8.1";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0g42w7k5007iv9dam6gnja2ry8ydwirh99mgdll35s12pyfzxsvk";
    };

    doCheck = false;

    buildInputs = with super.python3Packages; [
      setuptools
      setuptools_scm
      wheel
    ];

    propagatedBuildInputs = with super.python3Packages; [
      six
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://dateutil.readthedocs.io";
      license = licenses.bsdOriginal;
      description = "Extensions to the standard Python datetime module";
    };
  };

  social-auth-core = super.python3Packages.buildPythonPackage rec {
    pname = "social-auth-core";
    version = "3.2.0";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0n1v3rzll0r89s62shvw6n13d1p1pd1abvb8i4ayncm591jnc843";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      defusedxml
      oauthlib
      pyjwt
      python3-openid
      requests
      requests_oauthlib
      six
    ];
    meta = with super.stdenv.lib; {
      homepage = "https://github.com/python-social-auth/social-core";
      license = licenses.bsdOriginal;
      description = "Python social authentication made simple.";
    };
  };

  python-social-auth = super.python3Packages.buildPythonPackage rec {
    pname = "python-social-auth";
    version = "0.3.6";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0x1ffbv1g95ylcgz0fh4dsbs2w73xc9qm4slbkhswd39yw6j51k9";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      self.social-auth-core
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/omab/python-social-auth";
      license = licenses.bsdOriginal;
      description = "Python social authentication made simple.";
    };
  };

  social-auth-app-django = super.python3Packages.buildPythonPackage rec {
    pname = "social-auth-app-django";
    version = "3.1.0";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0y4phjpipyajis70fcr8l4jfcnbgs927pmcpa1aclwcy5n6d23bd";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      six
      self.social-auth-core
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/python-social-auth/social-app-django";
      license = licenses.bsdOriginal;
      description = "Python Social Authentication, Django integration.";
    };
  };

}
