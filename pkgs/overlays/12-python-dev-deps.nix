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

  django-js-asset = super.python3Packages.buildPythonPackage rec {
    pname = "django-js-asset";
    version = "1.2.2";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0q3j2rsdb2i7mvncy9z160cghcggvk87q14qnn7jvcp0sa0awqy1";
    };

    doCheck = false;

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/matthiask/django-js-asset/";
      license = licenses.bsdOriginal;
      description = "script tag with additional attributes for django.forms.Media";
    };
  };

  django-choices = super.python3Packages.buildPythonPackage rec {
    pname = "django-choices";
    version = "1.7.0";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "1pzzbhc9l122a3pjai2x6qkimbcpxdr0q5h6gkyvbxzpdjwkc3rw";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/bigjason/django-choices";
      license = licenses.mit;
      description = "Sanity for the django choices functionality.";
    };
  };

  django-debug-toolbar = super.python3Packages.buildPythonPackage rec {
    pname = "django-debug-toolbar";
    version = "2.1";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "1a77jckf2bgz1faidrsc6l5a01xhn7n12xaqlvh4h5hfdjy5gh94";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
      sqlparse
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/jazzband/django-debug-toolbar";
      license = licenses.bsdOriginal;
      description = "A configurable set of panels that display various debug information about the current request/response.";
    };
  };

  django-epiced = super.python3Packages.buildPythonPackage rec {
    pname = "django-epiced";
    version = "0.4.3";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0c481mr8s9aizpg4315iaw2ys8x58pi0qxjvqnznzp9nbpshmvyh";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      markdown
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/belug23/django-epiced";
      license = licenses.bsdOriginal;
      description = "A Django app to add the EpicEditor with easy to use widget.";
    };
  };

  django-mptt = super.python3Packages.buildPythonPackage rec {
    pname = "django-mptt";
    version = "0.10.0";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0pmrykyzrmvf1iba1vwanqnwv56j1dalifd81hpw5dfh3m8c2rf7";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
      self.django-js-asset
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/django-mptt/django-mptt";
      license = licenses.mit;
      description = "Utilities for implementing Modified Preorder Tree Traversal with your Django Models and working with trees of Model instances.";
    };
  };

  django-related-select = super.python3Packages.buildPythonPackage rec {
    pname = "django-related-select";
    version = "0.9.3";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "150rghb84m0qbmbg5ncfw9c0536li8br9p4xxwsldavvlr6kwiv2";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/zachmccormick/django-related-select";
      license = "UNKNOWN";
      description = "Class-based View and django form field for related select boxes";
    };
  };

  drest = super.python3Packages.buildPythonPackage rec {
    pname = "drest";
    version = "0.9.12";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0bjlar9kswp9353rka0jx0q31n6h5m2q1h7bmfx4965r34ysb22x";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      httplib2
    ];

    meta = with super.stdenv.lib; {
      homepage = "http://github.com/datafolklabs/drest/";
      license = licenses.bsdOriginal;
      description = "dRest API Client Library for Python";
    };
  };

  json-rpc = super.python3Packages.buildPythonPackage rec {
    pname = "json-rpc";
    version = "1.12.2";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0a7zf3z3zdx1kksl76b05gknn0ygk3db8xydk6cd67457jrw6wla";
    };

    doCheck = false;

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/pavlov99/json-rpc";
      license = licenses.mit;
      description = "JSON-RPC transport implementation";
    };
  };

  jsonfield = super.python3Packages.buildPythonPackage rec {
    pname = "jsonfield";
    version = "2.0.2";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "0d5qmjja31rgcj524qy8x527fx81dj1cpvys68f3bmnna14cvcdy";
    };

    doCheck = false;

    propagatedBuildInputs = with super.python3Packages; [
      django
    ];

    meta = with super.stdenv.lib; {
      homepage = "https://github.com/dmkoch/django-jsonfield/";
      license = licenses.mit;
      description = "A reusable Django field that allows you to store validated JSON in your model.";
    };
  };

}
