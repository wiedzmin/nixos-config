self: super: {

    flask-caching = super.python3Packages.buildPythonPackage rec {
        pname = "Flask-Caching";
        version = "1.7.2";

        src = super.python3Packages.fetchPypi {
            inherit pname version;
            sha256 = "17jnnmnpdflv120yhsfbnpick06iias6f2hcxmf1mi1nr35kdqjj";
        };

        propagatedBuildInputs = with super.python3Packages; [ flask ];

        checkInputs = with super.python3Packages; [ pytest pytestcov pytest-xprocess ];

        checkPhase = ''true'';

        meta = with super.stdenv.lib; {
            description = "Adds caching support to your Flask application";
            homepage = "https://github.com/sh4nks/flask-caching";
            license = licenses.bsd3;
        };
    };

    flask-common = super.python3Packages.buildPythonPackage rec {
        pname = "Flask-Common";
        version = "0.3.0";

        src = super.python3Packages.fetchPypi {
            inherit pname version;
            sha256 = "13d99f2dbc0a332b8bc4b2cc394d3e48f89672c266868e372cd9d7b433d921a9";
        };

        propagatedBuildInputs = with super.python3Packages; [
            crayons
            flask
            gunicorn
            maya
            meinheld
            self.flask-caching
            whitenoise
        ];

        meta = with super.stdenv.lib; {
            description = "Flask extension with lots of common time-savers";
            homepage = https://github.com/kennethreitz/flask-common;
            license = licenses.asl20; # XXX: setup.py lists BSD but git repo has Apache 2.0 LICENSE
        };
    };

    httpbin = super.python3Packages.buildPythonPackage rec {
        pname = "httpbin";
        version = "0.6.2";

        src = super.python3Packages.fetchPypi {
            inherit pname version;
            sha256 = "0afa0486a76305cac441b5cc80d5d4ccd82b20875da7c5119ecfe616cefef45f";
        };

        patches = [
            # https://github.com/kennethreitz/httpbin/issues/403
            # https://github.com/kennethreitz/flask-common/issues/7
            # https://github.com/evansd/whitenoise/issues/166
            (super.fetchpatch {
                url = "https://github.com/javabrett/httpbin/commit/5735c888e1e51b369fcec41b91670a90535e661e.patch";
                sha256 = "167h8mscdjagml33dyqk8nziiz3dqbggnkl6agsirk5270nl5f7q";
            })
        ];

        propagatedBuildInputs = with super.python3Packages; [
            brotlipy
            flask
            self.flask-common
            flask-limiter
            markupsafe
            decorator
            itsdangerous
            raven
            six
        ];

        # No tests
        doCheck = false;

        meta = with super.stdenv.lib; {
            homepage = https://github.com/kennethreitz/httpbin;
            description = "HTTP Request & Response Service";
            license = licenses.mit;
        };
    };

    pytest-httpbin = super.python3Packages.buildPythonPackage rec {
        pname = "pytest-httpbin";
        version = "1.0.0";

        src = super.python3Packages.fetchPypi {
            inherit pname version;
            sha256 = "0wlvw5qgkax7f0i5ks1562s37h2hdmn5yxnp1rajcc2289zm9knq";
        };

        checkInputs = with super.python3Packages; [ pytest ];

        propagatedBuildInputs = with super.python3Packages; [ self.httpbin six ];

        checkPhase = ''
            py.test
        '';

        # https://github.com/kevin1024/pytest-httpbin/pull/51
        doCheck = false;

        meta = {
            description = "Easily test your HTTP library against a local copy of httpbin.org";
            homepage = https://github.com/kevin1024/pytest-httpbin;
            license = super.stdenv.lib.licenses.mit;
        };
    };

    vcrpy = super.python3Packages.buildPythonPackage rec {
        pname = "vcrpy";
        version = "2.0.1";

        src = super.python3Packages.fetchPypi {
            inherit pname version;
            sha256 = "0kws7l3hci1dvjv01nxw3805q9v2mwldw58bgl8s90wqism69gjp";
        };

        checkInputs = with super.python3Packages; [
            pytest
            self.pytest-httpbin
        ];

        propagatedBuildInputs = with super.python3Packages; [
            pyyaml
            six
            wrapt
            yarl
        ];

        checkPhase = ''
            py.test --ignore=tests/integration -k "not TestVCRConnection"
        '';

        meta = with super.stdenv.lib; {
            description = "Automatically mock your HTTP interactions to simplify and speed up testing";
            homepage = https://github.com/kevin1024/vcrpy;
            license = licenses.mit;
        };
    };

    buku = super.python3Packages.buildPythonApplication rec {
        version = "4.2.2";
        pname = "buku";

        src = super.fetchFromGitHub {
            owner = "jarun";
            repo = "buku";
            rev = "v${version}";
            sha256 = "1wy5i1av1s98yr56ybiq66kv0vg48zci3fp91zfgj04nh2966w1w";
        };

        checkInputs = with super.python3Packages; [
            flake8
            hypothesis
            mypy_extensions
            pylint
            pytest
            pytestcov
            pyyaml
        ];

        propagatedBuildInputs = with super.python3Packages; [
            arrow
            beautifulsoup4
            click
            cryptography
            flask
            flask-api
            flask-bootstrap
            flask-paginate
            flask_wtf
            html5lib
            requests
            self.vcrpy
            urllib3
            werkzeug
        ];

        postPatch = ''
            # Jailbreak problematic dependencies
            sed -i \
              -e "s,'PyYAML.*','PyYAML',g" \
              setup.py
        '';

        preCheck = ''
            # Fixes two tests for wrong encoding
            export PYTHONIOENCODING=utf-8

            # Disables a test which requires internet
            substituteInPlace tests/test_bukuDb.py \
              --replace "@pytest.mark.slowtest" "@unittest.skip('skipping')" \
              --replace "self.assertEqual(shorturl, 'http://tny.im/yt')" "" \
              --replace "self.assertEqual(url, 'https://www.google.com')" ""
        '';

        postInstall = ''
            make install PREFIX=$out

            mkdir -p $out/share/zsh/site-functions $out/share/bash-completion/completions $out/share/fish/vendor_completions.d
            cp auto-completion/zsh/* $out/share/zsh/site-functions
            cp auto-completion/bash/* $out/share/bash-completion/completions
            cp auto-completion/fish/* $out/share/fish/vendor_completions.d
        '';

        meta = with super.stdenv.lib; {
            description = "Private cmdline bookmark manager";
            homepage = https://github.com/jarun/Buku;
            license = licenses.gpl3;
            platforms = platforms.linux;
        };
    };

}
