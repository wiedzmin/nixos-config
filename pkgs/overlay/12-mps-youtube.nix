self: super: {

  youtube-dl = super.python3Packages.buildPythonPackage rec {
    pname = "youtube-dl";
    version = "2019.06.27";

    src = super.fetchFromGitHub {
      owner = "ytdl-org";
      repo = "youtube-dl";
      rev = "f5629946608861097b6ce5095efb9a9e8ac7f056";
      sha256 = "17pch9i49xcgp9522gm8jn1i8r53k9gqqii78ayswa37qcx8wmf1";
    };

    nativeBuildInputs = [ super.makeWrapper ];
    buildInputs = [ super.zip ];
    propagatedBuildInputs = [ super.python3Packages.pycryptodome ];

    makeWrapperArgs = with super; [ ''--prefix PATH : "${stdenv.lib.makeBinPath [ atomicparsley ffmpeg_4 rtmpdump ]}"'' ];

    postInstall = ''
      mkdir -p $out/share/zsh/site-functions
      cp youtube-dl.plugin.zsh $out/share/zsh/site-functions/_youtube-dl
    '';

    # Requires network
    doCheck = false;

    meta = {
      homepage = https://rg3.github.io/youtube-dl/;
      repositories.git = https://github.com/rg3/youtube-dl.git;
      description = "Command-line tool to download videos from YouTube.com and other sites";
      longDescription = ''
        youtube-dl is a small, Python-based command-line program
        to download videos from YouTube.com and a few more sites.
        youtube-dl is released to the public domain, which means
        you can modify it, redistribute it or use it however you like.
      '';
      license = super.stdenv.lib.licenses.publicDomain;
      platforms = with super.stdenv.lib.platforms; linux ++ darwin;
    };
  };

  pafy = super.python3Packages.buildPythonPackage rec {
    pname = "pafy";
    version = "0.5.4";

    src = super.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "e842dc589a339a870b5869cc3802f2e95824edf347f65128223cd5ebdff21024";
    };

    # No tests included in archive
    doCheck = false;

    propagatedBuildInputs = [ self.youtube-dl ];

    meta = with super.stdenv.lib; {
      description = "A library to download YouTube content and retrieve metadata";
      homepage = http://np1.github.io/pafy/;
      license = licenses.lgpl3Plus;
    };
  };

  mps-youtube = super.python3Packages.buildPythonPackage rec {
    name = "mps-youtube-${version}";
    version = "0.2.8";

    src = super.fetchFromGitHub {
      owner = "mps-youtube";
      repo = "mps-youtube";
      rev = "v${version}";
      sha256 = "1w1jhw9rg3dx7vp97cwrk5fymipkcy2wrbl1jaa38ivcjhqg596y";
    };

    propagatedBuildInputs = [ self.pafy ];

    # disabled due to error in loading unittest
    # don't know how to make test from: <mps_youtube. ...>
    doCheck = false;

    # before check create a directory and redirect XDG_CONFIG_HOME to it
    preCheck = ''
      mkdir -p check-phase
      export XDG_CONFIG_HOME=$(pwd)/check-phase
    '';

    meta = with super.stdenv.lib; {
      description = "Terminal based YouTube player and downloader";
      homepage = https://github.com/np1/mps-youtube;
      license = licenses.gpl3;
    };
  };

}
