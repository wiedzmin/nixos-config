self: super: {

  youtube-dl = super.python3Packages.buildPythonPackage rec {
    pname = "youtube-dl";
    version = "2019.04.24";

    src = super.fetchurl {
      url = "https://yt-dl.org/downloads/${version}/${pname}-${version}.tar.gz";
      sha256 = "1kzz3y2q6798mwn20i69imf48kb04gx3rznfl06hb8qv5zxm9gqz";
    };

    nativeBuildInputs = [ super.makeWrapper ];
    buildInputs = [ super.zip ];
    propagatedBuildInputs = [ super.python3Packages.pycryptodome ];

    makeWrapperArgs = with super;
      [ ''--prefix PATH : "${stdenv.lib.makeBinPath [ atomicparsley ffmpeg_4 rtmpdump ]}"'' ];

    postInstall = ''
      mkdir -p $out/share/zsh/site-functions
      cp youtube-dl.zsh $out/share/zsh/site-functions/_youtube-dl
    '';

    # Requires network
    doCheck = false;

    meta = {
      homepage = "https://rg3.github.io/youtube-dl/";
      repositories.git = "https://github.com/rg3/youtube-dl.git";
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

}
