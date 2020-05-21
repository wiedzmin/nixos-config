self: super:
let
  rtpPath = "share/tmux-plugins";

  addRtp = path: rtpFilePath: attrs: derivation:
    derivation // {
      rtp = "${derivation}/${path}/${rtpFilePath}";
    } // {
      overrideAttrs = f: mkDerivation (attrs // f attrs);
    };

  mkDerivation = a@{ pluginName, rtpFilePath ? (builtins.replaceStrings [ "-" ] [ "_" ] pluginName) + ".tmux"
    , namePrefix ? "tmuxplugin-", src, unpackPhase ? "", postPatch ? "", configurePhase ? ":", buildPhase ? ":"
    , addonInfo ? null, preInstall ? "", postInstall ? "", path ? (builtins.parseDrvName pluginName).name
    , dependencies ? [ ], ... }:
    addRtp "${rtpPath}/${path}" rtpFilePath a (super.stdenv.mkDerivation (a // {
      name = namePrefix + pluginName;

      inherit pluginName unpackPhase postPatch configurePhase buildPhase addonInfo preInstall postInstall;

      installPhase = ''
        runHook postPatch

        runHook preInstall

        target=$out/${rtpPath}/${path}
        mkdir -p $out/${rtpPath}
        cp -r . $target
        if [ -n "$addonInfo" ]; then
          echo "$addonInfo" > $target/addon-info.json
        fi

        runHook postInstall
      '';

      dependencies = [ super.bash ] ++ dependencies;
    }));
  paneHistoryDepthLines = 10000;
in rec {

  inherit mkDerivation;

  fzf-tmux-url-with-history = mkDerivation {
    pluginName = "fzf-tmux-url";
    rtpFilePath = "fzf-url.tmux";
    src = super.fetchgit {
      url = "https://github.com/wfxr/tmux-fzf-url";
      rev = "ecd518eec1067234598c01e655b048ff9d06ef2f";
      sha256 = "0png8hdv91y2nivq5vdii2192mb2qcrkwwn69lzxrdnbfa27qrgv";
    };
    # we should have access not only to visible pane's content
    postPatch = ''
      substituteInPlace fzf-url.sh --replace "capture-pane -J -p" "capture-pane -S -${
        builtins.toString paneHistoryDepthLines
      } -J -p"
      substituteInPlace fzf-url.sh --replace "fzf-tmux" "${super.skim}/bin/sk-tmux"
      substituteInPlace fzf-url.sh --replace "--no-preview" ""
    '';
  };
}
