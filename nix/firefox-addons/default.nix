{ pkgs }:
let
  buildFirefoxXpiAddon = { pname, version, addonId, url, sha256, meta, ... }:
    pkgs.stdenv.mkDerivation {
      name = "${pname}-${version}";

      inherit meta;

      src = pkgs.fetchurl { inherit url sha256; };

      preferLocalBuild = true;
      allowSubstitutes = false;

      buildCommand = ''
        dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
        mkdir -p "$dst"
        install -v -m644 "$src" "$dst/${addonId}.xpi"
      '';
    };
  buildFirefoxXpiAddonFromArchPkg = { pname, version, addonId, url, sha256, meta, ... }:
    pkgs.stdenv.mkDerivation {
      name = "${pname}-${version}";

      inherit meta;

      src = pkgs.fetchurl { inherit url sha256; };

      sourceRoot = ".";

      preferLocalBuild = true;
      allowSubstitutes = false;

      installPhase = ''
        dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
        mkdir -p "$dst"
        install -v -m644 "$sourceRoot/usr/lib/firefox/browser/extensions/${addonId}.xpi" "$dst/${addonId}.xpi"
      '';
    };

  packages = with pkgs; import ./addons.nix { inherit buildFirefoxXpiAddon buildFirefoxXpiAddonFromArchPkg stdenv; };
in packages // { inherit buildFirefoxXpiAddon buildFirefoxXpiAddonFromArchPkg; }
