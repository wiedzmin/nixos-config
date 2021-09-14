{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

rec {
  emacsDatadir = config.ide.emacs.core.dataDir;
  fallbackPackageArchives = emacsBoolToString false;
}
