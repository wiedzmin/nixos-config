{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

rec {
  emacsDatadir = config.ide.emacs.core.dataDir;
  fallbackPackageArchives = emacsBoolToString false;
}
