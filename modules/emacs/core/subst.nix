{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  emacsDatadir = config.ide.emacs.core.dataDir;
  fallbackPackageArchives = emacsBoolToString false;
}
