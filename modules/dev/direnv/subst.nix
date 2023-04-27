{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  direnvGranularityFile = emacsBoolToString (config.dev.direnv.emacs.granularity == "file");
  direnvGranularityProject = emacsBoolToString (config.dev.direnv.emacs.granularity == "project");
}
