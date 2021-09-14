{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

rec {
  direnvGranularityFile = emacsBoolToString (config.dev.direnv.emacs.granularity == "file");
  direnvGranularityProject = emacsBoolToString (config.dev.direnv.emacs.granularity == "project");
}
