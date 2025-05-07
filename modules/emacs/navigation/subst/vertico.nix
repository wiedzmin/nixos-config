{ config, ... }:
with config.ide.emacs;

{
  selectionCandidatesCount = builtins.toString navigation.selection.candidatesCount;
  currentLineHighlightFaceVerticoPatch = if config.appearance.emacs.currentLineHighlightFace != "" then '':custom-face (vertico-current ((t (:background "${config.appearance.emacs.currentLineHighlightFace}"))))'' else "";
}
