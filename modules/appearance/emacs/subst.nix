{ config, ... }:

{
  systemTimeZone = config.time.timeZone;
  currentLineHighlightFaceHlLinePatch = if config.appearance.emacs.currentLineHighlightFace != "" then '':custom-face (hl-line ((t (:background "${config.appearance.emacs.currentLineHighlightFace}"))))'' else "";
}
