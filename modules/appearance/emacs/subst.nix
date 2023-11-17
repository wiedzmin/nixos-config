{ config, ... }:

{
  systemTimeZone = config.time.timeZone;
  currentLineHighlightFaceHlLinePatch = if config.appearance.emacs.currentLineHighlightFace != "" then '':custom-face (hl-line ((t (:background "${config.appearance.emacs.currentLineHighlightFace}"))))'' else "";
  windowDividerWidth = builtins.toString config.appearance.emacs.windowDivider.width;
  wdMinus = if config.appearance.emacs.windowDivider.enable then "" else "-";
}
