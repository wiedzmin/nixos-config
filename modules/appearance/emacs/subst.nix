{ config, inputs, ... }:

rec {
  systemTimeZone = config.time.timeZone;
  emacsLinPath = inputs.emacs-lin;
}
