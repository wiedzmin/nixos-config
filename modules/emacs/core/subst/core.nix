{ config, inputs, ... }:

{
  emacsDatadir = config.ide.emacs.core.dataDir;
  emacsEtcDir = config.ide.emacs.core.etcDir;
  emacsPasswordMenuPath = inputs.emacs-password-menu;
  emacsMonospacedFillColumn = config.attributes.fonts.monospaced.fillColumn;
}
