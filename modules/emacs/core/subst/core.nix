{ config, inputs, ... }:

{
  emacsDatadir = config.ide.emacs.core.dataDir;
  emacsEtcDir = config.ide.emacs.core.etcDir;
  emacsMonospacedFillColumn = config.attributes.fonts.monospaced.fillColumn;
}
