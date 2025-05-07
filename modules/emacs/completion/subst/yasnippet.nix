{ config, inputs, ... }:

{
  emacsEtcDir = config.ide.emacs.core.etcDir;
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
}
