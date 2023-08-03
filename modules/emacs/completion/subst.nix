{ config, inputs, pkgs, ... }:

{
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsTempelSnippetsPath = config.ide.emacs.completion.tempel.snippetsPath;
}
