{ config, inputs, ... }:

{
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsTempelSnippetsPath = config.completion.emacs.tempel.snippetsPath;
}
