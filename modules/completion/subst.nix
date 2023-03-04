{ config, inputs, ... }:

rec {
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsTempelSnippetsPath = config.completion.emacs.tempel.snippetsPath;
}
