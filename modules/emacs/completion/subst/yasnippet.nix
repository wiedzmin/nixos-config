{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsYasnippetSnippetsPaths = concatStringListsQuoted " "
    config.ide.emacs.completion.yasnippet.snippetsPaths;
}
