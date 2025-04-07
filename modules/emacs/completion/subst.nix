{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
in
{
  emacsEtcDir = config.ide.emacs.core.etcDir;
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsTempelSnippetsPath = config.ide.emacs.completion.tempel.snippetsPath;
}
