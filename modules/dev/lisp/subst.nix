{ config, inputs, ... }:

{
  emacsHighlightSexpPath = inputs.emacs-highlight-sexp;
  clsDisabled = if config.ide.emacs.completion.snippets.backend != "yasnippet" then ":disabled" else "";
}
