{ config, inputs, ... }:

{
  emacsHighlightSexpPath = inputs.emacs-highlight-sexp;
  clsDisabled = if config.completion.emacs.snippets.backend != "yasnippet" then ":disabled" else "";
}
