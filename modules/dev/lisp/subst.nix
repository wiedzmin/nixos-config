{ config, inputs, ... }:

{
  emacsHighlightSexpPath = inputs.emacs-highlight-sexp;
  emacsElispTsModePath = inputs.emacs-elisp-ts-mode;
  clsDisabled = if config.ide.emacs.completion.snippets.backend != "yasnippet" then ":disabled" else "";
}
