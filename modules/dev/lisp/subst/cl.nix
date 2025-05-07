{ config, ... }:

{
  clsDisabled = if config.ide.emacs.completion.snippets.backend != "yasnippet" then ":disabled" else "";
}
