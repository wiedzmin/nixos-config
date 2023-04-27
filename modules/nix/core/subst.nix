{ config, ... }:

{
  nixLspInitSection =
    if config.ext.nix.core.lsp.enable then ''
      :init
        (require 'lsp-nix)'' else "";
  nixLspHookSection =
    if config.ext.nix.core.lsp.enable then ''
      (nix-mode-hook . lsp-deferred)'' else "";
}
