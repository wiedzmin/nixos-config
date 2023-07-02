{ config, pkgs, ... }:

{
  nixLspInitSection =
    if config.ext.nix.core.lsp.enable then ''
      :init
        (require 'lsp-nix)'' else "";
  nixLspHookSection =
    if config.ext.nix.core.lsp.enable then ''
      (nix-mode-hook . lsp-deferred)'' else "";
  nixLspNixdSection =
    if config.ext.nix.core.lsp.server == "nixd" then ''
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection "${pkgs.nixd}/bin/nixd")
                        :major-modes '(nix-mode)
                        :server-id 'nixd))
    '' else "";
}
