{ config, lib, pkgs, ... }:

rec {
  rnixInitSection = if config.ext.nix.core.lsp.enable then ''
    :init
      (require 'lsp-nix)'' else "";
  rnixHookSection = if config.ext.nix.core.lsp.enable then ''
    (nix-mode-hook . lsp-deferred)'' else "";
}
