{ config, inputs, pkgs, ... }:

{
  tabnineExecutablePathPatch =
    if config.completion.tabnine.useNixpkgsVersion then ''
      (define-advice company-tabnine--executable-path
          (:override (orig-fun &rest args) company-tabnine/nix-executable-path)
        "${pkgs.tabnine}/bin/TabNine")
    '' else "";
}
