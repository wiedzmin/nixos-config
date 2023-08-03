{ config, inputs, pkgs, ... }:

{
  tabnineExecutablePathAdvice = if config.completion.tabnine.useNixpkgsVersion then ''(defun company-tabnine/nix-executable-path () "${pkgs.tabnine}/bin/TabNine")'' else "";
  tabnineExecutablePathPatch = if config.completion.tabnine.useNixpkgsVersion then "(advice-add #'company-tabnine--executable-path :override #'company-tabnine/nix-executable-path)" else "";
}
