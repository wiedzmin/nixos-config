{ config, inputs, pkgs, ... }:

{
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsTempelSnippetsPath = config.completion.emacs.tempel.snippetsPath;
  tabnineDisabled = if config.completion.tabnine.enable then "" else ":disabled";
  tabnineExecutablePathAdvice = if config.completion.tabnine.useNixpkgsVersion then ''(defun company-tabnine/nix-executable-path () "${pkgs.tabnine}/bin/TabNine")'' else "";
  tabnineExecutablePathPatch = if config.completion.tabnine.useNixpkgsVersion then "(advice-add #'company-tabnine--executable-path :override #'company-tabnine/nix-executable-path)" else "";
}
