{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  genTreesitSourceAlist = mappings:
    (builtins.concatStringsSep "\n"
      (lib.mapAttrsToList
        (key: value: ''${mkIndent 4}(${key} . ("${
        if builtins.isList value then builtins.concatStringsSep "\" \"" value else value}"))'')
        mappings));
  genTreesitModeRemapAlist = mappings:
    (builtins.concatStringsSep "\n"
      (lib.mapAttrsToList (key: value: ''${mkIndent 4}(${key} . ${value})'') mappings));
in
{
  emacsDatadir = config.ide.emacs.core.dataDir;
  emacsEtcDir = config.ide.emacs.core.etcDir;
  fallbackPackageArchives = emacsBoolToString false;
  treesitLanguageSourceAlistPatch = ''
    (setq treesit-language-source-alist '(
    ${genTreesitSourceAlist config.ide.emacs.core.treesitter.grammars}))'';
  treesitModeRemapAlistPatch = ''
    (setq major-mode-remap-alist '(
    ${genTreesitModeRemapAlist config.ide.emacs.core.treesitter.modeRemappings}))'';
  treesitFontLockLevelPatch = "(treesit-font-lock-level ${builtins.toString config.ide.emacs.core.treesitter.fontLockLevel})";
  emacsTreesitJumpPath = inputs.emacs-treesit-jump;
  emacsPasswordMenuPath = inputs.emacs-password-menu;
  emacsCombobulatePath = inputs.emacs-combobulate;
}
