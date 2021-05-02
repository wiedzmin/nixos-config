{ config, inputs, lib, pkgs, ... }:
with import ./util.nix { inherit config inputs lib pkgs; };
with import ./wmutil.nix { inherit config inputs lib pkgs; };

let
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  configHome = hm.xdg.configHome;
in
rec {
  autorandrProfiles = homePrefix ".config/autorandr";
  bashExecutable = "/run/current-system/sw/bin/bash";
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  defaultBrowser = config.attributes.browser.default.cmd;
  fallbackBrowser = config.attributes.browser.fallback.cmd;
  defaultContainerShell = config.ext.virtualization.docker.core.defaultContainerShell;
  defaultVTCmd = lib.concatStringsSep " " config.attributes.defaultVTCommand;
  deftPath = homePrefix "docs/deft";
  direnvGranularityProject = emacsBoolToString (config.dev.direnv.emacs.granularity == "project");
  direnvGranularityFile = emacsBoolToString (config.dev.direnv.emacs.granularity == "file");
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  globalWorkspaceRoot = homePrefix config.navigation.bookmarks.workspaces.globalRoot;
  emacsBrowserGenericProgram = "${pkgs.xdg_utils}/bin/xdg-open";
  emacsCustomFile = homePrefix ".emacs.d/customizations.el";
  emacsDatadir = config.ide.emacs.core.dataDir;
  emacsServerSocketPath = "/run/user/${mainUserID}/emacs/server";
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsObGoPath = inputs.emacs-ob-go;
  emacsOrgAppearPath = inputs.emacs-org-appear;
  emacsBruhPath = inputs.emacs-bruh;
  emacsBookmarkViewPath = inputs.emacs-bookmark-view;
  emacsConsultLsp = inputs.emacs-consult-lsp;
  emacsConsultProjectilePath = inputs.emacs-consult-projectile;
  fallbackPackageArchives = emacsBoolToString false;
  wmFontDmenu = config.wmCommon.fonts.dmenu;
  keybindingsCachePath = config.wmCommon.keybindingsCachePath;
  lockScreenCommand = config.workstation.lockscreen.command.lock;
  lspPythonMsExecutable = "${pkgs.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths =
    builtins.concatStringsSep " " (lib.forEach config.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
  mainUserName = config.attributes.mainUser.name;
  mainUserID = config.attributes.mainUser.ID;
  mcCmd = "${pkgs.mc}/bin/mc";
  nmcliBinary = "${pkgs.networkmanager}/bin/nmcli"; # because there is no `bin` output for some reason
  orgDir = config.ide.emacs.core.orgDir;
  passwordStorePath = config.ext.security.passwordStorePath;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  projectsRootMarkersEmacs =
    builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
  python3Binary = "${pkgs.python3}/bin/python3";
  systemTimeZone = config.time.timeZone;
  tmuxDefaultSession = config.shell.tmux.defaultSession;
  xprintidleBinary = "${pkgs.xprintidle-ng}/bin/xprintidle-ng";
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
} // lib.optionalAttrs (config.wm.xmonad.enable) rec {
  xmonadPrimaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.primary 20;
  xmonadSecondaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.secondary 22;
  xmonadTertiaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.tertiary 20;
}
