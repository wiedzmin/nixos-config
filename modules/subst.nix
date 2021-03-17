{ config, inputs, lib, pkgs, ... }:
with import ./util.nix { inherit config inputs lib pkgs; };
with import ./wmutil.nix { inherit config inputs lib pkgs; };

let
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users."${user}";
  configHome = hm.xdg.configHome;
in rec {
  autorandrProfiles = homePrefix ".config/autorandr";
  bashExecutable = "/run/current-system/sw/bin/bash";
  booksSearchCommand = config.content.ebooks.searchCommand;
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  defaultBrowser = config.attributes.browser.default.cmd;
  docsSearchCommand = config.paperworks.docflow.searchCommand;
  fallbackBrowser = config.attributes.browser.fallback.cmd;
  defaultContainerShell = config.ext.virtualization.docker.core.defaultContainerShell;
  defaultVTCmd = lib.concatStringsSep " " config.attributes.defaultVTCommand;
  deftPath = homePrefix "docs/deft";
  direnvGranularityProject = emacsBoolToString (config.dev.direnv.emacs.granularity == "project");
  direnvGranularityFile = emacsBoolToString (config.dev.direnv.emacs.granularity == "file");
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  globalWorkspaceRoot = homePrefix config.navigation.bookmarks.workspaces.globalRoot;
  searchReposRoot = config.dev.navigation.projects.fuzzySearch.root;
  searchDepth = config.dev.navigation.projects.fuzzySearch.depth;
  emacsBrowserGenericProgram = "${pkgs.xdg_utils}/bin/xdg-open";
  emacsCustomFile = homePrefix ".emacs.d/customizations.el";
  emacsDatadir = config.ide.emacs.core.dataDir;
  emacsServerSocketPath = "/run/user/${mainUserID}/emacs/server";
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  emacsObGoPath = inputs.emacs-ob-go;
  emacsBruhPath = inputs.emacs-bruh;
  emacsBookmarkViewPath = inputs.emacs-bookmark-view;
  factoryCalendarKey = "scheduling/fcalendar";
  fallbackPackageArchives = emacsBoolToString false;
  wmFontDmenu = config.wmCommon.fonts.dmenu;
  gitWipChangedLinesTreshold = builtins.toString config.dev.git.savewip.minChangedLines;
  gmrunHistorySize = builtins.toString config.controlcenter.gmrun.historySize;
  gmrunTerminalApps = lib.concatStringsSep " " config.controlcenter.gmrun.terminalApps;
  keybindingsCachePath = config.wmCommon.keybindingsCachePath;
  lockScreenCommand = config.workstation.lockscreen.command.lock;
  lspPythonMsExecutable = "${pkgs.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths =
    builtins.concatStringsSep " " (lib.forEach config.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
  mainUserName = config.attributes.mainUser.name;
  mainUserID = builtins.toString config.users.extraUsers."${config.attributes.mainUser.name}".uid;
  mcCmd = "${pkgs.mc}/bin/mc";
  nmcliBinary = "${pkgs.networkmanager}/bin/nmcli"; # because there is no `bin` output for some reason
  orgDir = config.ide.emacs.core.orgDir;
  orgWarningsFiledir = builtins.dirOf config.pim.orgmode.warningsFile;
  orgWarningsFilename = config.pim.orgmode.warningsFile;
  passwordStorePath = config.custom.security.passwordStorePath;
  passwordPlaceholder = config.attributes.secret;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  projectEnvConfigName = config.dev.projectenv.configName;
  projectEnvStashName = config.dev.projectenv.stashName;
  projectEnvBackupRoot = config.dev.projectenv.backupRoot;
  projectEnvBackupDateFormat = config.attributes.dateFormats.commonShellNoColons;
  projectsRootMarkers =
    builtins.concatStringsSep " " (lib.forEach config.dev.navigation.projects.rootMarkers (marker: ''"${marker}"''));
  python3Binary = "${pkgs.python3}/bin/python3";
  systemTimeZone = config.time.timeZone;
  screenshotsBasedir = config.content.screenshots.baseDir;
  tmuxDefaultSession = config.shell.tmux.defaultSession;
  urlRegexPy = config.attributes.regexes."url"."py";
  xprintidleBinary = "${pkgs.xprintidle-ng}/bin/xprintidle-ng";
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
  xmobarMaybeFont = lib.optionalString (config.wmCommon.fonts.statusbar != "")
    ''font = "${config.wmCommon.fonts.statusbar}"${mkNewlineAndIndent 7}, '';
} // lib.optionalAttrs (config.browsers.firefox.enable) rec {
  firefoxProfilePath = config.custom.programs.firefox.profiles.default.path;
  firefoxSessionsHistoryLength = builtins.toString config.browsers.firefox.sessions.historyLength;
  firefoxSessionsNameTemplate = config.browsers.firefox.sessions.nameTemplate;
  firefoxSessionsPath = config.browsers.firefox.sessions.path;
  firefoxSessionsSizeThreshold = builtins.toString config.browsers.firefox.sessions.sizeThreshold;
  firefoxSessionstorePath = homePrefix ".mozilla/firefox/${firefoxProfilePath}/sessionstore-backups";
} // lib.optionalAttrs (config.browsers.qutebrowser.enable) rec {
  qutebrowserSessionsNameTemplate = config.browsers.qutebrowser.sessions.nameTemplate;
  qutebrowserSessionsPath = config.browsers.qutebrowser.sessions.path;
} // lib.optionalAttrs (config.wm.xmonad.enable) rec {
  xmonadPrimaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.primary 20;
  xmonadSecondaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.secondary 22;
  xmonadTertiaryWorkspaces = mkWorkspacesXmonad config.wmCommon.workspaces.tertiary 20;
}
