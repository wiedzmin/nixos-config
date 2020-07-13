let
  deps = import ../../nix/sources.nix;
  nixpkgs-pinned-16_04_20 = import deps.nixpkgs-pinned-16_04_20 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };

rec {
  autorandrProfiles = homePrefix ".config/autorandr";
  bashExecutable = "/run/current-system/sw/bin/bash";
  booksSearchCommand = config.tools.ebooks.readers.booksSearchCommand;
  defaultBrowser = "${pkgs.xdg_utils}/bin/xdg-open";
  defaultContainerShell = config.custom.virtualization.docker.defaultContainerShell;
  defaultTerminal = config.custom.shell.terminal;
  deftPath = homePrefix "docs/deft";
  direnvGranularityProject = emacsBoolToString (config.custom.dev.direnvGranularity == "project");
  direnvGranularityFile = emacsBoolToString (config.custom.dev.direnvGranularity == "file");
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  downloadPath = config.custom.browsers.downloadPath;
  emacsBrowserGenericProgram = "${pkgs.xdg_utils}/bin/xdg-open";
  emacsCustomFile = homePrefix ".emacs.d/customizations.el";
  emacsClient = "${pkgs.emacs}/bin/emacsclient";
  emacsDatadir = config.ide.emacs.dataDir;
  emacsYasnippetSnippets = deps.yasnippet-snippets;
  fallbackPackageArchives = emacsBoolToString false;
  gitIdletimeStgit = builtins.toString config.custom.dev.git.idletime.stgit;
  gmrunHistorySize = builtins.toString config.custom.navigation.gmrun.historySize;
  gmrunTerminalApps = lib.concatStringsSep " " config.custom.navigation.gmrun.terminalApps;
  keybindingsCachePath = config.wmCommon.keybindingsCachePath;
  lockScreenCommand = config.custom.security.lockScreenCommand;
  lspPythonMsExecutable = "${pkgs.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths =
    builtins.concatStringsSep " " (lib.forEach config.custom.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
  mainUserName = config.attributes.mainUser.name;
  mycliBinary = "${nixpkgs-pinned-16_04_20.mycli}/bin/mycli"; # because of deps versions conflict with pgcli
  nmcliBinary = "${pkgs.networkmanager}/bin/nmcli"; # because there is no `bin` output for some reason
  orgDir = config.ide.emacs.orgDir;
  orgKbDir = homePrefix "docs/org-kb";
  orgWarningsFiledir = builtins.dirOf config.custom.pim.org.warningsFile;
  orgWarningsFilename = config.custom.pim.org.warningsFile;
  pgcliBinary = "${nixpkgs-pinned-16_04_20.pgcli}/bin/pgcli"; # because of deps versions conflict with mycli
  pimOrgAgendaElPatch = config.custom.pim.org.agendaElPatch;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  pythonPatchNotify = config.custom.dev.pythonLib."notify".patch;
  pythonPatchXlib = config.custom.dev.pythonLib."xlib".patch;
  screenshotsBasedir = config.custom.content.screenshots.baseDir;
  screenshotsDateFormat = config.custom.content.screenshots.dateFormat;
  systemTimeZone = config.time.timeZone;
  tmuxDefaultSession = config.custom.shell.tmux.defaultSession;
  urlRegexPy = config.custom.content.urlRegex.py;
  xprintidleBinary = "${nixpkgs-pinned-16_04_20.xprintidle-ng}/bin/xprintidle-ng";
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
  searchReposRoot = config.custom.dev.repoSearch.root;
  globalWorkspaceRoot = homePrefix config.custom.navigation.workspaceRoots.global;
  xmobarMaybeFont = lib.optionalString (config.wmCommon.fonts.statusbar != "")
    ''font = "${config.wmCommon.fonts.statusbar}"${mkNewlineAndIndent 7}, '';
} // lib.optionalAttrs (config.custom.browsers.firefox.enable) rec {
  firefoxProfilePath =
    config.home-manager.users."${config.attributes.mainUser.name}".programs.firefox.profiles.default.path;
  firefoxSessionsHistoryLength = builtins.toString config.custom.browsers.sessions.firefox.historyLength;
  firefoxSessionsNameTemplate = config.custom.browsers.sessions.firefox.nameTemplate;
  firefoxSessionsPath = config.custom.browsers.sessions.firefox.path;
  firefoxSessionsSizeThreshold = builtins.toString config.custom.browsers.sessions.sizeThreshold;
  firefoxSessionstorePath = homePrefix ".mozilla/firefox/${firefoxProfilePath}/sessionstore-backups";
}
