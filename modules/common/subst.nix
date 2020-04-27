let
  deps = import ../../nix/sources.nix;
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:

rec {
  autorandrProfiles = "/home/${config.attributes.mainUser.name}/.config/autorandr";
  bookReader = config.attributes.defaultCommands.ebookReader;
  bookshelfPath = "/home/${config.attributes.mainUser.name}/bookshelf";
  contentBookmarksBatchOpenThreshold = builtins.toString config.custom.content.bookmarks.batchOpenTreshold;
  defaultBrowser = config.attributes.defaultCommands.browser;
  defaultContainerShell = config.custom.virtualization.docker.defaultContainerShell;
  defaultEbookReader = config.attributes.defaultCommands.ebookReader;
  defaultPager = config.attributes.defaultCommands.pager;
  defaultSpreadsheetEditor = config.attributes.defaultCommands.spreadsheetEditor;
  defaultTerminal = config.attributes.defaultCommands.terminal;
  defaultTextProcessor = config.attributes.defaultCommands.textProcessor;
  deftPath = "/home/${config.attributes.mainUser.name}/docs/deft";
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  downloadPath = config.custom.browsers.downloadPath;
  emacsBrowserGenericProgram = config.attributes.defaultCommands.browser;
  emacsCustomFile = "/home/${config.attributes.mainUser.name}/.emacs.d/customizations.el";
  emacsClient = "${pkgs.emacs}/bin/emacsclient";
  emacsDatadir = config.ide.emacs.dataDir;
  emacsYasnippetSnippets = deps.yasnippet-snippets;
  gitIdletimeStgit = builtins.toString config.custom.dev.git.idletime.stgit;
  gmrunHistorySize = builtins.toString config.custom.navigation.gmrun.historySize;
  gmrunTerminalApps = lib.concatStringsSep " " config.custom.navigation.gmrun.terminalApps;
  ivyCandidatesCount = builtins.toString config.custom.navigation.emacs.ivy.candidatesCount;
  keybindingsCachePath = config.wm.xmonad.keybindingsCachePath;
  lspPythonMsExtraPaths =
    builtins.concatStringsSep " " (lib.forEach config.custom.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
  mainUserName = config.attributes.mainUser.name;
  mycliBinary = "${nixpkgs-pinned-05_12_19.mycli}/bin/mycli"; # because of deps versions conflict with pgcli
  orgDir = config.ide.emacs.orgDir;
  orgKbPath = "/home/${config.attributes.mainUser.name}/docs/org-kb";
  orgWarningsFiledir = builtins.dirOf config.custom.pim.org.warningsFile;
  orgWarningsFilename = config.custom.pim.org.warningsFile;
  pgcliBinary = "${nixpkgs-pinned-05_12_19.pgcli}/bin/pgcli"; # because of deps versions conflict with mycli
  pimOrgAgendaElPatch = config.custom.pim.org.agendaElPatch;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  pythonLibPatch = config.custom.dev.pythonLib;
  screenshotsBasedir = config.custom.content.screenshots.baseDir;
  screenshotsDateFormat = config.custom.content.screenshots.dateFormat;
  systemTimeZone = config.time.timeZone;
  tmuxDefaultSession = config.attributes.tmux.defaultSession;
  urlRegex = config.attributes.urlRegex;
  xprintidleBinary = "${nixpkgs-pinned-05_12_19.xprintidle-ng}/bin/xprintidle-ng";
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
  searchReposRoot = config.custom.dev.repoSearch.root;

} // lib.optionalAttrs (config.ide.emacs.enable) { globalWorkspaceRoot = config.custom.dev.workspaceRoots.global; }
// lib.optionalAttrs (config.custom.browsers.firefox.enable) rec {
  firefoxProfilePath =
    config.home-manager.users."${config.attributes.mainUser.name}".programs.firefox.profiles.default.path;
  firefoxSessionsHistoryLength = builtins.toString config.custom.browsers.sessions.firefox.historyLength;
  firefoxSessionsNameTemplate = config.custom.browsers.sessions.firefox.nameTemplate;
  firefoxSessionsPath = config.custom.browsers.sessions.firefox.path;
  firefoxSessionsSizeThreshold = builtins.toString config.custom.browsers.sessions.sizeThreshold;
  firefoxSessionstorePath =
    "/home/${config.attributes.mainUser.name}/.mozilla/firefox/${firefoxProfilePath}/sessionstore-backups";
}
