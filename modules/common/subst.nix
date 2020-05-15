let
  deps = import ../../nix/sources.nix;
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:

rec {
  autorandrProfiles = "/home/${config.attributes.mainUser.name}/.config/autorandr";
  booksSearchCommand = config.tools.ebooks.readers.booksSearchCommand;
  contentBookmarksBatchOpenThreshold = builtins.toString config.custom.content.bookmarks.batchOpenTreshold;
  defaultBrowser = "${pkgs.xdg_utils}/bin/xdg-open";
  defaultContainerShell = config.custom.virtualization.docker.defaultContainerShell;
  defaultTerminal = config.custom.shell.terminal;
  deftPath = "/home/${config.attributes.mainUser.name}/docs/deft";
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  downloadPath = config.custom.browsers.downloadPath;
  emacsBrowserGenericProgram = "${pkgs.xdg_utils}/bin/xdg-open";
  emacsCustomFile = "/home/${config.attributes.mainUser.name}/.emacs.d/customizations.el";
  emacsClient = "${pkgs.emacs}/bin/emacsclient";
  emacsDatadir = config.ide.emacs.dataDir;
  emacsYasnippetSnippets = deps.yasnippet-snippets;
  gitIdletimeStgit = builtins.toString config.custom.dev.git.idletime.stgit;
  gmrunHistorySize = builtins.toString config.custom.navigation.gmrun.historySize;
  gmrunTerminalApps = lib.concatStringsSep " " config.custom.navigation.gmrun.terminalApps;
  keybindingsCachePath = config.wm.xmonad.keybindingsCachePath;
  lspPythonMsExecutable = "${pkgs.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths =
    builtins.concatStringsSep " " (lib.forEach config.custom.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
  mainUserName = config.attributes.mainUser.name;
  mycliBinary = "${nixpkgs-pinned-05_12_19.mycli}/bin/mycli"; # because of deps versions conflict with pgcli
  orgDir = config.ide.emacs.orgDir;
  orgKbDir = "/home/${config.attributes.mainUser.name}/docs/org-kb";
  orgWarningsFiledir = builtins.dirOf config.custom.pim.org.warningsFile;
  orgWarningsFilename = config.custom.pim.org.warningsFile;
  pgcliBinary = "${nixpkgs-pinned-05_12_19.pgcli}/bin/pgcli"; # because of deps versions conflict with mycli
  pimOrgAgendaElPatch = config.custom.pim.org.agendaElPatch;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  pythonLibPatch = config.custom.dev.pythonLib;
  screenshotsBasedir = config.custom.content.screenshots.baseDir;
  screenshotsDateFormat = config.custom.content.screenshots.dateFormat;
  systemTimeZone = config.time.timeZone;
  tmuxDefaultSession = config.custom.shell.tmux.defaultSession;
  urlRegexPy = config.custom.content.urlRegex.py;
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
