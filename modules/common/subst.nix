{ config, lib, pkgs, inputs, ... }:
with import ../util.nix { inherit config lib pkgs; };

let configHome = config.home-manager.users."${config.attributes.mainUser.name}".xdg.configHome;
in rec {
  autorandrProfiles = homePrefix ".config/autorandr";
  bashExecutable = "/run/current-system/sw/bin/bash";
  booksSearchCommand = config.tools.ebooks.readers.booksSearchCommand;
  combyExcludes = lib.concatStringsSep "," config.custom.dev.comby.excludes;
  configResultPath = config.custom.packaging.configResultPath;
  defaultBrowser = "${pkgs.xdg_utils}/bin/xdg-open";
  defaultContainerShell = config.custom.virtualization.docker.defaultContainerShell;
  defaultTerminal = config.custom.shell.terminal;
  deftPath = homePrefix "docs/deft";
  direnvGranularityProject = emacsBoolToString (config.custom.dev.direnv.granularity == "project");
  direnvGranularityFile = emacsBoolToString (config.custom.dev.direnv.granularity == "file");
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  emacsBrowserGenericProgram = "${pkgs.xdg_utils}/bin/xdg-open";
  emacsCustomFile = homePrefix ".emacs.d/customizations.el";
  emacsDatadir = config.ide.emacs.dataDir;
  emacsServerSocketPath = "/run/user/${mainUserID}/emacs/server";
  emacsYasnippetSnippets = inputs.yasnippet-snippets;
  fallbackPackageArchives = emacsBoolToString false;
  wmFontDmenu = config.wmCommon.fonts.dmenu;
  gitDefaultMainBranchName = config.custom.dev.git.defaultMainBranchName;
  gitWipChangedLinesTreshold = builtins.toString config.custom.dev.git.wip.minChangedLines;
  gitWipIdletimeTreshold = builtins.toString config.custom.dev.git.wip.idleTime;
  gmrunHistorySize = builtins.toString config.custom.navigation.gmrun.historySize;
  gmrunTerminalApps = lib.concatStringsSep " " config.custom.navigation.gmrun.terminalApps;
  ivyCandidatesCount = builtins.toString config.custom.navigation.emacs.ivy.candidatesCount;
  keybindingsCachePath = config.wmCommon.keybindingsCachePath;
  lockScreenCommand = config.custom.security.lockScreenCommand;
  lspPythonMsExecutable = "${pkgs.python-language-server}/bin/python-language-server";
  lspPythonMsExtraPaths =
    builtins.concatStringsSep " " (lib.forEach config.custom.dev.python.pylsExtraSourcePaths (path: ''"${path}"''));
  mainUserName = config.attributes.mainUser.name;
  mainUserID = builtins.toString config.users.extraUsers."${config.attributes.mainUser.name}".uid;
  mycliCmd =
    "${inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.mycli}/bin/mycli --myclirc ${configHome}/.myclirc"; # because of deps versions conflict with pgcli
  nmcliBinary = "${pkgs.networkmanager}/bin/nmcli"; # because there is no `bin` output for some reason
  orgDir = config.ide.emacs.orgDir;
  orgKbDir = homePrefix "docs/org-kb";
  orgWarningsFiledir = builtins.dirOf config.custom.pim.org.warningsFile;
  orgWarningsFilename = config.custom.pim.org.warningsFile;
  passwordStorePath = config.custom.security.passwordStorePath;
  pgcliCmd =
    "${inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.pgcli}/bin/pgcli --pgclirc ${configHome}/.pgclirc"; # because of deps versions conflict with mycli
  pimOrgAgendaElPatch = config.custom.pim.org.agendaElPatch;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
  screenshotsBasedir = config.custom.content.screenshots.baseDir;
  screenshotsDateFormat = config.custom.content.screenshots.dateFormat;
  systemTimeZone = config.time.timeZone;
  tmuxDefaultSession = config.custom.shell.tmux.defaultSession;
  urlRegexPy = config.custom.content.urlRegex.py;
  xprintidleBinary = "${inputs.nixpkgs-16_04_20.legacyPackages.x86_64-linux.xprintidle-ng}/bin/xprintidle-ng";
  cclsExecutable = "${pkgs.ccls}/bin/ccls";
  xmobarMaybeFont = lib.optionalString (config.wmCommon.fonts.statusbar != "")
    ''font = "${config.wmCommon.fonts.statusbar}"${mkNewlineAndIndent 7}, '';
} // lib.optionalAttrs (config.custom.browsers.firefox.enable) rec {
  firefoxProfilePath = config.custom.programs.firefox.profiles.default.path;
  firefoxSessionsHistoryLength = builtins.toString config.custom.browsers.firefox.sessions.historyLength;
  firefoxSessionsNameTemplate = config.custom.browsers.firefox.sessions.nameTemplate;
  firefoxSessionsPath = config.custom.browsers.firefox.sessions.path;
  firefoxSessionsSizeThreshold = builtins.toString config.custom.browsers.firefox.sessions.sizeThreshold;
  firefoxSessionstorePath = homePrefix ".mozilla/firefox/${firefoxProfilePath}/sessionstore-backups";
} // lib.optionalAttrs (builtins.hasAttr "global" config.custom.dev.workspaceRoots) rec {
  globalWorkspaceRoot = wsRootAbs "global";
  searchReposRoot = config.custom.dev.repoSearch.root;
}
