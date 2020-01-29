let
  deps = import ../../nix/sources.nix;
  nixpkgs-pinned-05_12_19  = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in
{ config, pkgs, ... }:

rec {
  awkBinary = "${pkgs.gawk}/bin/awk";
  bashBinary = "${pkgs.bash}/bin/bash";
  bookReader = config.attributes.defaultCommands.ebookReader;
  bookshelfPath = "/home/${config.attributes.mainUser.name}/bookshelf";
  bukuBinary = "${nixpkgs-pinned-05_12_19.buku}/bin/buku";
  contentBookmarksBatchOpenThreshold = builtins.toString config.custom.content.bookmarks.batchOpenTreshold;
  cutBinary = "${pkgs.coreutils}/bin/cut";
  dateBinary = "${pkgs.coreutils}/bin/date";
  dejsonlz4Binary = "${pkgs.dejsonlz4}/bin/dejsonlz4";
  dmenuBinary = "${pkgs.dmenu}/bin/dmenu";
  dunstifyBinary = "${pkgs.dunst}/bin/dunstify";
  emacsBrowserGenericProgram = config.attributes.defaultCommands.browser;
  emacsCustomFile = "/home/${config.attributes.mainUser.name}/.emacs.d/customizations.el";
  emacsDatadir = config.ide.emacs.dataDir;
  emacsResourcesDir = "/home/${config.attributes.mainUser.name}/.emacs.d/resources/";
  emacsYasnippetSnippets = deps.yasnippet-snippets;
  emacsclientBinary = "${pkgs.emacs}/bin/emacsclient";
  fdBinary = "${pkgs.fd}/bin/fd";
  firefoxBinary = "${pkgs.firefox-unwrapped}/bin/firefox";
  firefoxProfilePath = config.home-manager.users."${config.attributes.mainUser.name}".programs.firefox.profiles.default.path;
  firefoxSessionsHistoryLength = builtins.toString config.custom.browsers.sessions.firefox.historyLength;
  firefoxSessionsNameTemplate = config.custom.browsers.sessions.firefox.nameTemplate;
  firefoxSessionsPath = config.custom.browsers.sessions.firefox.path;
  firefoxSessionsSizeThreshold = builtins.toString config.custom.browsers.sessions.sizeThreshold;
  firefoxSessionstorePath = "/home/${config.attributes.mainUser.name}/.mozilla/firefox/${firefoxProfilePath}/sessionstore-backups";
  gitBinary = "${pkgs.git}/bin/git";
  gitHooksDirname = config.custom.dev.git.hooks.dirName;
  gitHooksShortCircuitPatch = if config.custom.dev.git.hooks.shortCircuit then "return $exitcode" else "";
  gitSecretsBinary = "${pkgs.gitAndTools.git-secrets}/bin/git-secrets";
  grepBinary = "${pkgs.gnugrep}/bin/grep";
  ixBinary = "${pkgs.ix}/bin/ix";
  jqBinary = "${pkgs.jq}/bin/jq";
  maimBinary = "${pkgs.maim}/bin/maim";
  mycliBinary = "${nixpkgs-pinned-05_12_19.mycli}/bin/mycli";
  orgWarningsFiledir = builtins.dirOf config.custom.pim.org.warningsFile;
  orgWarningsFilename = config.custom.pim.org.warningsFile;
  passBinary = "${pkgs.pass}/bin/pass";
  pgcliBinary = "${nixpkgs-pinned-05_12_19.pgcli}/bin/pgcli";
  rmBinary = "${pkgs.coreutils}/bin/rm";
  screenshotsBaseDir = config.custom.content.screenshots.baseDir;
  screenshotsBasedir = config.custom.content.screenshots.baseDir;
  screenshotsDateFormat = config.custom.content.screenshots.dateFormat;
  sedBinary = "${pkgs.gnused}/bin/sed";
  sortBinary = "${pkgs.coreutils}/bin/sort";
  systemTimeZone = config.time.timeZone;
  teeBinary = "${pkgs.coreutils}/bin/tee";
  tmuxBinary = "${pkgs.tmux}/bin/tmux";
  trBinary = "${pkgs.coreutils}/bin/tr";
  wcBinary = "${pkgs.coreutils}/bin/wc";
  workspaceRoot = config.secrets.dev.workspaceRoot;
  xclipBinary = "${pkgs.xclip}/bin/xclip";
  xdotoolBinary = "${pkgs.xdotool}/bin/xdotool";
  xselBinary = "${pkgs.xsel}/bin/xsel";
}
