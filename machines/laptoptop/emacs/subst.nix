{ config, lib, pkgs, ... }:

rec {
  emacsAmxSaveFile = "${emacsDataDir}/amx-items";
  emacsCodesearchIndex = "${config.secrets.dev.workspaceRoot}/.csearchindex";
  emacsCustomFile = "/home/${config.attributes.mainUser.name}/.emacs.d/customizations.el";
  emacsCustomInfoDir = "/home/${config.attributes.mainUser.name}/help/info";
  emacsCustomYasnippetsDir = "/etc/nixos/pkgs/forges/github.com/wiedzmin/yasnippet-snippets";
  emacsDataDir = "/home/${config.attributes.mainUser.name}/.emacs.d/data";
  emacsElpaDir = "/home/${config.attributes.mainUser.name}/.emacs.d/elpa";
  emacsInitFile = "/etc/nixos/users/${config.attributes.mainUser.name}/config/dev/emacs/default.nix";
  emacsOrgAttachDir = "${emacsOrgDir}/org-attach-data";
  emacsOrgBrowserTabs = "${emacsOrgDir}/browser-tabs.org";
  emacsOrgDefaultNotesFile = "${emacsOrgDir}/refile.org";
  emacsOrgDir = "/home/${config.attributes.mainUser.name}/docs/org";
  emacsOrgDitaaJarPath = "${pkgs.ditaa}/lib/ditaa.jar";
  emacsOrgJournalFile = "${emacsOrgDir}/journal.org";
  emacsOrgJournalsDir = "${emacsOrgDir}/journals";
  emacsOrgKbDir = "/home/${config.attributes.mainUser.name}/docs/org-kb";
  emacsOrgMasteringFile = "${emacsOrgDir}/mastering.org";
  emacsOrgProjectsFile = "${emacsOrgDir}/projects.org";
  emacsOrgRecentHeadingsSaveFile = "${emacsDataDir}/org-recent-headings";
  emacsPdfViewRestoreFilename = "${emacsDataDir}/.pdf-view-restore";
  emacsPlantumlJarPath = "${pkgs.plantuml}/lib/plantuml.jar";
  emacsResourcesDir = "/home/${config.attributes.mainUser.name}/.emacs.d/resources";
  emacsXprintidleBin = "${pkgs.xprintidle-ng}/bin/xprintidle";
  jobReposRoot = config.secrets.job.workspaceRoot;
  timeZone = config.time.timeZone;
}
