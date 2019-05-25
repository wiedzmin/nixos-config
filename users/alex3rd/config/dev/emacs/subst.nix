{config, ...}:
with import ../../../const.nix {inherit config;};
rec {
    emacsAmxSaveFile = "${emacsDataDir}/amx-items";
    emacsCustomFile = "/home/${userName}/.emacs.d/customizations.el";
    emacsCustomInfoDir = "/home/${userName}/help/info";
    emacsCustomYasnippetsDir = "${emacsResourcesDir}/yasnippet";
    emacsDataDir = "/home/${userName}/.emacs.d/data";
    emacsElpaDir = "/home/${userName}/.emacs.d/elpa";
    emacsInitFile = "/etc/nixos/users/${userName}/config/dev/emacs/default.nix";
    emacsMagithubCloneDefaultDirectory = "/home/${userName}/workspace/repos/github.com";
    emacsOrgAttachDir = "${emacsOrgDir}/org-attach-data";
    emacsOrgBrowserTabs = "${emacsOrgDir}/browser-tabs.org";
    emacsOrgDefaultNotesFile = "${emacsOrgDir}/refile.org";
    emacsOrgDir = "/home/${userName}/docs/org";
    emacsOrgDitaaJarPath = "${emacsResourcesDir}/ditaa0_9.jar";
    emacsOrgJournalFile = "${emacsOrgDir}/journal.org";
    emacsOrgJournalsDir = "${emacsOrgDir}/journals";
    emacsOrgKbDir = "/home/${userName}/docs/org-kb";
    emacsOrgMasteringFile = "${emacsOrgDir}/mastering.org";
    emacsOrgProjectsFile = "${emacsOrgDir}/projects.org";
    emacsOrgRecentHeadingsSaveFile = "${emacsDataDir}/org-recent-headings";
    emacsPdfViewRestoreFilename = "${emacsDataDir}/.pdf-view-restore";
    emacsResourcesDir = "/home/${userName}/.emacs.d/resources";
    timeZone = config.time.timeZone;
}
