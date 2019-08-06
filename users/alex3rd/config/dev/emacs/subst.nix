{config, pkgs, ...}:
with import ../../../const.nix {inherit config pkgs;};
rec {
    emacsAmxSaveFile = "${emacsDataDir}/amx-items";
    emacsCustomFile = "/home/${userName}/.emacs.d/customizations.el";
    emacsCustomInfoDir = "/home/${userName}/help/info";
    emacsCustomYasnippetsDir = "/etc/nixos/pkgs/forges/github.com/wiedzmin/yasnippet-snippets";
    emacsDataDir = "/home/${userName}/.emacs.d/data";
    emacsElpaDir = "/home/${userName}/.emacs.d/elpa";
    emacsInitFile = "/etc/nixos/users/${userName}/config/dev/emacs/default.nix";
    emacsOrgAttachDir = "${emacsOrgDir}/org-attach-data";
    emacsOrgBrowserTabs = "${emacsOrgDir}/browser-tabs.org";
    emacsOrgDefaultNotesFile = "${emacsOrgDir}/refile.org";
    emacsOrgDir = "/home/${userName}/docs/org";
    emacsOrgDitaaJarPath = "${pkgs.ditaa}/lib/ditaa.jar";
    emacsOrgJournalFile = "${emacsOrgDir}/journal.org";
    emacsOrgJournalsDir = "${emacsOrgDir}/journals";
    emacsOrgKbDir = "/home/${userName}/docs/org-kb";
    emacsOrgMasteringFile = "${emacsOrgDir}/mastering.org";
    emacsOrgProjectsFile = "${emacsOrgDir}/projects.org";
    emacsOrgRecentHeadingsSaveFile = "${emacsDataDir}/org-recent-headings";
    emacsPdfViewRestoreFilename = "${emacsDataDir}/.pdf-view-restore";
    emacsResourcesDir = "/home/${userName}/.emacs.d/resources";
    emacsPlantumlJarPath = "${pkgs.plantuml}/lib/plantuml.jar";
    emacsXprintidleBin = "${pkgs.xprintidle-ng}/bin/xprintidle";
    timeZone = config.time.timeZone;
}
