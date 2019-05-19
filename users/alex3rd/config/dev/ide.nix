{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
let
    vcsNix = {
        "[nix]"."editor.tabSize" = 4;
    };
    vcsTextEditor = {
        "editor.accessibilitySupport" = "off";
        "editor.autoClosingBrackets" = "languageDefined";
        "editor.autoClosingQuotes" = "languageDefined";
        "editor.autoIndent" = true;
        "editor.autoSurround" = "languageDefined";
        "editor.codeActionsOnSaveTimeout" = 750;
        "editor.codeLens" = true;
        "editor.colorDecorators" = true;
        "editor.copyWithSyntaxHighlighting" = true;
        "editor.detectIndentation" = true;
        "editor.dragAndDrop" = false;
        "editor.emptySelectionClipboard" = false;
        "editor.fastScrollSensitivity" = 2;
        "editor.folding" = true;
        "editor.foldingStrategy" = "auto";
        "editor.glyphMargin" = true;
        "editor.gotoLocation.multiple" = "gotoAndPeek";
        "editor.hideCursorInOverviewRuler" = true;
        "editor.highlightActiveIndentGuide" = true;
        "editor.hover.delay" = 100;
        "editor.hover.enabled" = true;
        "editor.hover.sticky" = true;
        "editor.insertSpaces" = true;
        "editor.largeFileOptimizations" = true;
        "editor.letterSpacing" = 0;
        "editor.lightbulb.enabled" = true;
        "editor.lineHeight" = 0;
        "editor.lineNumbers" = "on";
        "editor.links" = true;
        "editor.matchBrackets" = true;
        "editor.maxTokenizationLineLength" = 20000;
        "editor.mouseWheelScrollSensitivity" = 1;
        "editor.mouseWheelZoom" = false;
        "editor.multiCursorMergeOverlapping" = true;
        "editor.multiCursorModifier" = "alt";
        "editor.occurrencesHighlight" = true;
        "editor.overviewRulerBorder" = true;
        "editor.overviewRulerLanes" = 3;
        "editor.parameterHints.cycle" = true;
        "editor.parameterHints.enabled" = true;
        "editor.renderControlCharacters" = true;
        "editor.renderFinalNewline" = false;
        "editor.renderIndentGuides" = true;
        "editor.renderLineHighlight" = "gutter";
        "editor.renderWhitespace" = "boundary";
        "editor.roundedSelection" = true;
        "editor.scrollBeyondLastColumn" = 5;
        "editor.scrollBeyondLastLine" = true;
        "editor.selectionClipboard" = false;
        "editor.selectionHighlight" = true;
        "editor.showFoldingControls" = "always";
        "editor.showUnused" = false;
        "editor.smoothScrolling" = false;
        "editor.stablePeek" = false;
        "editor.tabCompletion" = "on";
        "editor.tabSize" = 4;
        "editor.trimAutoWhitespace" = true;
        "editor.useTabStops" = true;
        "editor.wordSeparators" = "`~!@#$%^&*()-=+[{]}\\|;:'\",.<>/?";
        "editor.wordWrap" = "bounded";
        "editor.wordWrapColumn" = 120;
        "editor.wrappingIndent" = "same";
        "editor.cursorBlinking" = "smooth";
        "editor.cursorSmoothCaretAnimation" = false;
        "editor.cursorStyle" = "underline";
        "editor.cursorWidth" = 0;
        "editor.find.addExtraSpaceOnTop" = true;
        "editor.find.autoFindInSelection" = true;
        "editor.find.seedSearchStringFromSelection" = true;
        "editor.fontFamily" = "Iosevka";
        "editor.fontLigatures" = true;
        "editor.fontSize" = 11;
        "editor.fontWeight" = "bold";
        "editor.formatOnPaste" = true;
        "editor.formatOnSave" = false;
        "editor.formatOnSaveTimeout" = 750;
        "editor.formatOnType" = true;
        "diffEditor.ignoreTrimWhitespace" = false;
        "diffEditor.renderIndicators" = true;
        "diffEditor.renderSideBySide" = true;
        "editor.minimap.enabled" = true;
        "editor.minimap.maxColumn" = 120;
        "editor.minimap.renderCharacters" = true;
        "editor.minimap.showSlider" = "mouseover";
        "editor.minimap.side" = "right";
        "editor.acceptSuggestionOnCommitCharacter" = true;
        "editor.acceptSuggestionOnEnter" = "smart";
        "editor.quickSuggestions" = {
            "other" = true;
            "comments" = false;
            "strings" = false;
        };
        "editor.quickSuggestionsDelay" = 10;
        "editor.snippetSuggestions" = "inline";
        "editor.suggest.filteredTypes" = {
            "keyword" = true;
        };
        "editor.suggest.filterGraceful" = true;
        "editor.suggest.localityBonus" = false;
        "editor.suggest.maxVisibleSuggestions" = 15;
        "editor.suggest.shareSuggestSelections" = true;
        "editor.suggest.showIcons" = true;
        "editor.suggest.snippetsPreventQuickSuggestions" = true;
        "editor.suggestFontSize" = 0;
        "editor.suggestLineHeight" = 0;
        "editor.suggestOnTriggerCharacters" = true;
        "editor.suggestSelection" = "recentlyUsedByPrefix";
        "editor.wordBasedSuggestions" = false;
        "files.autoGuessEncoding" = false;
        "files.autoSave" = "afterDelay";
        "files.autoSaveDelay" = 20000;
        "files.defaultLanguage" = "";
        "files.enableTrash" = true;
        "files.encoding" = "utf8";
        "files.eol" = "auto";
        "files.exclude" = {
            "**/*.pyc" = true;
            "**/__pycache__" = true;
        };
        "files.hotExit" = "onExitAndWindowClose";
        "files.insertFinalNewline" = true;
        "files.maxMemoryForLargeFilesMB" = 4096;
        "files.trimFinalNewlines" = true;
        "files.trimTrailingWhitespace" = false;
        "files.useExperimentalFileWatcher" = true;
        "files.watcherExclude" = {
            "**/.git/objects/**" = true;
            "**/.git/subtree-cache/**" = true;
            "**/node_modules/**" = true;
        };
    };
    vscWorkbench = {
        "workbench.commandPalette.history" = 50;
        "workbench.commandPalette.preserveInput" = false;
        "workbench.enableExperiments" = true;
        "workbench.list.automaticKeyboardNavigation" = true;
        "workbench.list.horizontalScrolling" = true;
        "workbench.list.keyboardNavigation" = "filter";
        "workbench.list.multiSelectModifier" = "ctrlCmd";
        "workbench.list.openMode" = "doubleClick";
        "workbench.panel.defaultLocation" = "bottom";
        "workbench.quickOpen.closeOnFocusLost" = true;
        "workbench.quickOpen.preserveInput" = false;
        "workbench.startupEditor" = "welcomePage";
        "workbench.activityBar.visible" = true;
        "workbench.colorTheme" = "Zenburn";
        "workbench.iconTheme" = "vscode-icons";
        "workbench.sideBar.location" = "left";
        "workbench.statusBar.feedback.visible" = false;
        "workbench.statusBar.visible" = true;
        "workbench.tips.enabled" = true;
        "workbench.tree.indent" = 8;
        "workbench.view.alwaysShowHeaderActions" = true;
        "breadcrumbs.enabled" = true;
        "breadcrumbs.filePath" = "last";
        "breadcrumbs.symbolPath" = "on";
        "breadcrumbs.symbolSortOrder" = "position";
        "workbench.editor.centeredLayoutAutoResize" = true;
        "workbench.editor.closeEmptyGroups" = true;
        "workbench.editor.closeOnFileDelete" = false;
        "workbench.editor.enablePreview" = true;
        "workbench.editor.enablePreviewFromQuickOpen" = true;
        "workbench.editor.focusRecentEditorAfterClose" = true;
        "workbench.editor.highlightModifiedTabs" = true;
        "workbench.editor.labelFormat" = "short";
        "workbench.editor.openPositioning" = "right";
        "workbench.editor.openSideBySideDirection" = "right";
        "workbench.editor.restoreViewState" = true;
        "workbench.editor.revealIfOpen" = true;
        "workbench.editor.showIcons" = true;
        "workbench.editor.showTabs" = false; # was true
        "workbench.editor.tabCloseButton" = "right";
        "workbench.editor.tabSizing" = "shrink";
        "workbench.settings.editor" = "ui";
        "workbench.settings.enableNaturalLanguageSearch" = false;
        "workbench.settings.openDefaultKeybindings" = true;
        "workbench.settings.openDefaultSettings" = false;
        "workbench.settings.settingsSearchTocBehavior" = "filter";
        "workbench.settings.useSplitJSON" = false;
        "zenMode.centerLayout" = true;
        "zenMode.fullScreen" = true;
        "zenMode.hideActivityBar" = true;
        "zenMode.hideLineNumbers" = true;
        "zenMode.hideStatusBar" = true;
        "zenMode.hideTabs" = true;
        "zenMode.restore" = false;
    };
    vscWindow = {
        "window.closeWhenEmpty" = false;
        "window.doubleClickIconToClose" = false;
        "window.enableMenuBarMnemonics" = true;
        "window.menuBarVisibility" = "default";
        "window.restoreFullscreen" = false;
        "window.restoreWindows" = "all";
        "window.title" = ''
           ''${dirty}''${activeEditorShort}''${separator}''${rootName}''${separator}''${appName}"
        '';
        "window.titleBarStyle" = "native";
        "window.zoomLevel" = 0;
        "window.newWindowDimensions" = "default";
        "window.openFilesInNewWindow" = "off";
        "window.openFoldersInNewWindow" = "default";
        "window.openWithoutArgumentsInNewWindow" = "on";
    };
    vscFeatures = {
        "explorer.autoReveal" = true;
        "explorer.confirmDelete" = true;
        "explorer.confirmDragAndDrop" = true;
        "explorer.decorations.badges" = true;
        "explorer.decorations.colors" = true;
        "explorer.enableDragAndDrop" = false;
        "explorer.openEditors.visible" = 9;
        "explorer.sortOrder" = "default";
        "outline.icons" = true;
        "outline.problems.badges" = true;
        "outline.problems.colors" = true;
        "outline.problems.enabled" = true;
        "search.actionsPosition" = "auto";
        "search.collapseResults" = "auto";
        "search.exclude" = {
            "**/*.pyc" = true;
            "**/__pycache__" = true;
        };
        "search.followSymlinks" = true;
        "search.location" = "panel";
        "search.maintainFileSearchCache" = false;
        "search.quickOpen.includeHistory" = true;
        "search.quickOpen.includeSymbols" = true;
        "search.runInExtensionHost" = false;
        "search.showLineNumbers" = true;
        "search.smartCase" = true;
        "search.useGlobalIgnoreFiles" = false;
        "search.useIgnoreFiles" = true;
        "search.usePCRE2" = false;
        "search.useReplacePreview" = true;
        "debug.allowBreakpointsEverywhere" = false;
        "debug.console.fontFamily" = "Iosevka";
        "debug.console.fontSize" = 11;
        "debug.console.lineHeight" = 0;
        "debug.enableAllHovers" = false;
        "debug.inlineValues" = true;
        "debug.internalConsoleOptions" = "openOnFirstSessionStart";
        "debug.openDebug" = "openOnSessionStart";
        "debug.openExplorerOnEnd" = false;
        "debug.showInStatusBar" = "onFirstSessionStart";
        "debug.toolBarLocation" = "docked";
        "launch" = {
            "configurations" = [];
            "compounds" = [];
        };
        "scm.alwaysShowActions" = false;
        "scm.alwaysShowProviders" = false;
        "scm.diffDecorations" = "all";
        "scm.diffDecorationsGutterWidth" = 3;
        "scm.providers.visible" = 10;
        "extensions.autoCheckUpdates" = true;
        "extensions.autoUpdate" = false;
        "extensions.closeExtensionDetailsOnViewChange" = false;
        "extensions.ignoreRecommendations" = false;
        "extensions.showRecommendationsOnlyOnDemand" = false;
        "terminal.explorerKind" = "integrated";
        "terminal.external.linuxExec" = "dumb";
        "terminal.integrated.commandsToSkipShell" = [];
        "terminal.integrated.confirmOnExit" = false;
        "terminal.integrated.copyOnSelection" = false;
        "terminal.integrated.cursorBlinking" = false;
        "terminal.integrated.cursorStyle" = "underline";
        "terminal.integrated.cwd" = "";
        "terminal.integrated.drawBoldTextInBrightColors" = true;
        "terminal.integrated.enableBell" = false;
        # "terminal.integrated.env.linux" = {};
        "terminal.integrated.fontFamily" = "Iosevka";
        "terminal.integrated.fontSize" = 11;
        "terminal.integrated.fontWeight" = "bold";
        "terminal.integrated.fontWeightBold" = "bold";
        "terminal.integrated.letterSpacing" = 0;
        "terminal.integrated.lineHeight" = 1;
        "terminal.integrated.rendererType" = "auto";
        "terminal.integrated.rightClickBehavior" = "default";
        "terminal.integrated.scrollback" = 100000;
        "terminal.integrated.setLocaleVariables" = true;
        "terminal.integrated.shell.linux" = "/run/current-system/sw/bin/zsh";
        "terminal.integrated.shellArgs.linux" = [];
        "terminal.integrated.showExitAlert" = true;
        "terminal.integrated.splitCwd" = "inherited";
        "problems.autoReveal" = true;
        "problems.decorations.enabled" = true;
        "comments.openPanel" = "openOnSessionStartWithComments";
    };
    vscApplication = {
        "http.proxy" = "";
        "http.proxyStrictSSL" = true;
        "http.proxySupport" = "override";
        "http.systemCertificates" = true;
        "keyboard.dispatch" = "code";
        "update.enableWindowsBackgroundUpdates" = false;
        "update.mode" = "none";
        "update.showReleaseNotes" = true;
        "telemetry.enableCrashReporter" = true;
        "telemetry.enableTelemetry" = false;
    };
    vscExtensions = { # only sparse subset is pinned
        "autoDocstring.docstringFormat" = "default";
        "autoDocstring.generateDocstringOnEnter" = true;
        "autoDocstring.guessTypes" = true;
        "autoDocstring.includeExtendedSummary" = false;
        "autoDocstring.includeName" = false;
        "autoDocstring.quoteStyle" = "\"\"\"";
        "autoDocstring.startOnNewLine" = false;
        "bookmarks.backgroundLineColor" = "";
        "bookmarks.navigateThroughAllFiles" = true;
        "bookmarks.saveBookmarksInProject" = true;
        "bookmarks.showCommandsInContextMenu" = true;
        "bookmarks.useWorkaroundForFormatters" = true;
        "bookmarks.wrapNavigation" = true;
        "django.i18n" = true;
        "django.snippets.exclude" = [
            "cms"
            "wagtail"
        ];
        "django.snippets.use" = true;
        "djangointellisense.debugMessages" = false;
        # "djangointellisense.projectRoot" = "";
        # "djangointellisense.settingsModule" = "";
        "docker.attachShellCommand.linuxContainer" = "/bin/sh -c \"[ -e /bin/bash ] && /bin/bash || /bin/sh\"";
        # "docker.defaultRegistryPath" = "";
        "docker.dockerComposeBuild" = true;
        "docker.dockerComposeDetached" = true;
        "docker.explorerRefreshInterval" = 1000;
        "docker.groupImagesBy" = "Repository";
        "emmet.excludeLanguages" = [
            "markdown"
        ];
        "emmet.includeLanguages" = {};
        "emmet.showAbbreviationSuggestions" = true;
        "git.allowForcePush" = false;
        "git.alwaysShowStagedChangesResourceGroup" = true;
        "git.alwaysSignOff" = false;
        "git.autofetch" = false;
        "git.autofetchPeriod" = 180;
        "git.autorefresh" = true;
        "git.autoRepositoryDetection" = true;
        "git.autoStash" = true;
        "git.branchWhitespaceChar" = "-";
        "git.branchValidationRegex" = "";
        "git.checkoutType" = "all";
        "git.confirmEmptyCommits" = true;
        "git.confirmForcePush" = true;
        "git.confirmSync" = true;
        "git.countBadge" = "all";
        "git.decorations.enabled" = true;
        "git.defaultCloneDirectory" = "${config.dev.workspacePath}/fromvscode";
        "git.detectSubmodules" = true;
        "git.detectSubmodulesLimit" = 10;
        "git.enableCommitSigning" = false;
        "git.enabled" = true;
        "git.enableSmartCommit" = false;
        "git.fetchOnPull" = false;
        "git.ignoredRepositories" = [];
        "git.ignoreLegacyWarning" = false;
        "git.ignoreLimitWarning" = false;
        "git.ignoreMissingGitWarning" = false;
        "git.inputValidation" = "warn";
        "git.inputValidationLength" = 72;
        "git.inputValidationSubjectLength" = 50;
        "git.openDiffOnClick" = true;
        "git.postCommitCommand" = "none";
        "git.promptToSaveFilesBeforeCommit" = true;
        "git.rebaseWhenSync" = true;
        "git.scanRepositories" = [];
        "git.showInlineOpenFileAction" = true;
        "git.showProgress" = true;
        "git.showPushSuccessNotification" = true;
        "git.useForcePushWithLease" = true;
        "gitHistory.hideCommitViewExplorer" = false;
        "gitHistory.pageSize" = 100;
        "gitHistory.showEditorTitleMenuBarIcons" = true;
        "gitlens.advanced.abbreviatedShaLength" = "7";
        "gitlens.advanced.blame.delayAfterEdit" = 5000;
        "gitlens.advanced.blame.sizeThresholdAfterEdit" = 5000;
        "gitlens.advanced.caching.enabled" = true;
        "gitlens.advanced.fileHistoryFollowsRenames" = true;
        "gitlens.advanced.maxListItems" = 200;
        "gitlens.advanced.messages" = {
            "suppressCommitHasNoPreviousCommitWarning" = false;
            "suppressCommitNotFoundWarning" = false;
            "suppressFileNotUnderSourceControlWarning" = false;
            "suppressGitDisabledWarning" = false;
            "suppressGitVersionWarning" = false;
            "suppressLineUncommittedWarning" = false;
            "suppressNoRepositoryWarning" = false;
            "suppressSupportGitLensNotification" = false;
        };
        "gitlens.advanced.quickPick.closeOnFocusOut" = true;
        "gitlens.advanced.repositorySearchDepth" = 1;
        "gitlens.advanced.telemetry.enabled" = false;
        "gitlens.blame.avatars" = true;
        "gitlens.blame.compact" = true;
        "gitlens.blame.format" = ''
            ''${message|40?} ''${agoOrDate|14-}
        '';
        "gitlens.blame.heatmap.enabled" = true;
        "gitlens.blame.heatmap.location" = "right";
        "gitlens.blame.highlight.enabled" = true;
        "gitlens.blame.highlight.locations" = [
            "gutter"
            "line"
            "overview"
        ];
        "gitlens.blame.ignoreWhitespace" = false;
        "gitlens.blame.separateLines" = true;
        "gitlens.blame.toggleMode" = "file";
        "gitlens.codeLens.authors.command" = "gitlens.toggleFileBlame";
        "gitlens.codeLens.authors.enabled" = true;
        "gitlens.codeLens.enabled" = true;
        "gitlens.codeLens.includeSingleLineSymbols" = true;
        "gitlens.codeLens.recentChange.command" = "gitlens.showQuickCommitFileDetails";
        "gitlens.codeLens.recentChange.enabled" = true;
        "gitlens.codeLens.scopes" = [
            "document"
            "containers"
        ];
        "gitlens.codeLens.scopesByLanguage" = [
            {
                "language" = "azure-pipelines";
                "scopes" = [ "document" ];
            }
            {
                "language" = "css";
                "scopes" = [ "document" ];
            }
            {
                "language" = "html";
                "scopes" = [ "document" ];
            }
            {
                "language" = "json";
                "scopes" = [ "document" ];
            }
            {
                "language" = "jsonc";
                "scopes" = [ "document" ];
            }
            {
                "language" = "less";
                "scopes" = [ "document" ];
            }
            {
                "language" = "postcss";
                "scopes" = [ "document" ];
            }
            {
                "language" = "python";
                "symbolScopes" = [ "!Module" ];
            }
            {
                "language" = "scss";
                "scopes" = [ "document" ];
            }
            {
                "language" = "stylus";
                "scopes" = [ "document" ];
            }
            {
                "language" = "vue";
                "scopes" = [ "document" ];
            }
            {
                "language" = "yaml";
                "scopes" = [ "document" ];
            }
        ];
        "gitlens.codeLens.symbolScopes" = [];
        "gitlens.currentLine.enabled" = true;
        "gitlens.currentLine.format" = ''
            ''${authorAgoOrDate} • ''${message}
        '';
        "gitlens.currentLine.scrollable" = true;
        "gitlens.defaultDateStyle" = "relative";
        "gitlens.defaultGravatarsStyle" = "robohash";
        "gitlens.heatmap.ageThreshold" = "90";
        "gitlens.heatmap.coldColor" = "#0a60f6";
        "gitlens.heatmap.hotColor" = "#f66a0a";
        "gitlens.heatmap.toggleMode" = "file";
        "gitlens.hovers.annotations.changes" = true;
        "gitlens.hovers.annotations.details" = true;
        "gitlens.hovers.annotations.enabled" = true;
        "gitlens.hovers.annotations.over" = "line";
        "gitlens.hovers.avatars" = true;
        "gitlens.hovers.changesDiff" = "hunk";
        "gitlens.hovers.currentLine.changes" = true;
        "gitlens.hovers.currentLine.details" = true;
        "gitlens.hovers.currentLine.enabled" = true;
        "gitlens.hovers.currentLine.over" = "annotation";
        "gitlens.hovers.detailsMarkdownFormat" = ''
            [''${avatar} &nbsp;__''${author}__](mailto:''${email}), ''${ago} &nbsp; _(''${date})_ \n\n''${message}\n\n''${commands}
        '';
        "gitlens.hovers.enabled" = true;
        "gitlens.insiders" = false;
        "gitlens.keymap" = "alternate";
        "gitlens.liveshare.allowGuestAccess" = false;
        "gitlens.menus" = {
            "editor" = {
                "blame" = false;
                "clipboard" = true;
                "compare" = true;
                "details" = false;
                "history" = false;
                "remote" = false;
            };
            "editorGroup" = {
                "blame" = true;
                "compare" = true;
            };
            "editorTab" = {
                "clipboard" = true;
                "compare" = true;
                "history" = true;
                "remote" = true;
            };
            "explorer" = {
                "clipboard" = true;
                "compare" = true;
                "history" = true;
                "remote" = true;
            };
            "scmGroup" = {
                "compare" = true;
                "openClose" = true;
                "stash" = true;
                "stashInline" = true;
            };
            "scmItem" = {
                "clipboard" = true;
                "compare" = true;
                "history" = true;
                "remote" = true;
                "stash" = true;
            };
        };
        "gitlens.mode.statusBar.alignment" = "right";
        "gitlens.mode.statusBar.enabled" = true;
        "gitlens.modes" = {
            "zen" = {
                "name" = "Zen";
                "statusBarItemName" = "Zen";
                "description" = "for a zen-like experience; disables many visual features";
                "codeLens" = false;
                "currentLine" = false;
                "hovers" = false;
                "statusBar" = false;
            };
            "review" = {
                "name" = "Review";
                "statusBarItemName" = "Reviewing";
                "description" = "for reviewing code; enables many visual features";
                "codeLens" = true;
                "currentLine" = true;
                "hovers" = true;
            };
        };
        "gitlens.outputLevel" = "errors";
        "gitlens.recentChanges.toggleMode" = "file";
        "gitlens.statusBar.alignment" = "right";
        "gitlens.statusBar.command" = "gitlens.showQuickCommitDetails";
        "gitlens.statusBar.enabled" = true;
        "gitlens.statusBar.format" = ''''${authorAgoOrDate}'';
        "gitlens.statusBar.reduceFlicker" = true;
        "gitlens.views.commitDescriptionFormat" = ''''${changes  •  }''${authorAgoOrDate}'';
        "gitlens.views.commitFileDescriptionFormat" = ''''${directory}'';
        "gitlens.views.commitFileFormat" = ''''${file}'';
        "gitlens.views.commitFormat" = ''''${message}'';
        "gitlens.views.compare.avatars" = true;
        "gitlens.views.compare.enabled" = true;
        "gitlens.views.compare.files.compact" = true;
        "gitlens.views.compare.files.layout" = "auto";
        "gitlens.views.compare.files.threshold" = 5;
        "gitlens.views.compare.location" = "gitlens";
        "gitlens.views.defaultItemLimit" = 10;
        "gitlens.views.fileHistory.avatars" = true;
        "gitlens.views.fileHistory.enabled" = true;
        "gitlens.views.fileHistory.location" = "gitlens";
        "gitlens.views.lineHistory.avatars" = true;
        "gitlens.views.lineHistory.enabled" = true;
        "gitlens.views.lineHistory.location" = "gitlens";
        "gitlens.views.pageItemLimit" = 20;
        "gitlens.views.repositories.autoRefresh" = true;
        "gitlens.views.repositories.autoReveal" = true;
        "gitlens.views.repositories.avatars" = true;
        "gitlens.views.repositories.branches.layout" = "tree";
        "gitlens.views.repositories.compact" = false;
        "gitlens.views.repositories.enabled" = true;
        "gitlens.views.repositories.files.compact" = true;
        "gitlens.views.repositories.files.layout" = "auto";
        "gitlens.views.repositories.files.threshold" = 5;
        "gitlens.views.repositories.includeWorkingTree" = true;
        "gitlens.views.repositories.location" = "gitlens";
        "gitlens.views.repositories.showTrackingBranch" = true;
        "gitlens.views.search.avatars" = true;
        "gitlens.views.search.enabled" = true;
        "gitlens.views.search.files.compact" = true;
        "gitlens.views.search.files.layout" = "auto";
        "gitlens.views.search.files.threshold" = 5;
        "gitlens.views.search.location" = "gitlens";
        "gitlens.views.showRelativeDateMarkers" = true;
        "gitlens.views.stashDescriptionFormat" = ''''${changes  •  }''${agoOrDate}'';
        "gitlens.views.stashFileDescriptionFormat" = ''''${directory}'';
        "gitlens.views.stashFileFormat" = ''''${file}'';
        "gitlens.views.stashFormat" = ''''${message}'';
        "gitlens.views.statusFileDescriptionFormat" = ''''${directory}'';
        "gitlens.views.statusFileFormat" = ''''${working}''${file}'';
        "go.addTags" = {
            "tags" = "json";
            "options" = "json=omitempty";
            "promptForTags" = false;
            "transform" = "snakecase";
        };
        "go.autocompleteUnimportedPackages" = false;
        "go.buildOnSave" = "package";
        "go.coverageDecorator" = {
            "type" = "highlight";
            "coveredHighlightColor" = "rgba(64,128,128,0.5)";
            "uncoveredHighlightColor" = "rgba(128,64,64,0.25)";
            "coveredGutterStyle" = "blockblue";
            "uncoveredGutterStyle" = "slashyellow";
        };
        "go.coverageOptions" = "showBothCoveredAndUncoveredCode";
        "go.coverOnSave" = false;
        "go.coverOnSingleTest" = false;
        "go.coverOnTestPackage" = true;
        "go.delveConfig" = {
            "dlvLoadConfig" = {
                "followPointers" = true;
                "maxVariableRecurse" = 1;
                "maxStringLen" = 64;
                "maxArrayValues" = 64;
                "maxStructFields" = -1;
            };
            "apiVersion" = 2;
            "showGlobalVariables" = true;
        };
        "go.docsTool" = "godoc";
        "go.editorContextMenuCommands" = {
            "toggleTestFile" = true;
            "addTags" = true;
            "removeTags" = false;
            "testAtCursor" = true;
            "testFile" = false;
            "testPackage" = false;
            "generateTestForFunction" = true;
            "generateTestForFile" = false;
            "generateTestForPackage" = false;
            "addImport" = true;
            "testCoverage" = true;
            "playground" = true;
            "debugTestAtCursor" = true;
        };
        "go.enableCodeLens" = {
            "references" = true;
            "runtest" = true;
        };
        "go.formatTool" = "gofmt";
        "go.gocodeAutoBuild" = false;
        "go.gocodePackageLookupMode" = "go";
        # "go.gopath" = null;
        # "go.goroot" = null;
        # "go.gotoSymbol.ignoreFolders" = [];
        "go.gotoSymbol.includeGoroot" = false;
        "go.gotoSymbol.includeImports" = false;
        "go.inferGopath" = false;
        "go.installDependenciesWhenBuilding" = true;
        "go.languageServerExperimentalFeatures" = {
            "format" = true;
            "autoComplete" = true;
            "rename" = true;
            "goToDefinition" = true;
            "hover" = true;
            "signatureHelp" = true;
            "goToTypeDefinition" = true;
            "goToImplementation" = true;
            "documentSymbols" = true;
            "workspaceSymbols" = true;
            "findReferences" = true;
            "diagnostics" = false;
        };
        "go.lintOnSave" = "package";
        "go.lintTool" = "gometalinter";
        "go.liveErrors" = {
            "enabled" = false;
            "delay" = 500;
        };
        "go.playground" = {
            "openbrowser" = true;
            "share" = true;
            "run" = true;
        };
        "go.removeTags" = {
            "tags" = "";
            "options" = "";
            "promptForTags" = false;
        };
        "go.testOnSave" = false;
        "go.testTimeout" = "30s";
        "go.useCodeSnippetsOnFunctionSuggest" = false;
        "go.useLanguageServer" = true;
        "importMagic.indentWithTabs" = false;
        "importMagic.maxColumns" = 120;
        "importMagic.multiline" = "parentheses";
        "json.format.enable" = true;
        "json.trace.server" = "off";
        "merge-conflict.autoNavigateNextConflict.enabled" = false;
        "merge-conflict.codeLens.enabled" = true;
        "merge-conflict.decorators.enabled" = true;
        "org.addLeftZero" = true;
        "org.clockInOutSeparator" = "--";
        "org.clockTotalSeparator" = " =>  ";
        "org.todoKeywords" = [ # TODO: sync with actual tags from emacs config
            "TODO"
            "DONE"
        ];
        "pascal.codeNavigation" = "workspace";
        "pascal.tags.autoGenerate" = true;
        "pascal.format.indent" = 2;
        "pascal.format.wrapLineLength" = 120;
        "projectManager.any.baseFolders" = []; # !!!!!!
        "projectManager.any.ignoredFolders" = [
            "node_modules"
            "out"
            "typings"
            "test"
        ];
        "projectManager.any.maxDepthRecursion" = 4;
        "projectManager.cacheProjectsBetweenSessions" = true;
        "projectManager.checkInvalidPathsBeforeListing" = true;
        "projectManager.filterOnFullPath" = false;
        "projectManager.git.baseFolders" = [];
        "projectManager.git.ignoredFolders" = [
            "node_modules"
            "out"
            "typings"
            "test"
            ".haxelib"
        ];
        "projectManager.git.maxDepthRecursion" = 4;
        "projectManager.groupList" = false;
        "projectManager.projectsLocation" = "${config.dev.workspacePath}";
        "projectManager.removeCurrentProjectFromList" = true;
        "projectManager.showProjectNameInStatusBar" = true;
        "projectManager.sortList" = "Name";
        "python.analysis.diagnosticPublishDelay" = 1000;
        # "python.analysis.disabled" = [];
        # "python.analysis.errors" = [];
        # "python.analysis.information" = [];
        "python.analysis.logLevel" = "Error";
        "python.analysis.openFilesOnly" = true;
        "python.analysis.symbolsHierarchyDepthLimit" = 10;
        # "python.analysis.typeshedPaths" = [];
        # "python.analysis.warnings" = [];
        "python.autoComplete.addBrackets" = true;
        "python.autoComplete.extraPaths" = [ "/home/alex3rd/.virtualenvs/emias-middlend/lib" ];
        "python.autoComplete.showAdvancedMembers" = true;
        # "python.autoComplete.typeshedPaths" = [];
        "python.autoUpdateLanguageServer" = false;
        "python.dataScience.enabled" = false;
        "python.diagnostics.sourceMapsEnabled" = false;
        "python.disableInstallationCheck" = false;
        # "python.envFile" = "${workspaceFolder}/.env";
        "python.formatting.provider" = "yapf";
        "python.formatting.yapfPath" = "${pkgs.yapf}/bin/yapf";
        "python.jediEnabled" = false;
        "python.linting.banditEnabled" = false; # TODO: package into Nixpkgs, then enable
        "python.linting.enabled" = true;
        "python.linting.flake8CategorySeverity.E" = "Error";
        "python.linting.flake8CategorySeverity.F" = "Error";
        "python.linting.flake8CategorySeverity.W" = "Warning";
        "python.linting.flake8Enabled" = true;
        "python.linting.flake8Path" = "${pkgs.python3Packages.flake8}/bin/flake8";
        "python.linting.ignorePatterns" = [
            ".vscode/*.py"
            "**/site-packages/**/*.py"
        ];
        "python.linting.lintOnSave" = true;
        "python.linting.maxNumberOfProblems" = 100;
        "python.linting.mypyCategorySeverity.error" = "Error";
        "python.linting.mypyCategorySeverity.note" = "Information";
        "python.linting.mypyEnabled" = true;
        "python.linting.mypyPath" = "${pkgs.mypy}/bin/mypy";
        "python.linting.prospectorEnabled" = false; # TODO: package into Nixpkgs, then enable
        "python.linting.pydocstyleEnabled" = true;
        "python.linting.pydocstylePath" = "${pkgs.python3Packages.pydocstyle}/bin/pydocstyle";
        "python.linting.pylamaEnabled" = true;
        "python.linting.pylamaPath" = "${pkgs.python3Packages.pylama}/bin/pylama";
        "python.linting.pylintCategorySeverity.convention" = "Information";
        "python.linting.pylintCategorySeverity.error" = "Error";
        "python.linting.pylintCategorySeverity.fatal" = "Error";
        "python.linting.pylintCategorySeverity.refactor" = "Hint";
        "python.linting.pylintCategorySeverity.warning" = "Warning";
        "python.linting.pylintEnabled" = true;
        "python.linting.pylintUseMinimalCheckers" = true;
        "python.terminal.activateEnvironment" = true;
        "python.terminal.executeInFileDir" = false;
        "python.testing.autoTestDiscoverOnSaveEnabled" = true;
        "python.testing.debugPort" = 3000;
        "python.venvFolders" = [ # !!!!
            "envs"
            ".pyenv"
            ".direnv"
        ];
        "python.venvPath" = "/home/${userName}/.virtualenvs";
        "python.workspaceSymbols.enabled" = false;
        "pythonPreview.allowAllModules" = true;
        "pythonPreview.codAndNavWidth" = 510;
        "pythonPreview.code.fontFamily" = "Iosevka";
        "pythonPreview.code.fontSize" = 11;
        "pythonPreview.code.lineHeight" = 1.2;
        "pythonPreview.codeFooterDocs.fontFamily" = "verdana, arial, helvetica, sans-serif";
        "pythonPreview.codeFooterDocs.fontSize" = 12;
        "pythonPreview.cumulativeMode" = true;
        "pythonPreview.dark.dict-class-instKey.bgColor" = "#f9906f";
        "pythonPreview.dark.dict-class-instVal.bgColor" = "#b35c44";
        "pythonPreview.dark.highlightedArrow.color" = "#005583";
        "pythonPreview.dark.highlightedStackFrame.bgColor" = "#800080";
        "pythonPreview.dark.list-tuple-setTbl.bgColor" = "#ca6924";
        "pythonPreview.disableHeapNesting" = false;
        "pythonPreview.hideCode" = false;
        "pythonPreview.high-contrast.dict-class-instKey.bgColor" = "#f9906f";
        "pythonPreview.high-contrast.dict-class-instVal.bgColor" = "#b35c44";
        "pythonPreview.high-contrast.highlightedArrow.color" = "#005583";
        "pythonPreview.high-contrast.highlightedStackFrame.bgColor" = "#ff7500";
        "pythonPreview.high-contrast.list-tuple-setTbl.bgColor" = "#ca6924";
        "pythonPreview.light.dict-class-instKey.bgColor" = "#faebbf";
        "pythonPreview.light.dict-class-instVal.bgColor" = "#ffffc6";
        "pythonPreview.light.highlightedArrow.color" = "#005583";
        "pythonPreview.light.highlightedStackFrame.bgColor" = "#e9f1f6";
        "pythonPreview.light.list-tuple-setTbl.bgColor" = "#ffffc6";
        "pythonPreview.maxExecutedLines" = 1000;
        "pythonPreview.showAllFrameLabels" = false;
        "pythonPreview.textualMemoryLabels" = false;
        "pythonPreview.trace" = "off";
        "pythonTestExplorer.testFramework" = "pytest";
        "references.preferredLocation" = "peek";
        "trailing-spaces.backgroundColor" = "rgba(255,0,0,0.3)";
        "trailing-spaces.borderColor" = "rgba(255,100,100,0.15)";
        "trailing-spaces.deleteModifiedLinesOnly" = true;
        "trailing-spaces.highlightCurrentLine" = true;
        "trailing-spaces.includeEmptyLines" = true;
        "trailing-spaces.liveMatching" = true;
        "trailing-spaces.regexp" = "[ \t]+";
        "trailing-spaces.schemeIgnore" = [ "output" ];
        "trailing-spaces.showStatusBarMessage" = true;
        "trailing-spaces.trimOnSave" = true;
    };
in
{
    home-manager.users.alex3rd = {
        programs.vscode = {
            enable = true;
            userSettings = vcsNix //
                           vcsTextEditor //
                           vscWorkbench //
                           vscWindow //
                           vscFeatures //
                           vscApplication //
                           vscExtensions;
            extensions = [
                pkgs.vscode-extensions.bbenoist.Nix
            ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                {
                    name = "python";
                    publisher = "ms-python";
                    version = "2019.4.11987";
                    sha256 = "0iq4pbz4sy7730ap9kbv8b5ncb3jbxpf0ibsyjf3rs6j6jwvdv1v";
                }
                {
                    name = "autodocstring";
                    publisher = "njpwerner";
                    version = "0.3.0";
                    sha256 = "1mlafwzag4l42y070ssgqi34zgg7khb4lc45ycnz0a13ql0f9yx6";
                }
                {
                    name = "trailing-spaces";
                    publisher = "shardulm94";
                    version = "0.3.1";
                    sha256 = "0h30zmg5rq7cv7kjdr5yzqkkc1bs20d72yz9rjqag32gwf46s8b8";
                }
                {
                    name = "gitlens";
                    publisher = "eamodio";
                    version = "9.6.3";
                    sha256 = "0psgvlf3945724l2lf3znr4xlmsk0zcrig48lap2bck43lr7xxfw";
                }
                {
                    name = "Bookmarks";
                    publisher = "alefragnani";
                    version = "10.4.3";
                    sha256 = "1y6y17zcfx56dqnki7yz4awl5b0xwajrlfg2jbwcpwpsi0njz46q";
                }
                {
                    name = "vscode-importmagic";
                    publisher = "brainfit";
                    version = "0.1.3";
                    sha256 = "0vpdcd3z6z0bwpd2832ndgx76fng8kbl6fddi64l8qy7agj2znn6";
                }
                {
                    name = "jinja";
                    publisher = "wholroyd";
                    version = "0.0.8";
                    sha256 = "1ln9gly5bb7nvbziilnay4q448h9npdh7sd9xy277122h0qawkci";
                }
                {
                    name = "python-preview";
                    publisher = "dongli";
                    version = "0.0.4";
                    sha256 = "08z0r8v5nkhg1mx7846p7s8mdnhx7w5ijbmbxav09yicxld04xz7";
                }
                {
                    name = "vscode-django";
                    publisher = "batisteo";
                    version = "0.19.0";
                    sha256 = "1bn9rkdqn9w23yn1sv6rivqirfvmn5xi7sfmj6d4z6qwy2cnwxfj";
                }
                {
                    name = "arepl";
                    publisher = "almenon";
                    version = "1.0.13";
                    sha256 = "1170hfrm6blfzjsk8n6w8l7yi9q5z4z89pgxfwkmkfdqmhpx1avn";
                }
                {
                    name = "vscode-python-test-adapter";
                    publisher = "LittleFoxTeam";
                    version = "0.3.4";
                    sha256 = "1k5y4w2qi0xm5a116kvpprlnpdididp76z4yzhn1xwzrmglnjcjg";
                }
                {
                    name = "vscode-gitignore-generator";
                    publisher = "piotrpalarz";
                    version = "1.0.1";
                    sha256 = "12l2gz73gni3n82dpiaq2sr8348437a6wnzyndaqdmxw5h26rhlb";
                }
                {
                    name = "githistory";
                    publisher = "donjayamanne";
                    version = "0.4.6";
                    sha256 = "1wj838iv1xg25a604j4mccdcqhjjcgpaaq6yhnng1ph0s16ypin1";
                }
                {
                    name = "project-manager";
                    publisher = "alefragnani";
                    version = "10.5.1";
                    sha256 = "1k8l5pyacpld9r76fqynpdx0zkzylvb5lickvxlnql2zb70cxk05";
                }
                {
                    name = "vscode-icons";
                    publisher = "vscode-icons-team";
                    version = "8.6.0";
                    sha256 = "0gcl43pdvkjnkdwc4zbl5xikjzij74m093p9hja4m36bmzvnixkp";
                }
                {
                    name = "django-intellisense";
                    publisher = "shamanu4";
                    version = "0.0.2";
                    sha256 = "146irhf1mfzawl4kbmx9zzs8rb9yvi6wr83mm8hhh3f0ihkdxdrn";
                }
                {
                    name = "jinjahtml";
                    publisher = "samuelcolvin";
                    version = "0.10.1";
                    sha256 = "0z3g3dd4ixrphf31jvacr5bsywhhnkzpdapgr3ncyr1cr19d9vbf";
                }
                {
                    name = "vscode-awesome-snippets";
                    publisher = "xindzju";
                    version = "0.2.1";
                    sha256 = "02v8ky8gavfw8gi7pf1amyzhmrmyvx8hw947q8ya9qbmxy1bdi7q";
                }
                {
                    name = "python-path";
                    publisher = "mgesbert";
                    version = "0.0.11";
                    sha256 = "06m2daywn234maicm4p9w1kz58d61fkvqjvcybkglkj91japj7mn";
                }
                {
                    name = "omnipascal";
                    publisher = "Wosi";
                    version = "0.17.1";
                    sha256 = "0wn322vwd0jhwqsh5fzdsiiywmrrx0i9fr2wrd6c5qxwkivhiic1";
                }
                {
                    name = "pascal";
                    publisher = "alefragnani";
                    version = "8.0.1";
                    sha256 = "13kla3wlqcgxqmln2qchag7j6z54ckzi0g5x3jggf050gvm16v5k";
                }
                {
                    name = "pascal-formatter";
                    publisher = "alefragnani";
                    version = "2.2.0";
                    sha256 = "1zz5zb2ma9m54hjy69pd9j4d3dq492g3vsz27jwvlzzzqwxvy30g";
                }
                {
                    name = "zenburn";
                    publisher = "ryanolsonx";
                    version = "1.0.1";
                    sha256 = "1gb159fkvadsnx5gb3l8pps9sa5xjcaqqiym952ar0yfhpfd0ziz";
                }
                {
                    name = "vscode-docker";
                    publisher = "PeterJausovec";
                    version = "0.6.1";
                    sha256 = "0clxy66qi5c3k5di5xsjm3vjib525xq89z1q2h3a5x5qwvbvd0mj";
                }
                {
                    name = "docker-compose";
                    publisher = "p1c2u";
                    version = "0.3.4";
                    sha256 = "1kpsxkridsvbhx7c7jlpyh4k0vhmr503fw2csh4vy3fc5f96x9ws";
                }
                {
                    name = "Go";
                    publisher = "ms-vscode";
                    version = "0.10.1";
                    sha256 = "1gqpqivfg046s9sydjndm8pnfc4q4m9412dl56fc0f2rb7xfgsbn";
                }
                {
                    name = "rust";
                    publisher = "rust-lang";
                    version = "0.6.1";
                    sha256 = "0f66z6b374nvnrn7802dg0xz9f8wq6sjw3sb9ca533gn5jd7n297";
                }
                {
                    name = "org-mode";
                    publisher = "vscode-org-mode";
                    version = "1.0.0";
                    sha256 = "1dp6mz1rb8awrrpig1j8y6nyln0186gkmrflfr8hahaqr668il53";
                }
                {
                    name = "vscode-emacs-improved";
                    publisher = "rkwan94";
                    version = "1.1.0";
                    sha256 = "0574my67m6vlj4fyr590d1m3v69b2agjh0f568gzxsy0c4ij606b";
                }
            ];
        };
    };
}
