{config, pkgs, lib, ...}:

let
    warningsOrgFile = "${config.users.extraUsers.alex3rd.home}/docs/org/tasks.org";
    gitSrcStopSnippets = [
        "ipdb"
        "wdb"
        "^\\s*print("
    ];
    gitCommitStopWords = [
        "wip"
        "WIP"
    ];
in
{
    home-manager.users.alex3rd = {
        home.file = {
            "git-assets/git-commit-template".text = ''


                # <type>: (If applied, this commit will...) <subject> (Max 50 char)
                # |<----  Using a Maximum Of 50 Characters  ---->|

                # Explain why this change is being made
                # |<----   Try To Limit Each Line to a Maximum Of 72 Characters   ---->|

                # Provide links or keys to any relevant tickets, articles or other resources
                # Example: Github issue #23

                # --- COMMIT END ---
                # Type can be
                #    feat     (new feature)
                #    fix      (bug fix)
                #    refactor (refactoring production code)
                #    style    (formatting, missing semi colons, etc; no code change)
                #    docs     (changes to documentation)
                #    test     (adding or refactoring tests; no production code change)
                #    chore    (updating grunt tasks etc; no production code change)
                # --------------------
                # Remember to
                #    Capitalize the subject line
                #    Use the imperative mood in the subject line
                #    Do not end the subject line with a period
                #    Separate subject from body with a blank line
                #    Use the body to explain what and why vs. how
                #    Can use multiple lines with "-" for bullet points in body
                # --------------------
                # For more information about this template, check out
                # https://gist.github.com/adeekshith/cd4c95a064977cdc6c50
            '';
            "git-assets/.gitignore".text = ''
                # reference ignore list / inspired by Github
                *.o
                *.ko
                *.so
                *.obj
                *.elf

                *.lib
                *.a
                *.la
                *.lo

                *.dll
                *.so
                *.so.*
                *.dylib

                *.exe
                *.out
                *.app
                *.i*86
                #.x86_64
                *.hex
                *.dSYM/

                # emacs
                *~
                \#*\#
                *.elc
                auto-save-list
                .\#*

                # Autotools
                Makefile.in
                /autom4te.cache
                /aclocal.m4
                /compile
                /configure
                /depcomp
                /install-sh
                /missing
                /m4
                /stamp-h1

                # various
                .DS_Store
                *.out
                /.DS_Store
                *.swp
                cache/*
                page_cache/*
                *.patch
                .agignore
                .dir-locals.el
            '';
            "git-assets/templates/hooks/post-commit" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash

                    CURRENT_REV=`${pkgs.git}/bin/git rev-parse HEAD`
                    PREVIOUS_REV=`${pkgs.git}/bin/git rev-parse HEAD^1`

                    OUTFILE="${warningsOrgFile}"
                    THRESHOLD=250

                    MESSAGE="** commit ''${CURRENT_REV} deleted more than ''${THRESHOLD} lines in a file!"

                    DETAILS="#+BEGIN_SRC sh :results output
                    cd ${builtins.dirOf warningsOrgFile}
                    echo \"commit ''${CURRENT_REV}\"
                    ${pkgs.git}/bin/git diff --stat \"''${PREVIOUS_REV}\" \"''${CURRENT_REV}\"
                    #+END_SRC"

                    ${pkgs.git}/bin/git diff --numstat "''${PREVIOUS_REV}" "''${CURRENT_REV}" | \
                      cut -f 2 | \
                      while read line
                        do test "$line" -gt "''${THRESHOLD}" && \
                          echo "''${MESSAGE}\n<`date '+%Y-%m-%d %H:%M'` +1d>\n\n''${DETAILS}\n" >> \
                          "''${OUTFILE}"; \
                        done
                '';
            };
            "git-assets/templates/hooks/pre-push" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash

                    IS_CLEAN=true

                    for i in ${lib.concatMapStringsSep " " (w: "\"" + w + "\"") gitSrcStopSnippets}
                    do
                        RESULTS=$(${pkgs.git}/bin/git grep -C 2 -n -G "$i" -- `${pkgs.git}/bin/git ls-files | grep -v hook`);
                        if [[ ! -z "$RESULTS" ]]; then
                            IS_CLEAN=false
                            echo "Found stop snippets:"
                            echo "$RESULTS"
                        fi
                    done

                    for i in ${lib.concatMapStringsSep " " (w: "\"" + w + "\"") gitCommitStopWords}
                    do
                        RESULTS=$(${pkgs.git}/bin/git shortlog "@{u}.." | grep "$i");
                        if [[ ! -z "$RESULTS" ]]; then
                            IS_CLEAN=false
                            echo "Found commits with stop snippets:"
                            echo "$RESULTS"
                        fi
                    done

                    if [[ "$IS_CLEAN" = false ]]; then
                        exit 1;
                    fi
                '';
            };
            ".config/pass-git-helper/git-pass-mapping.ini".text = ''
                [github.com*]
                target=alex3rd/webservices/social/programming/github.com

                [bitbucket.org*]
                target=alex3rd/webservices/social/programming/bitbucket.com
            '';
        };
        programs.git = {
            enable = true;
            userName = config.common.userName;
            userEmail = config.common.userEmail;
            signing = {
                key = config.common.primaryGpgKeyID;
                signByDefault = true;
            };
            extraConfig = {
                "color" = {
                    diff = "auto";
                    status = "auto";
                    ui = "always";
                };
                "color \"branch\"" = {
                    current = "yellow reverse";
                    local = "yellow";
                    remote = "green";
                };
                "color \"diff\"" = {
                    commit = "cyan bold";
                    frag = "magenta bold";
                    meta = "yellow bold";
                    new = "green bold";
                    old = "red bold";
                    whitespace = "red reverse";
                };
                "color \"diff-highlight\"" = {
                    newHighlight = "green bold 22";
                    newNormal = "green bold";
                    oldHighlight = "red bold 52";
                    oldNormal = "red bold";
                };
                "color \"status\"" = {
                    added = "green";
                    changed = "yellow";
                    untracked = "red";
                };
                "commit" = {
                    template = "${config.users.extraUsers.alex3rd.home}/git-assets/git-commit-template";
                };
                "rebase" = {
                    autoSquash = true;
                };
                "core" = {
                    autocrlf = false;
                    excludesfile = "${config.users.extraUsers.alex3rd.home}/git-assets/.gitignore";
                    quotepath = false;
                    askPass = "";
                    hooksPath = "${config.users.extraUsers.alex3rd.home}/git-assets/templates/hooks";
                };
                "credential" = {
                    helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
                };
                "diff" = {
                    algorithm = "patience";
                };
                "init" = {
                    templatedir = "${config.users.extraUsers.alex3rd.home}/git-assets/templates";
                };
                "clone" = {
                    templatedir = "${config.users.extraUsers.alex3rd.home}/git-assets/templates";
                };
                "pager" = {
                    diff = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX";
                    show = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX";
                    log = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | less --tabs=4 -RFX";
                };
                "push" = {
                    default = "current";
                };
                "ghq" = {
                    root = "${config.users.extraUsers.alex3rd.home}/workspace/repos";
                };
                "ghq \"import\"" = {
                    bbcontribs = "${pkgs.bitbucket_team_contributor_repos}/bin/bitbucket_team_contributor_repos";
                };
            };
            aliases = {
                co = "checkout";
                cwo = "checkout -b origin/"; # fetch branch from primary remote, eliminates ambiguity

                bl = "branch -l";
                merged = "branch --merged master";
                nomerged = "branch --no-merged master";
                fro = "!f() { git fetch && git rebase '@{u}'}; f";

                cs = "commit -S";

                undo = "reset HEAD~1";
                hundo = "reset --hard HEAD~1";

                cont = "shortlog -n -s";
                fc = "!f() { git log --pretty=format:'* %C(yellow)%h%Creset -%C(red)%d%Creset %s %Cgreen(%ad) %C(bold blue)<%cn>%Creset' --decorate --date=relative --grep=$1; }; f";
                hist = "log --pretty=format:\"%h %ad | %s%d [%an]\" --graph --date=short";
                lg = "log --graph --pretty='%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
                who = "shortlog -n -s --no-merges";
                sl = "log --name-only --oneline";
                updated = "show --name-only --oneline";

                # TODO: think of using --patience
                d = "!git diff --patch-with-stat --color $@ | ${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy";
                dc = "diff --cached";
                df = "diff --patch-with-stat --color --color-words --abbrev";
                fpd = "diff --no-prefix -s -p >";
                last = "diff --stat=150,120 HEAD^ HEAD";
                pd = "diff --no-prefix -s -p >";

                pf = "format-patch -1 --no-prefix -s -p FETCH_HEAD";

                s = "status -s";
                st = "status";

                remotes = "remote -v";
                sclone = "clone --depth 1";

                trycl = "git clean -d -x -n -i";
                cleanup = "clean -d -x -f";
                tcleanup = "remote prune origin && git gc && git clean -dfx && git stash clear";
            };
        };
    };
}
