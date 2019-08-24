{config, pkgs, lib, ...}:
with import ../../../../pkgs/util.nix {inherit config pkgs lib;};
with import ../../../../pkgs/const.nix {inherit config pkgs;};
with import ../../const.nix {inherit config pkgs;};
with import ../../secrets/const.nix {inherit config pkgs lib;};
let
    custom = import ../../../../pkgs/custom pkgs config;
in
{
    environment.etc."nixos/.hooks/pre-push/stop-wip" = {
        mode = "0644";
        user = "${userName}";
        group = "users";
        text = ''
            . ${custom.git_lib}/bin/git_lib

            check_for_wip
            exit $?
        '';
    };
    home-manager.users."${userName}" = {
        home.file = {
            ".mrtrust".text = ''
                ${jobWorkspacePath}/.mrconfig
            '';
            # TODO: review https://github.com/RichiH/myrepos/blob/master/mrconfig.complex
            ".mrconfig".text = genIniHum {
                "DEFAULT" = {
                    "update" = indentedLines [
                        "${pkgs.git}/bin/git fetch origin"
                        "${pkgs.git}/bin/git rebase origin/$(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)"
                    ] "    ";
                    "savewip" = indentedLines [
                        "if [[ ! -z $(${pkgs.git}/bin/git status --porcelain) ]]; then"
                        "    ${pkgs.git}/bin/git add ."
                        "    ${pkgs.git}/bin/git commit -m \"WIP $(${pkgs.coreutils}/bin/date -R)\""
                        "else"
                        "    return 0"
                        "fi"
                    ] "    ";
                    "push" = indentedLines [
                        "if [[ $(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD) =~ master ]]; then"
                        "    echo master is active, skipping..."
                        "else"
                        "    ${pkgs.git}/bin/git push origin $(${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD)"
                        "fi"
                    ] "    ";
                    "usync" = indentedLines [
                        "if [[ ! -z $(${pkgs.git}/bin/git remote | grep ${defaultUpstreamRemoteName}) ]]; then"
                        "    ${pkgs.git}/bin/git fetch ${defaultUpstreamRemoteName}"
                        "    current_branch=$(${pkgs.git}/bin/git branch | ${pkgs.gnugrep}/bin/grep \* | ${pkgs.coreutils}/bin/cut -d ' ' -f2)"
                        "    ${pkgs.git}/bin/git merge ${defaultUpstreamRemoteName}/$current_branch"
                        "else"
                        "    echo No upstream defined, skipping"
                        "    return 0"
                        "fi"
                    ] "    ";
                };
            } + ''
                include = cat ${jobWorkspacePath}/.mrconfig
            '';
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
                .agignore
                .dir-locals.el

                .mypy_cache/*

                ${gitRepoHooks}
            '';
            "git-assets/templates/hooks/pre-push" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash
                    # TODO: templatize further
                    . ${custom.git_lib}/bin/git_lib
                    execute_hook_items pre-push
                    exit $?;
                '';
            };
            "git-assets/templates/hooks/pre-commit" = {
                executable = true;
                text = ''
                    #!${pkgs.bash}/bin/bash
                    # TODO: templatize further
                    . ${custom.git_lib}/bin/git_lib
                    execute_hook_items pre-commit
                    exit $?;
                '';
            };
        };
        xdg.configFile."pass-git-helper/git-pass-mapping.ini".text = ''
            [github.com*]
            target=${userName}/webservices/social/programming/github.com

            [bitbucket.org*]
            target=${userName}/webservices/social/programming/bitbucket.com
        '';
        programs.git = {
            enable = true;
            userName = userFullName;
            userEmail = userEmail;
            signing = {
                key = userPrimaryGpgKeyID;
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
                    template = "/home/${userName}/git-assets/git-commit-template";
                };
                "rebase" = {
                    autoSquash = true;
                    autoStash = true;
                };
                "core" = {
                    autocrlf = false;
                    excludesfile = "/home/${userName}/git-assets/.gitignore";
                    quotepath = false;
                    askPass = "";
                    hooksPath = "/home/${userName}/git-assets/templates/hooks";
                };
                "credential" = {
                    helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
                };
                "diff" = {
                    algorithm = "patience";
                };
                "init" = {
                    templatedir = "/home/${userName}/git-assets/templates";
                };
                "clone" = {
                    templatedir = "/home/${userName}/git-assets/templates";
                };
                "pager" = {
                    diff = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
                    show = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
                    log = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
                };
                "push" = {
                    default = "current";
                };
                "ghq" = {
                    root = "/home/${userName}/workspace/repos";
                };
                "ghq \"import\"" = {
                    bbcontribs = "${custom.bitbucket_team_contributor_repos}";
                };
                "github" = {
                    user = "wiedzmin";
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
