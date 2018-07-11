{config, pkgs, ...}:
{
    imports = [
        <home-manager/nixos>
    ];

    users.extraUsers = {
        alex3rd = {
            isNormalUser = true;
            uid = 1000;
            description = "Alex Ermolov";
            shell = pkgs.zsh;
            extraGroups = [ "audio" "docker" "lp" "networkmanager" "scanner" "vboxusers" "video" "wheel" ];
        };
    };
    programs.zsh = {
        enable = true;
        ohMyZsh = {
            enable = true;
            plugins = [
                "colored-man-pages"
                "dirpersist"
            ];
            theme = "simple";
        };
        autosuggestions.enable = true;
        enableCompletion = true;
        interactiveShellInit = ''
            PATH=$PATH:${pkgs.autojump}/bin
            . ${pkgs.autojump}/share/autojump/autojump.zsh
        '';
    };
    programs.zsh.zsh-autoenv = {
        enable = true;
        package = pkgs.zsh-autoenv;
    };
    programs.bash.enableCompletion = true;

    home-manager.users.alex3rd = {
        programs.git = {
            enable = true;
            userName = "Alex Ermolov";
            userEmail = "aaermolov@gmail.com";
            signing = {
                key = "alex3rd <aaermolov@gmail.com>";
                signByDefault = true;
            };
            extraConfig = {
                color.diff = "auto";
                color.status = "auto";
                color.ui = "always";
                commit.gpgSign = true;
                commit.template = "${config.users.extraUsers.alex3rd.home}/.git/git-commit-template";
                core.autocrlf = false;
                core.excludesfile = "${config.users.extraUsers.alex3rd.home}/.git/.gitignore";
                core.quotepath = false;
                credential.helper = "cache --timeout=3600";
                diff.algorithm = "patience";
                gpg.program = "gpg2";
                init.templatedir = "${config.users.extraUsers.alex3rd.home}/.git/templates";
                pager.diff = "diff-so-fancy | less --tabs=4 -RFX";
                pager.show = "diff-so-fancy | less --tabs=4 -RFX";
                push.default = "current";
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
                d = "!git diff --patch-with-stat --color $@ | diff-so-fancy";
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
