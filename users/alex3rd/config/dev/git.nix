{ config, pkgs, lib, ... }:
with import ../../../../pkgs/util.nix { inherit config lib; };
with import ../../../../pkgs/const.nix { inherit config pkgs; };
with import ../../const.nix { inherit config pkgs; };
with import ../../secrets/const.nix { inherit config pkgs lib; };
let custom = import ../../../../pkgs/custom pkgs config;
in {
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
      "git-assets/.gitignore".text = ''
        # reference ignore list / inspired by Github
        # emacs
        *.elc
        .dir-locals.el

        # various
        *.out
        *.swp
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
    xdg.configFile."pass-git-helper/git-pass-mapping.ini".text = genIni {
      "github.com*" = {
        target = "${userName}/webservices/social/programming/github.com";
      };
      "bitbucket.org*" = {
        target = "${userName}/webservices/social/programming/bitbucket.com";
      };
    };
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
        "credential" = { helper = "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper"; };
        "diff" = { algorithm = "patience"; };
        "init" = { templatedir = "/home/${userName}/git-assets/templates"; };
        "clone" = { templatedir = "/home/${userName}/git-assets/templates"; };
        "pager" = {
          diff = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
          show = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
          log = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less --tabs=4 -CR";
        };
        "push" = { default = "current"; };
        "ghq" = { root = "/home/${userName}/workspace/repos"; };
        "ghq \"import\"" = { bbcontribs = "${custom.bitbucket_team_contributor_repos}"; };
        "github" = { user = "wiedzmin"; };
      };
      aliases = {
        bl = "branch -l";
        merged = "branch --merged master";
        nomerged = "branch --no-merged master";

        undo = "reset HEAD~1";
        hundo = "reset --hard HEAD~1";

        who = "shortlog -n -s --no-merges";
        sl = "log --name-only --oneline";

        # TODO: think of using --patience
        df = "diff --patch-with-stat --color --color-words --abbrev";
        last = "diff --stat=150,120 HEAD^ HEAD";

        pf = "format-patch -1 --no-prefix -s -p FETCH_HEAD";

        remotes = "remote -v";
        slice = "clone --depth 1";
      };
    };
  };
}
