{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
let
    git-save-wip-batch = pkgs.writeShellScriptBin "git-save-wip-batch" ''
        GIT_REPOS=
        ADJUST_GIT_REPOS=
        case $# in
            0 )
                exit 1
                ;;
            1 )
                BASE_PATH=$1
                GIT_REPOS=$(${pkgs.fd}/bin/fd ".git" --type d -H $BASE_PATH)
                ADJUST_GIT_REPOS=1
                ;;
            * )
                GIT_REPOS=$@
                ADJUST_GIT_REPOS=0
                ;;
        esac

        if [[ $(DISPLAY=:0 ${pkgs.xprintidle-ng}/bin/xprintidle-ng) -lt $((3600*1000)) ]]; then
            exit 0
        fi

        set -x

        for ITEM in $GIT_REPOS
        do
            echo "$ITEM"
            if [[ $ADJUST_GIT_REPOS -eq 1 ]]; then
                 cd $ITEM/..
            else
                 cd $ITEM
            fi
            echo "Processing $(basename `pwd`)..."
            ${pkgs.stgit}/bin/stg init
            ${pkgs.stgit}/bin/stg repair

            if [[ ! -z $(${pkgs.git}/bin/git status --porcelain) ]]; then
                ${pkgs.git}/bin/git add .
                PATCH_DESC="WIP $(date -R)"
                ${pkgs.stgit}/bin/stg new -m "$PATCH_DESC"
                ${pkgs.stgit}/bin/stg refresh
            fi
        done
    '';
in
{
    imports = [
        ../private/job.nix
        ../private/dev.nix
    ];
    systemd.services."git-fetch-updates-work" = let
        git-fetch-batch = pkgs.writeShellScriptBin "git-fetch-batch" ''
            BASE_PATH=$1
            if [[ ! -n $BASE_PATH ]]
            then
                exit 1
            fi

            set -x

            for ITEM in $(${pkgs.fd}/bin/fd ".git" --type d -H $BASE_PATH)
            do
                cd $ITEM/..
                echo "Processing $(basename `pwd`)..."
                REMOTES=$(${pkgs.git}/bin/git remote)
                if [[ "origin" =~ $REMOTES ]]; then
                    SLEEPSEC=2
                    RET=1
                    until [ ''${RET} -eq 0 ]; do
                        ${pkgs.git}/bin/git fetch origin &
                        wait $!
                        RET=$?
                        if [ ''${RET} -ne 0 ]; then
                            echo Failed fetching origin, retrying in $SLEEPSEC seconds...
                            sleep $SLEEPSEC
                            SLEEPSEC="$((SLEEPSEC * 2))"
                        fi
                    done
                    ${pkgs.git}/bin/git rebase --autostash &
                    wait $!
                fi
            done
        '';
    in
    {
        description = "Fetch updates from work git repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper ];
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "${git-fetch-batch}/bin/git-fetch-batch ${config.job.workspacePath}";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-fetch-updates-work" = {
        description = "Fetch updates from work git repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "1hour";
        };
    };
    systemd.services."git-save-wip-work" = {
        description = "Commit WIP changes to work repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper pkgs.stgit ];
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "${git-save-wip-batch} ${config.job.workspacePath}";
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-save-wip-work" = {
        description = "Commit WIP changes to work repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "30min";
        };
    };
    systemd.services."git-save-wip-pets" = {
        description = "Commit WIP changes to pet projects' repos";
        path = [ pkgs.pass pkgs.gitAndTools.pass-git-helper pkgs.stgit ];
        serviceConfig = {
            Type = "oneshot";
            User = "${userName}";
            ExecStart = "${git-save-wip-batch}" + (lib.concatMapStrings (loc: " ${loc}") config.dev.petProjectLocations);
            StandardOutput = "journal+console";
            StandardError = "inherit";
        };
    };
    systemd.timers."git-save-wip-pets" = {
        description = "Commit WIP changes to pet projects' repos";
        wantedBy = [ "timers.target" ];
        timerConfig = {
            OnBootSec = "1min";
            OnUnitActiveSec = "30min";
        };
    };
}
