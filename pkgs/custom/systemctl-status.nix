{ bash, gawk, systemd, ... }: ''
    #!${bash}/bin/bash

    if [ $# -le 1 ]; then
        echo -e ""
    else
        if [[ $# == 2 ]]; then
            if [[ $2 =~ ^\[ ]]; then
                STATUS=`${systemd}/bin/systemctl status $1 | ${gawk}/bin/awk 'NR==3 {print $2}'`
            else
                echo -e ""
            fi
        elif [[ $# == 3 ]]; then
            STATUS=`${systemd}/bin/systemctl --user status $1 | ${gawk}/bin/awk 'NR==3 {print $2}'`
        fi
        if [ $STATUS == "inactive" ]
        then
            echo -e ""
        else
            if [ -z "$2" ]
            then
                echo -e "[*]"
            else
                echo -e $2
            fi
        fi
    fi
''
