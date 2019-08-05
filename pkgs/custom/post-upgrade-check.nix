{ bash, systemd, ... }: ''
    #!${bash}/bin/bash

    current=$(readlink -f /run/current-system/kernel)
    booted=$(readlink -f /run/booted-system/kernel)
    if [ "$current" != "$booted" ]; then
        echo "kernel changed, reboot" | ${systemd}/bin/systemd-cat --identifier "post-upgrade-check";
        reboot
    else
        echo "same kernel, do not reboot" | ${systemd}/bin/systemd-cat --identifier "post-upgrade-check";
    fi
''
