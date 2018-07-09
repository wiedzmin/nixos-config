{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            hddtemp = pkgs.writeShellScriptBin "hddtemp" ''
                nc localhost 7634 | awk -F\| '{print ($4)}'
            '';
            status_bat_info = pkgs.writeShellScriptBin "status_bat_info" ''
                BAT_NAME="BAT0"
                UPOWER_ENERGY_FULL_DESIGN=$(upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "energy-full-design:" | cut -f 14 -d " ")
                UPOWER_ENERGY_FULL_FACT=$(upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "energy-full:" | cut -f 14 -d " ")
                UPOWER_PERCENTAGE=$(upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "percentage:" | cut -f 15 -d " ")
                UPOWER_STATE=$(upower -i /org/freedesktop/UPower/devices/battery_$BAT_NAME | grep "state:" | cut -f 20 -d " ")
                case $UPOWER_STATE in
                    fully-charged)
                        st="="
                        ;;
                    discharging)
                        st="▼"
                        ;;
                    charging)
                        st="▲"
                        ;;
                esac
                echo $st$UPOWER_PERCENTAGE
            '';
            status_cpu_temp = pkgs.writeShellScriptBin "status_cpu_temp" ''
                t=$(${pkgs.lm_sensors}/bin/sensors | ${pkgs.gawk}/bin/awk '/Core\ 0/ {gsub(/\+/,"",$3); gsub(/\..+/,"",$3)    ; print $3}')
                tc=$C0
                case 1 in
                    $((t <= 50)))
                        tc=$C2
                        ;;
                    $((t >= 87)))
                        tc=$C3
                        ;;
                    $((t >= 105)))
                        tc=$C4
                        ;;
                esac
                echo "$t°C"
            '';
            status_uptime = pkgs.writeShellScriptBin "status_uptime" ''
                uptime | cut -f 4-7 -d " " | cut -f 1-2 -d ","
            '';
            zip2targz = pkgs.writeShellScriptBin "zip2targz" ''
                tmpdir=`mktemp -d`
                #Copy the zip to the temporary directory
                cp "$1" $tmpdir/
                #Unzip
                (cd $tmpdir && unzip -q "$1")
                #Remove the original zipfile because we don't want that to be tar'd
                rm "$tmpdir/$1"
                #Tar the files
                outfilename=$(echo "$1" | rev | cut -d. -f2- | rev).tar
                (cd $tmpdir && tar cf "$outfilename" *)
                mv "$tmpdir/$outfilename" .
                gzip ./$outfilename
                #Remove the temporary directory
                rm -rf $tmpdir
                #Print what we did
                echo "Converted $1 to $outfilename.gz"
            '';
        };
    };
}
