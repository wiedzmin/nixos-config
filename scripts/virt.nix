{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            docker-machine-export = pkgs.writeShellScriptBin "docker-machine-export" ''
                if [ -z "$1" ]; then
                  echo "Usage: machine-export.sh MACHINE_NAME"
                  echo ""
                  echo "Exports the specified docker-machine to a MACHINE_NAME.zip file"
                  echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
                  exit 0
                fi

                machine_name=$1

                ${pkgs.docker-machine}/bin/docker-machine status $machine_name 2>&1 > /dev/null
                if [ $? -ne 0 ]; then
                  echo "No such machine found"
                  exit 1
                fi

                set -e

                MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
                machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"
                tmp_path="/tmp/machine-export-$(date +%s%3)"

                # copy to /tmp and strip out $MACHINE_STORAGE_PATH
                ${pkgs.coreutils}/bin/mkdir -p $tmp_path
                ${pkgs.coreutils}/bin/cp -r "$machine_path" "$tmp_path"
                ${pkgs.perl}/bin/perl -pi -e "s|$MACHINE_STORAGE_PATH|__MACHINE__STORAGE_PATH__|g" $tmp_path/$machine_name/config.json

                # create zip
                ${pkgs.coreutils}/bin/rm -f "$machine_name.zip"
                ${pkgs.zip}/bin/zip -rj "$machine_name.zip" "$tmp_path/$machine_name" > /dev/null

                echo "Exported machine to $machine_name.zip"

                # cleanup
                ${pkgs.coreutils}/bin/rm -rf $tmp_path
            '';
            docker-machine-import = pkgs.writeShellScriptBin "docker-machine-import" ''
                set -e

                if [ -z "$1" ]; then
                  echo "Usage: docker-machine-import.sh MACHINE_NAME.zip"
                  echo ""
                  echo "Imports an exported machine from a MACHINE_NAME.zip file"
                  echo "Note: This script requires you to have the same \$MACHINE_STORAGE_PATH/certs available on all host systems"
                  exit 0
                fi

                machine_archive="$1"
                machine_name="$machine_archive/.zip/"
                MACHINE_STORAGE_PATH="$(MACHINE_STORAGE_PATH:-"$HOME/.docker/machine")"
                machine_path="$MACHINE_STORAGE_PATH/machines/$machine_name"

                if [ -d "$machine_path" ]; then
                  echo "$machine_name already exists"
                  exit 1
                fi

                ${pkgs.coreutils}/bin/rm -rf "$machine_name"
                ${pkgs.unzip}/bin/unzip "$machine_archive" -d "$machine_name" > /dev/null
                ${pkgs.perl}/bin/perl -pi -e "s|__MACHINE__STORAGE_PATH__|$MACHINE_STORAGE_PATH|g" $machine_name/config.json
                ${pkgs.coreutils}/bin/mv "$machine_name" "$MACHINE_STORAGE_PATH/machines"

                echo "Imported $machine_name to docker-machine ($machine_path)"
            '';
        };
    };
}
