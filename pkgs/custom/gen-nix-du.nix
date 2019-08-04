{ bash, nix-du, graphviz, ... }: ''
    #!${bash}/bin/bash
    set -eu

    # TODO: think of moving these params to module level
    nixDuBasedir=/tmp
    nixDuFilename=nix-du
    nixDuFileFormat=svg
    nixDuSizeThreshold=500MB

    ${nix-du}/bin/nix-du -s $nixDuSizeThreshold | \
    ${graphviz}/bin/dot -T$nixDuFileFormat > $nixDuBasedir/$nixDuFilename.$nixDuFileFormat
''
