{ lib, pkgs, ... }:

rec {
  genCustomPackages = customPackages:
    ''
    ${builtins.concatStringsSep "\n"
      (lib.mapAttrsToList (feature: impl:
        let
          def = pkgs.writeTextFile {
            name = "${feature}";
            text = ''
              ${impl}
              (provide '${feature})
            '';
            destination = "/${feature}.el";
          };
        in
          ''
            (add-to-list 'load-path "${def}")
          ''
      ) customPackages)}
    '';
}
