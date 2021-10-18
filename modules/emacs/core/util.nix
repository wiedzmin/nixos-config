{ config, inputs, lib, pkgs, ... }:

rec {
  mkCustomPackage = feature: impl:
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
    '';
  genCustomPackages = customPackages:
    ''
    ${builtins.concatStringsSep "\n"
      (lib.mapAttrsToList (key: value: mkCustomPackage key value) customPackages)}
    '';
}
