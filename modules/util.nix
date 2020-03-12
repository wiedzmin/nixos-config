{ lib, pkgs, ... }:

rec {
  addBuildInputs = pkg: inputs: pkg.overrideAttrs (attrs: { buildInputs = attrs.buildInputs ++ inputs; });
  withPatches = pkg: patches: lib.overrideDerivation pkg (_: { inherit patches; });
  writePythonScriptWithPythonPackages = pname: packages: text:
    pkgs.python3Packages.buildPythonPackage rec {
      inherit pname;
      version = "unstable";
      src = pkgs.writeTextFile {
        name = "${pname}.py";
        text = ''
          #!${pkgs.python3}/bin/python3
          ${text}'';
        executable = true;
      };
      format = "other";
      unpackPhase = "true";
      buildInputs = with pkgs; [ makeWrapper ];
      propagatedBuildInputs = with pkgs; packages;
      buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${pname}";
      installPhase = "true";
      postInstall = ''
        chmod a+x $out/bin/${pname}
      '';
    };
  renderTimer = desc: boot: active: cal: {
    description = "${desc}";
    wantedBy = [ "timers.target" ];
    timerConfig = lib.optionalAttrs (boot != "") {
      OnBootSec = "${boot}";
    } // lib.optionalAttrs (active != "") {
      OnUnitActiveSec = "${active}";
    } // lib.optionalAttrs (cal != "") {
      OnCalendar = "${cal}";
    };
  };
}
