{ config, lib, pkgs, ... }:

let configBasePath = "/etc/nixos";
in rec {
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
  writeShellScriptBinWithDeps = name: packages: contents:
    pkgs.stdenv.mkDerivation rec {
      inherit name;
      version = "unstable";
      src = pkgs.writeTextFile {
        name = "${name}.sh";
        text = ''
          #!${pkgs.stdenv.shell}
          ${contents}
        '';
        executable = true;
      };
      unpackPhase = "true";
      buildInputs = with pkgs; [ makeWrapper ];
      propagatedBuildInputs = with pkgs; packages;
      buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${name}";
      installPhase = ''
        mkdir -p $out/bin
        wrapProgram $out/bin/${name} \
          --prefix PATH : ${pkgs.lib.makeBinPath packages}
      '';
      postInstall = ''
        chmod a+x $out/bin/${name}
      '';
    };
  renderTimer = desc: boot: active: cal: {
    description = "${desc}";
    wantedBy = [ "timers.target" ];
    timerConfig = lib.optionalAttrs (boot != "") { OnBootSec = "${boot}"; }
      // lib.optionalAttrs (active != "") { OnUnitActiveSec = "${active}"; }
      // lib.optionalAttrs (cal != "") { OnCalendar = "${cal}"; };
  };
  renderHosts = metadata:
    builtins.concatStringsSep "\n"
    (lib.mapAttrsToList (ip: meta: ip + "    " + (builtins.concatStringsSep " " meta.hostnames)) metadata);
  mkProjectShellNix = args: ''
    let
      pkgs = import <nixpkgs> {};
    in
      pkgs.mkShell {
        ${
          lib.optionalString ((builtins.hasAttr "simple" args) && args.simple != [ ]) ''
            buildInputs = with pkgs; [
            ${builtins.concatStringsSep "\n" (builtins.map (elt: (mkIndent 6) + elt) args.simple)}
            ${mkIndent 4}];''
        }
        ${
          lib.optionalString ((builtins.hasAttr "native" args) && args.native != [ ]) ''
            nativeBuildInputs = with pkgs; [
            ${builtins.concatStringsSep "\n" (builtins.map (elt: (mkIndent 6) + elt) args.native)}
            ${mkIndent 4}];''
        }
        ${
          lib.optionalString ((builtins.hasAttr "prop" args) && args.prop != [ ]) ''
            propagatedBuildInputs = with pkgs; [
            ${builtins.concatStringsSep "\n" (builtins.map (elt: (mkIndent 6) + elt) args.native)}
            ${mkIndent 4}];''
        }
        ${
          lib.optionalString ((builtins.hasAttr "env" args) && args.env != { }) (builtins.concatStringsSep "\n"
            (lib.mapAttrsToList (key: value: key + " = " + (lib.strings.escapeNixString value) + ";") args.env))
        }
      }
  '';
  mkIndent = width: with lib; (concatStrings (genList (const " ") width));
  mapMimesToApp = mimes: app: lib.genAttrs mimes (_: [ app ]);
  homePrefix = suffix: "/home/${config.attributes.mainUser.name}/" + suffix;
  xdgConfig = suffix: (homePrefix ".config") + suffix;
  secretsPrefix = suffix: configBasePath + "/machines/" + config.attributes.machine.name + "/secrets/" + suffix;
  assetsPrefix = suffix: configBasePath + "/machines/" + config.attributes.machine.name + "/assets/" + suffix;
  fromYAML = yaml:
    builtins.fromJSON (builtins.readFile (pkgs.runCommand "from-yaml" {
      inherit yaml;
      allowSubstitutes = false;
      preferLocalBuild = true;
    } ''
      ${pkgs.remarshal}/bin/remarshal  \
        -if yaml \
        -i <(echo "$yaml") \
        -of json \
        -o $out
    ''));
}
