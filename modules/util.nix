{ config, lib, pkgs, ... }:

let configBasePath = "/etc/nixos";
in rec {
  addBuildInputs = pkg: inputs: pkg.overrideAttrs (attrs: { buildInputs = attrs.buildInputs ++ inputs; });
  withPatches = pkg: patches: lib.overrideDerivation pkg (_: { inherit patches; });
  mkPythonScriptWithDeps = pname: packages: text: buildPythonScriptWithDeps pname false packages text;
  mkPythonScriptWithDepsAndConflicts = pname: packages: text: buildPythonScriptWithDeps pname true packages text;
  buildPythonScriptWithDeps = pname: allowConflicts: packages: text:
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
      dontUsePythonCatchConflicts = allowConflicts;
      buildPhase = "mkdir -p $out/bin && cp -r $src $out/bin/${pname}";
      installPhase = "true";
      postInstall = ''
        chmod a+x $out/bin/${pname}
      '';
    };
  mkShellScriptWithDeps = name: packages: contents:
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
    (lib.mapAttrsToList (ip: meta: ip + "   " + (builtins.concatStringsSep " " (lib.forEach meta (x: x.host))))
      (lib.groupBy (x: x.ip) (lib.flatten (lib.mapAttrsToList (host: meta:
        (lib.forEach meta.ips (ip: {
          "ip" = ip;
          "host" = host;
        }))) metadata))));
  mkProjectShellNix = args: ''
    let
      deps = import /etc/nixos/nix/sources.nix;
      pkgs = import deps.${
        if builtins.hasAttr "nixpkgs" args then args.nixpkgs else "nixpkgs"
      } { config.allowUnfree = true; };
    in
      pkgs.${
        lib.optionalString ((builtins.hasAttr "useShell" args && args.useShell) || !builtins.hasAttr "useShell" args)
        "mkShell"
      }${lib.optionalString ((builtins.hasAttr "useShell" args) && !args.useShell) "stdenv.mkDerivation"} {
        ${lib.optionalString ((builtins.hasAttr "useShell" args) && !args.useShell) ''name = "env";''}
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
            ${builtins.concatStringsSep "\n" (builtins.map (elt: (mkIndent 6) + elt) args.prop)}
            ${mkIndent 4}];''
        }
        ${
          lib.optionalString ((builtins.hasAttr "env" args) && args.env != { }) (builtins.concatStringsSep "\n"
            (lib.mapAttrsToList (key: value: key + " = " + (lib.strings.escapeNixString value) + ";") args.env))
        }
        ${
          lib.optionalString ((builtins.hasAttr "shell" args) && args.shell != "") ''
            shellHook = '''
            ${builtins.concatStringsSep "\n" (builtins.map (elt: (mkIndent 6) + elt) (lib.splitString "\n" args.shell))}
            ${mkIndent 4}''';
          ''
        }
      }
  '';
  mkIndent = width: with lib; (concatStrings (genList (const " ") width));
  mkNewlineAndIndent = width: with lib; "\n" + (concatStrings (genList (const " ") width));
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
  toToml = attrs:
    builtins.readFile (pkgs.runCommand "to-toml" {
      # inherit attrs;
      allowSubstitutes = false;
      preferLocalBuild = true;
    } ''
      ${pkgs.remarshal}/bin/remarshal  \
        -if json \
        -of toml \
        < ${pkgs.writeText "attrs.json" (builtins.toJSON attrs)} \
        > $out
    '');
  maybeAttrIsBool = name: set: (builtins.hasAttr name set) && (set."${name}" == true);
  maybeAttrString = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else ph;
  maybeAttrList = name: set: ph: if (builtins.hasAttr name set) then set."${name}" else [ ph ];
  emacsBoolToString = v: if v == true then "t" else "nil";
  wsRoot = key:
    let roots = config.custom.dev.workspaceRoots;
    in if builtins.hasAttr key roots then
      lib.getAttrFromPath [ key ] roots
    else
      throw "no '${key}' workspace root found";
  wsRootAbs = key:
    let roots = config.custom.dev.workspaceRoots;
    in if builtins.hasAttr key roots then
      homePrefix (lib.getAttrFromPath [ key ] roots)
    else
      builtins.trace "no '${key}' workspace root found";
  selectorFunction = lib.mkOptionType {
    name = "selectorFunction";
    description = "Function that takes an attribute set and returns a list"
      + " containing a selection of the values of the input set";
    check = lib.isFunction;
    merge = _loc: defs: as: lib.concatMap (select: select as) (lib.getValues defs);
  };
}
