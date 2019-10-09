{ lib, ... }:
with lib;

rec {
  sedPlaceholderChar = "_";
  genIni = lib.generators.toINI {
    mkKeyValue = key: value:
      let
        mvalue = if builtins.isBool value then
          (if value then "true" else "false")
        else if (builtins.isString value && key != "include-file") then
          value
        else
          builtins.toString value;
      in "${key}=${mvalue}";
  };
  prettifyValue = value:
    if builtins.typeOf value == "int" then
      builtins.toString value
    else if builtins.typeOf value == "bool" then
      if value == true then "✓" else "✗"
    else
      builtins.toString value;
  indentedLines = lines: indentation:
    ''

      ${indentation}'' + builtins.concatStringsSep ''

        ${indentation}'' lines;
  setToBashKeyValue = set: keyname: valueSep: omitKey:
    let
      keyValue = set.${keyname};
      strippedSet = builtins.removeAttrs set [ keyname ];
    in ''["'' + keyValue + ''"]="'' + (builtins.concatStringsSep valueSep (lib.mapAttrsToList
      (key: value: if omitKey then "${prettifyValue value}" else "${key}:${sedPlaceholderChar}${prettifyValue value}")
      strippedSet)) + ''"'';
  unfoldListOfSetsByAttr = list: attr:
    let v = if builtins.length list == 0 then { ${attr} = [ ]; } else builtins.head list;
    in (map (elem: v // { ${attr} = elem; }) v.${attr})
    ++ (if builtins.length list == 0 then [ ] else (unfoldListOfSetsByAttr (builtins.tail list) attr));
  listOfSetsToShellHashtable = list: keyname: tablename: omitKey:
    "declare -A ${tablename}" + "\n" + "${tablename}=(" + "\n"
    + (builtins.concatStringsSep "\n" (map (attrs: setToBashKeyValue attrs keyname " " omitKey) list)) + "\n" + ")";
  addBuildInputs = pkg: inputs: pkg.overrideAttrs (attrs: { buildInputs = attrs.buildInputs ++ inputs; });
  withPatches = pkg: patches: lib.overrideDerivation pkg (attrs: { inherit patches; });
}
