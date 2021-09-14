{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

let
  keysymsXmonad = {
    "F1" = "<F1>";
    "F2" = "<F2>";
    "F3" = "<F3>";
    "F4" = "<F4>";
    "F5" = "<F5>";
    "F6" = "<F6>";
    "F7" = "<F7>";
    "F8" = "<F8>";
    "F9" = "<F9>";
    "F10" = "<F10>";
    "F11" = "<F11>";
    "F12" = "<F12>";
  };
in
rec {
  toHaskellBool = v: builtins.replaceStrings [ "true" "false" ] [ "True" "False" ] (lib.trivial.boolToString v);
  mkKeysymXmonad = key: if builtins.hasAttr key keysymsXmonad then builtins.getAttr key keysymsXmonad else key;
  mkKeysXmonadSpawn = keys: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList (key: meta:
        ''
          "${key}" ~> spawn "${meta.cmd}"${
            if lib.hasAttrByPath [ "desktop" ] meta then " >> showWSOnProperScreen \"${meta.desktop}\"" else ""
          }'') keys)
    }${mkIndent indent}";
  mkKeysXmonadRaw = keys: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList (key: cmd: ''"${key}" ~> ${cmd}'') keys)
    }${mkIndent indent}";
  mkWorkspacesXmonad = wss: indent: # FIXME: update logic (presumably broken)
    "${
      lib.concatStringsSep "${mkNewlineAndIndent indent}, " (lib.mapAttrsToList
        (ws: meta: ''("${ws}", Just "${convertKeyXmonad meta.key}", ${toHaskellBool meta.transient})'') wss)
    }${mkIndent indent}";
}
