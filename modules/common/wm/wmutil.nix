{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };

let
  keysymsI3 = {
    "`" = "grave";
    "Esc" = "Escape";
  };
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
in rec {
  dmenu_runapps = writeShellScriptBinWithDeps "dmenu_runapps"
    (with pkgs; [ coreutils dmenu haskellPackages.yeganesh j4-dmenu-desktop ]) ''
      j4-dmenu-desktop --display-binary --dmenu="(cat ; (stest -flx $(echo $PATH | tr : ' ') | sort -u)) | \
        yeganesh -- -i -l 15 -fn '${config.wmCommon.fonts.dmenu}'"
    '';
  dmenu_select_windows = writeShellScriptBinWithDeps "dmenu_select_windows" (with pkgs; [ coreutils dmenu wmctrl ]) ''
    wmctrl -a $(wmctrl -l | cut -d" " -f5- | dmenu -i -l 15 -fn '${config.wmCommon.fonts.dmenu}')
  '';
  getWorkspacesByType = wsdata: type: (lib.groupBy (x: x.type) wsdata)."${type}";
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
  mkKeyI3 = keys: sep:
    lib.concatStringsSep sep
    (lib.forEach keys (k: if builtins.hasAttr k keysymsI3 then builtins.getAttr k keysymsI3 else k));
  getWorkspaceByNameI3 = wsdata: name:
    let ws = builtins.head (builtins.filter (ws: ws.name == name) wsdata);
    in "${builtins.toString ws.index}: ${ws.name}";
  mkWorkspacesI3 = wsdata: mod: sep:
    lib.concatStringsSep "\n" (lib.forEach wsdata (ws: ''
      set $ws_${ws.name} ${builtins.toString ws.index}: ${ws.name}
      bindsym ${mod}+${mkKeyI3 ws.key sep} workspace $ws_${ws.name}
      bindsym ${mod}+Shift+${mkKeyI3 ws.key sep} move container to workspace $ws_${ws.name}
    ''));
  mvWorkspacesI3Cmd = wsdata: type: head:
    "${lib.concatStringsSep " " (lib.forEach (getWorkspacesByType wsdata type) (ws:
      "workspace --no-auto-back-and-forth ${
        builtins.toString ws.index
      }: ${ws.name}; move workspace to output ${head}; "))}";
  # TODO: implement `transient` flag (whether to issue --no-startup-id)
  mkKeybindingI3 = meta: sep:
    let wss = config.wmCommon.workspaces;
    in "bindsym ${mkKeyI3 meta.key sep}${
      if builtins.hasAttr "raw" meta then " " else " exec --no-startup-id "
    }${meta.cmd}${
      if builtins.hasAttr "desktop" meta then "; workspace ${getWorkspaceByNameI3 wss meta.desktop}" else ""
    }${
      if ((!builtins.hasAttr "sticky" meta) || ((builtins.hasAttr "sticky" meta) && meta.sticky == false)) && meta.mode
      != "root" then
        ''; mode "default"''
      else
        ""
    }";
  mkKeybindingsI3 = keys: modeBindings: sep:
    let
      prefixedModesMeta = lib.filterAttrs (k: _: k != "root") (lib.groupBy (x: x.mode) keys);
      rootModeBindings = (lib.filterAttrs (k: _: k == "root") (lib.groupBy (x: x.mode) keys)).root;
    in ''
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (mode: bindings: ''
        mode "${mode}" {
          ${lib.concatStringsSep (mkNewlineAndIndent 2) (lib.forEach bindings (x: mkKeybindingI3 x sep))}
        }
      '') prefixedModesMeta)}
      ${lib.concatStringsSep "\n" (lib.forEach rootModeBindings (x: mkKeybindingI3 x sep))}

      ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (mode: key: ''bindsym ${mkKeyI3 key sep} mode "${mode}"'') modeBindings)}
    '';
  mkKeybindingsFocusI3 = rules: sep:
    lib.concatStringsSep "\n" (lib.forEach (builtins.filter (r: builtins.hasAttr "focus" r) rules) (r:
      "bindsym ${mkKeyI3 r.focus sep} [ ${if builtins.hasAttr "class" r then ''class="^${r.class}$" '' else ""}${
        if builtins.hasAttr "title" r then ''title=".*${builtins.replaceStrings [ " " ] [ ".*" ] r.title}.*"'' else ""
      } ] focus"));
  # FIXME: remove duplicaion ^V
  mkPlacementRulesI3 = wsdata: rules:
    lib.concatStringsSep "\n" (lib.forEach rules (rule:
      "for_window [ ${if builtins.hasAttr "class" rule then ''class="^${rule.class}$" '' else ""}${
        if builtins.hasAttr "title" rule then
          ''title=".*${builtins.replaceStrings [ " " ] [ ".*" ] rule.title}.*"''
        else
          ""
      } ] move to workspace ${getWorkspaceByNameI3 wsdata rule.desktop}"));
}
