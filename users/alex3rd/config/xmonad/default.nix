{ config, pkgs, lib, ... }:

with import <home-manager/modules/lib/dag.nix> { inherit lib; }; # TODO: make more declarative
with import ../../../../pkgs/util.nix { inherit lib config; };
{
  home-manager.users."${config.attributes.mainUser.name}" = {
    home.file = {
      ".xmonad/lib/XMonad/Util/Xkb.hs".source = ./lib/XMonad/Util/Xkb.hs;
      ".xmonad/lib/XMonad/Util/ExtraCombinators.hs".source = ./lib/XMonad/Util/ExtraCombinators.hs;
      ".xmonad/lib/XMonad/Util/WindowTypes.hs".source = ./lib/XMonad/Util/WindowTypes.hs;
      ".xmonad/lib/Controls.hs" = {
        text = builtins.readFile
          (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) // { src = ./Controls.hs; }));
        onChange = "xmonad --recompile";
      };
      ".xmonad/lib/Workspaces.hs" = {
        text = builtins.readFile
          (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) // { src = ./Workspaces.hs; }));
        onChange = "xmonad --recompile";
      };
      ".xmonad/xmonad.hs" = {
        text = builtins.readFile
          (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) // { src = ./xmonad.hs; }));
        onChange = "xmonad --recompile";
      };
    };
  };
}
