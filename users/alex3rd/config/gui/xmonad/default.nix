{config, pkgs, lib, ...}:

with import <home-manager/modules/lib/dag.nix> { inherit lib; }; # TODO: make more declarative
with import ../../../../../pkgs/util.nix {inherit lib config pkgs;};
with import ../../../const.nix {inherit config pkgs;};
{
    home-manager.users."${userName}" = {
        home.file = {
            ".xmonad/lib/Extensions/Xkb.hs".source = ./lib/Xkb.hs;
            ".xmonad/lib/Extensions/ExtraCombinators.hs".source = ./lib/ExtraCombinators.hs;
            ".xmonad/lib/Controls.hs" = {
                text = builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) //
                    { src = ./Controls.hs; }));
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/Layouts.hs" = {
                text = builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) //
                    { src = ./Layouts.hs; }));
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/StatusBar.hs" = {
                text = builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) //
                    { src = ./StatusBar.hs; }));
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/Themes.hs" = {
                text = builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) //
                    { src = ./Themes.hs; }));
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/Workspaces.hs" = {
                text = builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) //
                    { src = ./Workspaces.hs; }));
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/xmonad.hs" = {
                text = builtins.readFile (pkgs.substituteAll ((import ./subst.nix { inherit config pkgs lib; }) //
                    { src = ./xmonad.hs; }));
                onChange = ''xmonad --recompile'';
            };
        };
    };
}
