{ config, pkgs, lib, ... }:
with import ../../const.nix { inherit config pkgs; };
{
  home-manager.users."${userName}" = {
    xresources.properties = {
      "Xmessage*Buttons" = "Quit";
      "Xmessage*defaultButton" = "Quit";
      "Xmessage*faceName" = "${fontMainName}";
      "Xmessage*faceSize" = "${fontSizeXmessage}";
      "Xmessage*faceWeight" = "${fontMainWeight}";
      "Xmessage*international" = true;

      "dzen2.font" =
        "${fontMainName}:${fontMainWeightKeyword}=${fontMainWeight}:${fontMainSizeKeyword}=${fontSizeDzen}";

      "Emacs*XlwMenu.font" = "${fontCodeName}:weight=${fontCodeWeight}:size=${fontSizeEmacs}";
      "Emacs.Font" = "${fontCodeName}:weight=${fontCodeWeight}:size=${fontSizeEmacs}";
      "Emacs.FontBackend" = "xft,x";
      "Emacs.dialog*.font" = "${fontCodeName}:weight=${fontCodeWeight}:size=${fontSizeEmacs}";
      "Emacs.menuBar" = "0";
      "Emacs.toolBar" = "0";
      "Emacs.verticalScrollBars" = false;

      "urgentOnBell" = true;
      "visualBell" = true;

      "Xft.antialias" = true;
      "Xft.autohint" = false;
      "Xft.dpi" = "120.0";
      "Xft.hinting" = true;
      "Xft.hintstyle" = "hintslight";
      "Xft.lcdfilter" = "lcddefault";
      "Xft.rgba" = "none";
    };
    xsession.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    };
  };
}
