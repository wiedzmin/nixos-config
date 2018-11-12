{config, pkgs, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            org-protocol.desktop = pkgs.makeDesktopItem {
                name = "org-protocol";
                exec = "${pkgs.emacs}/bin/emacsclient %U";
                comment = "";
                desktopName = "Custom org-protocol handler";
                categories = "System";
                mimeType = pkgs.stdenv.lib.concatStringsSep ";" [
                    "x-scheme-handler/org-protocol"
                ];
            };
        };
    };
}
