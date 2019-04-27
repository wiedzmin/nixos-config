{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
{
    home-manager.users.alex3rd = {
        programs.vscode = {
            enable = true;
            userSettings = {
                "update.channel" = "none";
                "[nix]"."editor.tabSize" = 4;
            };
            extensions = [
                pkgs.vscode-extensions.bbenoist.Nix
            ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                {
                    name = "python";
                    publisher = "ms-python";
                    version = "2019.4.11987";
                    sha256 = "0iq4pbz4sy7730ap9kbv8b5ncb3jbxpf0ibsyjf3rs6j6jwvdv1v";
                }
            ];
        };
    };
}
