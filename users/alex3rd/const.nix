{config, ...}:

rec {
    userName = "alex3rd";
    userNamePrevious = "octocat";
    defaultBrowserCmd = "firefox --new-window";
    wallpaperBaseDir = "/home/${userName}/blobs/wallpaper";
    wallpaperCurrent = builtins.elemAt wallpaperInventory 4;
    wallpaperInventory = [
        "mountain.jpg"
        "westernmongolia.jpg"
        "mongolia_2.jpg"
        "mongolia_desert.jpg"
        "512445_orkhon-falls_mongolia_reki_1920x1200_www.Gde-Fon.com.jpg"
    ];
    fontMainName = builtins.elemAt fontInventory 7;
    fontMainWeight = "Bold";
    fontMainWeightKeyword = "weight";
    fontMainUseWeight = true;
    fontMainSizeKeyword = "size";
    fontMainAntialias = false;
    fontMainUseAntialias = false;
    fontCodeName = builtins.elemAt fontInventory 7;
    fontCodeWeight = "Bold";
    fontCodeWeightKeyword = "weight";
    fontCodeUseWeight = true;
    fontCodeSizeKeyword = "size";
    fontCodeAntialias = false;
    fontCodeUseAntialias = false;
    fontTermName = builtins.elemAt fontInventory 7;
    fontTermWeight = "Bold";
    fontTermWeightKeyword = "weight";
    fontTermUseWeight = true;
    fontTermSizeKeyword = "size";
    fontTermAntialias = false;
    fontTermUseAntialias = false;
    fontInventory = [
        "AnonymousPro"
        "DejavuSansMono"
        "FantasqueMono"
        "FiraCode"
        "FiraMono"
        "Hack"
        "Inconsolata"
        "Iosevka"
        "Mononoki"
        "Profont"
        "RobotoMono"
        "SourceCodePro"
        "Unifont"
    ];
    fontSizeXmessage = "16";
    fontSizeDzen = "16";
    fontSizeEmacs = "14";
    fontSizeURxvt = "12";
    fontSizeAlacritty = 11.0;
    fontSizeDunst = "10";
    defaultShellClass = "Alacritty"; # TODO: generalize in some way
    defaultUpstreamRemoteName = "upstream";
}
