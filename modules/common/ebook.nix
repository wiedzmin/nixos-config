{ config, lib, pkgs, ... }:
with import ../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.tools.ebooks;
  bookshelf = writePythonScriptWithPythonPackages "bookshelf" [
    pkgs.python3Packages.dmenu-python
  ] ''
    import os
    import re
    import subprocess

    import dmenu

    books = []

    books_task = subprocess.Popen("${pkgs.fd}/bin/fd --full-path /home/${config.attributes.mainUser.name}/bookshelf -e pdf -e djvu -e epub",
                                  shell=True, stdout=subprocess.PIPE)
    books.extend([book for book in books_task.stdout.read().decode().split("\n")])
    assert books_task.wait() == 0

    result = dmenu.show(books, prompt='book', lines=30)
    if result:
        os.system("${config.attributes.defaultCommands.ebookReader} {0}".format(re.escape(result)))
  '';
in {
  options = {
    tools.ebooks = {
      readers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebook readers for various formats.";
      };
      processors.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebooks processors (mostly pdf-centric).";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.readers.enable {
      fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
        device = "/home/${config.attributes.mainUser.name}/bookshelf";
        options = [ "bind" ];
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          calibre
          djview
          djvulibre
        ] ++ lib.optionals config.attributes.staging.enable [
          epr
        ];
        programs.zathura = {
          enable = true;
          options = {
            pages-per-row = 1;
          };
        };
      };
    })
    (mkIf cfg.processors.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          enca
          img2pdf
          pandoc
          pdfcpu
          pdftk
        ] ++ lib.optionals (config.attributes.staging.enable) [
          pdfarranger
          wpWorking.python3Packages.weasyprint
        ];
      };
    })
    (mkIf (cfg.xmonad.enable && cfg.readers.enable) {
      wm.xmonad.keybindings = {
        "M-S-b" = ''spawn "${bookshelf}/bin/bookshelf"'';
      };
    })
  ];
}
