{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.ebooks;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    content.ebooks = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebooks infrastructure.";
      };
      extensions.primary = mkOption {
        type = types.listOf types.str;
        default = [ "pdf" "djvu" "epub" ];
        description = "Main ebook file extensions to consider.";
      };
      extensions.secondary = mkOption {
        type = types.listOf types.str;
        default = [ "mobi" "fb2" ];
        description = "Auxiliary ebook file extensions, mostly for timetracking at the moment.";
      };
      emacs.pdf-tools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable `pdf-tools` emacs-native reader";
      };
      emacs.org-noter.searchPath = mkOption {
        type = types.str;
        default = "${config.pim.orgmode.rootDir}/annot";
        description = "Annotation data search path for `org-noter`";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fileSystems."${config.services.syncthing.dataDir}/bookshelf" = {
        device = homePrefix user "bookshelf";
        options = [ "bind" ];
      };
      systemd.user.services = builtins.listToAttrs (forEach (localPathsByType "ebooks" config.navigation.bookmarks.entries) (root:
        let
          token = concatStringsSep "-" (takeLast 2 (splitString "/" root));
        in
        {
          name = "update-ebooks-${token}";
          value = {
            description = "Update ${token} contents";
            after = [ "graphical-session-pre.target" ];
            partOf = [ "graphical-session.target" ];
            wantedBy = [ "graphical-session.target" ];
            path = [ pkgs.bash ];
            serviceConfig = {
              Type = "simple";
              WorkingDirectory = root;
              ExecStart = "${pkgs.watchexec}/bin/watchexec -r --exts ${
                concatStringsSep "," cfg.extensions.primary
              } -- ${nurpkgs.toolbox}/bin/collect --root ${root} --exts ${
                concatStringsSep "," cfg.extensions.primary
              } --key content/${token}/ebooks";
              StandardOutput = "journal";
              StandardError = "journal";
            };
          };
        }));
      pim.timetracking.rules = mkArbttTitleRule [ ".*papers/.*" ] "ebooks:papers" + "\n"
        + mkArbttPrefixedTitlesRule (with cfg.extensions; primary ++ secondary) "read:";
      home-manager.users."${user}" = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.ebook "org.pwmt.zathura.desktop";
        home.packages = with pkgs; [ djview djvulibre ];
        programs.zathura = {
          enable = true;
          options = {
            pages-per-row = 1;
            selection-clipboard = "clipboard";
          };
        };
      };
      attributes.ebookreader.default.cmd = "${pkgs.zathura}/bin/zathura";
      attributes.ebookreader.default.windowClass = [ "Zathura" ];
      shell.core.variables = [{ TB_EBOOKS_READER_COMMAND = config.attributes.ebookreader.default.cmd; global = true; }];
    })
    (mkIf (cfg.enable && cfg.emacs.pdf-tools.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.pdf-tools
        epkgs.nov
        epkgs.org-noter
        epkgs.org-pdftools
        epkgs.org-noter-pdftools
      ];
      ide.emacs.core.config = (readSubstituted config inputs pkgs [ ./subst/ebooks.nix ] [ ./elisp/ebooks.el ]);
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keybindings.entries = [{
        key = [ "b" ];
        cmd = "${nurpkgs.toolbox}/bin/insight ebooks";
        mode = "select";
        leaveFullscreen = true;
      }];
    })
  ];
}
