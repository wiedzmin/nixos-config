{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.paperworks;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  nixpkgs-libreoffice = import inputs.nixpkgs-libreoffice {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };

  paperlessDefaultUser = "paperless";

  paperlessManageScript = cfg.scanning.paperless.package.withConfig {
    config = {
      PAPERLESS_CONSUMPTION_DIR = cfg.scanning.paperless.consumptionDir;
      PAPERLESS_INLINE_DOC = "true";
      PAPERLESS_DISABLE_LOGIN = "true";
    } // cfg.scanning.paperless.extraConfig;
    inherit (cfg.scanning.paperless) dataDir ocrLanguages;
    paperlessPkg = cfg.scanning.paperless.package;
  };
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    paperworks = {
      printing.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable printing infrastructure.";
      };
      printing.drivers = mkOption {
        type = types.listOf types.path;
        default = [ ];
        example = [ pkgs.hplipWithPlugin ];
        description = "Printer drivers list.";
      };
      scanning.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable scanning infrastructure.";
      };
      scanning.extraBackends = mkOption {
        type = types.listOf types.path;
        default = [ ];
        example = [ pkgs.epkowa ];
        description = "Extra SANE backends list.";
      };
      scanning.snapscan.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether we are using Epson Snapscan series scanner which is
          currently true for this configuration.
        '';
      };
      scanning.snapscan.firmware = mkOption {
        type = types.path;
        default = /homeless-shelter;
        description = "Path to snapscan firmware file.";
      };
      scanning.frontend = mkOption {
        type = types.enum [ "skanlite" "gscan2pdf" "simple-scan" "xsane" ];
        default = "skanlite";
        description = "Scanner frontend tool";
      };
      scanning.paperless.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable scanned images management.";
      };
      scanning.paperless.consumptionDir = mkOption {
        type = types.str;
        default = "";
        description = "Directory to consume images from.";
      };
      scanning.paperless.dataDir = mkOption {
        type = types.str;
        default = "";
        description = "Paperless service data directory.";
      };
      scanning.paperless.consumptionDirIsPublic = mkOption {
        type = types.bool;
        default = false;
        description = "Whether all users can write to the consumption dir.";
      };
      scanning.paperless.ocrLanguages = mkOption {
        type = with types; nullOr (listOf str);
        default = null;
        description = ''
          Languages available for OCR via Tesseract, specified as
          <literal>ISO 639-2/T</literal> language codes.
          If unset, defaults to all available languages.
        '';
        example = [ "eng" "spa" "jpn" ];
      };
      scanning.paperless.address = mkOption {
        type = types.str;
        default = "localhost";
        description = "Server listening address.";
      };
      scanning.paperless.port = mkOption {
        type = types.int;
        default = 28981;
        description = "Server port to listen on.";
      };
      scanning.paperless.extraConfig = mkOption {
        type = types.attrs;
        default = { };
        description = ''
          Extra paperless config options.

          The config values are evaluated as double-quoted Bash string literals.

          See <literal>paperless-src/paperless.conf.example</literal> for available options.

          To enable user authentication, set <literal>PAPERLESS_DISABLE_LOGIN = "false"</literal>
          and run the shell command <literal>$dataDir/paperless-manage createsuperuser</literal>.

          To define secret options without storing them in /nix/store, use the following pattern:
          <literal>PAPERLESS_PASSPHRASE = "$(&lt; /etc/my_passphrase_file)"</literal>
        '';
        example = literalExpression ''
          {
            PAPERLESS_OCR_LANGUAGE = "deu";
          }
        '';
      };
      scanning.paperless.user = mkOption {
        type = types.str;
        default = paperlessDefaultUser;
        description = "User under which Paperless runs.";
      };
      scanning.paperless.group = mkOption {
        type = types.str;
        default = config.attributes.localGroup;
        description = "Group under which Paperless runs.";
      };
      scanning.paperless.package = mkPackageOption pkgs "paperless" { extraDescription = "The Paperless package to use"; };
      scanning.paperless.manage = mkOption {
        type = types.package;
        readOnly = true;
        default = paperlessManageScript;
        description = ''
          A script to manage the Paperless instance.
          It wraps Django's manage.py and is also available at
          <literal>$dataDir/manage-paperless</literal>
        '';
      };
      docflow.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable docflow tooling";
      };
      docflow.libreoffice.package = mkPackageOption nixpkgs-libreoffice "libreoffice-still" { extraDescription = "The `libreoffice` package to install"; };
      docflow.extensions = mkOption {
        type = types.listOf types.str;
        default = [ "doc" "docx" "xls" "xlsx" "odt" ];
        description = "Documents file extensions to consider";
      };
      processors.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable ebooks processors (mostly pdf-centric).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.printing.enable {
      assertions = [{
        assertion = cfg.printing.enable && cfg.printing.drivers != [ ];
        message = "paperwork: must provide at least one printer driver package.";
      }];
      services.printing = {
        enable = true;
        browsing = true;
        defaultShared = true;
        webInterface = true;
        inherit (cfg.printing) drivers;
        cups-pdf.enable = true;
      };
      environment.systemPackages = with pkgs; [ system-config-printer gtklp ];
      users.users."${user}".extraGroups = [ "lp" ];
    })
    (mkIf cfg.scanning.enable {
      hardware.sane = {
        enable = true;
        inherit (cfg.scanning) extraBackends;
      };

      environment.systemPackages = with pkgs;
        [ deskew scantailor-advanced naps2 ] ++ lib.optionals (cfg.scanning.frontend == "skanlite") [ skanlite ]
        ++ lib.optionals (cfg.scanning.frontend == "gscan2pdf") [ gscan2pdf ]
        ++ lib.optionals (cfg.scanning.frontend == "simple-scan") [ simple-scan ]
        ++ lib.optionals (cfg.scanning.frontend == "xsane") [ xsane ];
      users.users."${user}".extraGroups = [ "scanner" ];

      wmCommon.wsMapping.rules = lib.optionals (cfg.scanning.frontend == "skanlite") [{
        class = "skanlite";
        desktop = "scan"; # [ref:desktop_scan]
      }] ++ lib.optionals (cfg.scanning.frontend == "gscan2pdf") [{
        class = "gscan2pdf-wrapped-wrapped";
        desktop = "scan"; # [ref:desktop_scan]
      }] ++ lib.optionals (cfg.scanning.frontend == "simple-scan") [{
        class = "Simple-scan";
        desktop = "scan"; # [ref:desktop_scan]
      }] ++ lib.optionals (cfg.scanning.frontend == "xsane") [{
        class = "Xsane";
        desktop = "scan"; # [ref:desktop_scan]
        float = false;
      }];
    })
    (mkIf (cfg.scanning.enable && cfg.scanning.snapscan.enable) {
      assertions = [
        {
          assertion = cfg.scanning.snapscan.enable && cfg.scanning.snapscan.firmware != "";
          message = "paperwork: must provide firmware file if snapscan is enabled.";
        }
        {
          assertion = cfg.scanning.snapscan.enable && builtins.pathExists cfg.scanning.snapscan.firmware;
          message = "paperwork: no firmware file found at ${cfg.scanning.snapscan.firmware}";
        }
      ];
      nixpkgs.config = { sane.snapscanFirmware = cfg.scanning.snapscan.firmware; };
    })
    (mkIf (cfg.scanning.enable && cfg.scanning.paperless.enable) {
      assertions = [{
        assertion = cfg.scanning.paperless.enable && cfg.scanning.paperless.consumptionDir != ""
          && cfg.scanning.paperless.dataDir != "";
        message = "paperless: must provide paths for consume and data directories.";
      }];

      systemd.tmpfiles.rules =
        [ "d '${cfg.scanning.paperless.dataDir}' - ${cfg.scanning.paperless.user} ${cfg.scanning.paperless.group} - -" ]
        ++ (optional cfg.scanning.paperless.consumptionDirIsPublic
          "d '${cfg.scanning.paperless.consumptionDir}' 777 ${cfg.scanning.paperless.user} ${cfg.scanning.paperless.group} - -"
          # If the consumption dir is not created here, it's automatically created by
          # 'manage' with the default permissions.
        );

      systemd.services.paperless-consumer = {
        description = "Paperless document consumer";
        serviceConfig = {
          User = cfg.scanning.paperless.user;
          ExecStart = "${paperlessManageScript} document_consumer";
          Restart = "always";
        };
        after = [ "systemd-tmpfiles-setup.service" ];
        wantedBy = [ "multi-user.target" ];
        preStart = ''
          if [[ $(readlink ${cfg.scanning.paperless.dataDir}/paperless-manage) != ${paperlessManageScript} ]]; then
            ln -sf ${paperlessManageScript} ${cfg.scanning.paperless.dataDir}/paperless-manage
          fi

          ${paperlessManageScript.setupEnv}
          # Auto-migrate on first run or if the package has changed
          versionFile="$PAPERLESS_DBDIR/src-version"
          if [[ $(cat "$versionFile" 2>/dev/null) != ${cfg.scanning.paperless.package} ]]; then
            python $paperlessSrc/manage.py migrate
            echo ${cfg.scanning.paperless.package} > "$versionFile"
          fi
        '';
      };
      systemd.services.paperless-server = {
        description = "Paperless document server";
        serviceConfig = {
          User = cfg.scanning.paperless.user;
          ExecStart = "${paperlessManageScript} runserver --noreload ${cfg.scanning.paperless.address}:${
              toString cfg.scanning.paperless.port
            }";
          Restart = "always";
        };
        # Bind to `paperless-consumer` so that the server never runs
        # during migrations
        bindsTo = [ "paperless-consumer.service" ];
        after = [ "paperless-consumer.service" ];
        wantedBy = [ "multi-user.target" ];
      };

      users = optionalAttrs (cfg.scanning.paperless.user == paperlessDefaultUser) {
        users = [{
          name = paperlessDefaultUser;
          group = paperlessDefaultUser;
          uid = config.ids.uids.paperless;
          home = cfg.scanning.paperless.dataDir;
        }];

        groups = [{
          name = paperlessDefaultUser;
          gid = config.ids.gids.paperless;
        }];
      };
    })
    (mkIf cfg.docflow.enable {
      systemd.user.services = builtins.listToAttrs (forEach (localPathsByType "docs" config.navigation.bookmarks.entries) (root:
        let
          token = concatStringsSep "-" (takeLast 2 (splitString "/" root));
        in
        {
          name = "update-docs-${token}";
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
                concatStringsSep "," cfg.docflow.extensions
              } -- ${nurpkgs.toolbox}/bin/collect --root ${root} --exts ${
                concatStringsSep "," cfg.docflow.extensions
              } --key paperworks/${token}/docs";
              StandardOutput = "journal";
              StandardError = "journal";
            };
          };
        }));

      wmCommon.keybindings.entries = [{
        key = [ "d" ];
        cmd = "${nurpkgs.toolbox}/bin/insight docs";
        mode = "select";
      }];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          cfg.docflow.libreoffice.package
          unipicker
        ];
        xdg.mimeApps.defaultApplications =
          (mapMimesToApp config.attributes.mimetypes.office.docs "writer.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.spreadsheets "calc.desktop");
        xdg.configFile = optionalAttrs (config.completion.expansions.enable) {
          "espanso/match/paperworks_docflow.yml".source = yaml.generate "espanso-paperworks_docflow.yml" {
            matches = [
              {
                trigger = ":docpdf";
                replace = "libreoffice --headless --convert-to pdf {{inputfile.value}}";
                vars = [
                  {
                    name = "inputfile";
                    type = "form";
                    params = { layout = "Inputfile [[value]]"; };
                  }
                ];
              }
            ];
          };
        };
      };

      pim.timetracking.rules =
        mkArbttProgramMultipleTagsRule "libreoffice" [ "activity:docflow" "program:libreoffice" ];
      # TODO: consider using metadata similar to `config.attributes.ebookreader.default`
      shell.core.variables = [{ TB_DOCS_VIEWER_COMMAND = "${cfg.docflow.libreoffice.package}/bin/soffice"; global = true; }];
    })
    (mkIf cfg.processors.enable {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [ enca imagemagick img2pdf ocamlPackages.cpdf pandoc pdfchain pdfcpu pdfslicer pdftk ];
        xdg.configFile = optionalAttrs (config.completion.expansions.enable) {
          "espanso/match/paperworks_processors.yml".source = yaml.generate "espanso-paperworks_processors.yml" {
            matches = [
              {
                trigger = ":pmdpdf";
                replace = "pandoc --self-contained -o {{namesansext}}.pdf {{inputfile.value}}.md";
                vars = [
                  {
                    name = "inputfile";
                    type = "form";
                    params = { layout = "Inputfile [[value]]"; };
                  }
                  {
                    name = "namesansext";
                    type = "shell";
                    params = { cmd = "echo \"toolbox-migration.org\" | cut -d'.' -f1"; };
                  }
                ];
              }
              {
                trigger = ":imp";
                replace = "convert *{{wildcard.value}}* -auto-orient result.pdf";
                vars = [
                  {
                    name = "wildcard";
                    type = "form";
                    params = { layout = "Source name wildcard [[value]]"; };
                  }
                ];
              }
              {
                trigger = ":im2d";
                replace = "img2pdf *{{wildcard.value}}* --output result.pdf";
                vars = [
                  {
                    name = "wildcard";
                    type = "form";
                    params = { layout = "Source name wildcard [[value]]"; };
                  }
                ];
              }
            ];
          };
        };
      };
    })
    (mkIf (cfg.scanning.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "scanning-work-directory" = {
          tags = [ "scanning" "work" ];
          local.path = homePrefix user "docs/paperless/raw";
        };
      };
    })
  ];
}
