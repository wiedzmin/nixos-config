{config, pkgs, lib, ...}:
with lib;
{
  home-manager.users."${config.attributes.mainUser.name}" = {
    home.file = {
      "tmuxp/main.yml".text = ''
        session_name: main
        windows:
          - window_name: repl
            panes:
              - nix repl '<nixpkgs/nixos>'
          - window_name: files
            panes:
              - mc
      '';
      "tmuxp/dev.yml".text = ''
        session_name: dev
        windows:
          - window_name: mc
            start_directory: ${config.secrets.dev.workspaceRoot}/github.com/wiedzmin
            panes:
              - mc
      '';
      ".ssh/id_rsa.pub".text = config.secrets.network.ssh.publicKey; # kept secrets, it tends to diverge otherwise
      "tmuxp/media.yml".text = ''
        session_name: media
        windows:
          - window_name: mps-youtube
            panes:
              - ${pkgs.python3Packages.mps-youtube}/bin/mpsyt
      '';
      ".local/share/applications/defaults.list" = {
        text = ''
          [Default Applications]
          application/pdf=zathura.desktop
        '';
      };
      "workspace/.editorconfig".text = ''
        # top-most EditorConfig file
        root = true

        # Unix-style newlines with a newline ending every file
        [*]
        end_of_line = lf
        insert_final_newline = true
        indent_style = space
        charset = utf-8
        trim_trailing_whitespace = true

        # Matches multiple files with brace expansion notation
        # Set default charset
        [*.{js,py,go}]
        charset = utf-8

        # 4 space indentation
        [*.py]
        indent_style = space
        indent_size = 4

        # Tab indentation (no size specified)
        [Makefile]
        indent_style = tab

        # Indentation override for all JS under lib directory
        [lib/**.js]
        indent_style = space
        indent_size = 2

        # Matches the exact files either package.json or .travis.yml
        [{package.json,.travis.yml}]
        indent_style = space
        indent_size = 2

        [*.{json,yml}]
        indent_style = space
        indent_size = 2

        [*.md]
        trim_trailing_whitespace = false
      '';
      ".mpv/config".text = ''
        hwdec=vdpau
        hwdec-codecs=all

        vo=gpu,xv
        ao=pulse

        af=scaletempo
        audio-samplerate=48000

        slang = en
        alang = en,eng,us

        volume-max=200
        cache-default = 250000 # Use 250MB cache for network streams

        # Always use 1080p+ or 60 fps where available. Prefer VP9
        # over AVC and VP8 for high-resolution streams.
        ytdl=yes
        ytdl-format=(bestvideo[ext=webm]/bestvideo[height>720]/bestvideo[fps=60])[tbr<13000]+(bestaudio[acodec=opus]/bestaudio[ext=webm]/bestaudio)/best
      '';
      ".arbtt/categorize.cfg".text = ''
        aliases (
                "Navigator" -> "Firefox",
                "Zathura" -> "PDF reader",
                "telegram-desktop" -> "Telegram",
                "Alacritty" -> "Shell",
        )

        $idle > 60 ==> tag inactive,
        current window $program == ["Navigator", "Google-chrome", "Google-chrome-stable"] ==> tag activity:web,
        current window $program == ["Zathura"] ==> tag activity:pdf,
        current window $program == ["FBReader"] ==> tag activity:fiction,

        tag apps:$current.program, -- just tags the current program

        -- projects at work
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
          ==> tag project:$1-$2,
        current window ($program == "Alacritty" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/src/.*-([a-zA-Z0-9]*)/!)
          ==> tag project:$1-$2,

        -- personal projects
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/workspace/([a-zA-Z0-9]*)/([a-zA-Z0-9]*)/!)
          ==> tag project:$1-$2,
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.xmonad/!) ==> tag project:xmonad-config,
        current window ($program == "Alacritty" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.xmonad/!) ==> tag project:xmonad-config,
        current window ($program == "emacs" && $title =~ m!(?:~|home/${config.attributes.mainUser.name})/.emacs.d/!) ==> tag project:emacs-config,
        current window ($program == "emacs" && $title =~ m!(?:/etc)/nixos/!) ==> tag project:nixos-config,

        current window ($program == "Navigator" && $title =~ /Facebook/) ==> tag site:facebook,
        current window ($program == "Navigator" && $title =~ /Gmail/) ==> tag web:Gmail,
        current window ($program == "Navigator" && $title =~ /Google/) ==> tag web:Google,
        current window ($program == "Navigator" && $title =~ /wikipedia/) ==> tag site:wikipedia,
        current window ($program == "Navigator" && $title =~ /habr/) ==> tag site:habr,
        current window ($program == "Navigator" && $title =~ /pypi/) ==> tag site:pypi,
        current window ($program == "Navigator" && $title =~ /stackoverflow/) ==> tag site:stackoverflow,

        current window ($title =~ /^emacs - [^ ]+\.c .*$/) ==> tag edit:c,
        current window ($title =~ /^emacs - [^ ]+\.py .*$/) ==> tag edit:python,
        current window ($title =~ /^emacs - [^ ]+\.hs .*$/) ==> tag edit:haskell,
        current window ($title =~ /^emacs - [^ ]+\.lisp .*$/) ==> tag edit:cl,
        current window ($title =~ /^emacs - [^ ]+\.el .*$/) ==> tag edit:elisp,
        current window ($title =~ /^emacs - [^ ]+config\.org .*$/) ==> tag edit:elisp,
        current window ($title =~ /^emacs - [^ ]+\.pdf .*$/) ==> tag activity:pdf,

        -- $time evaluates to local time.
        $time >=  2:00 && $time <  8:00 ==> tag time-of-day:night,
        $time >=  8:00 && $time < 12:00 ==> tag time-of-day:morning,
        $time >= 12:00 && $time < 14:00 ==> tag time-of-day:lunchtime,
        $time >= 14:00 && $time < 18:00 ==> tag time-of-day:afternoon,
        $time >= 18:00 && $time < 22:00 ==> tag time-of-day:evening,
        $time >= 22:00 || $time <  2:00 ==> tag time-of-day:late-evening,

        -- This tag always refers to the last 24h
        $sampleage <= 24:00 ==> tag last-day,
        -- ...and last hour respectively
        $sampleage <= 1:00 ==> tag last-hour,

        -- year/months
        year $date == 2014 ==> tag year:2014,
        year $date == 2015 ==> tag year:2015,
        year $date == 2016 ==> tag year:2016,
        year $date == 2017 ==> tag year:2017,
        year $date == 2018 ==> tag year:2018,
        month $date == 1 ==> tag month:January,
        month $date == 2 ==> tag month:February,
        month $date == 3 ==> tag month:March,
        month $date == 4 ==> tag month:April,
        month $date == 5 ==> tag month:May,
        month $date == 6 ==> tag month:June,
        month $date == 7 ==> tag month:July,
        month $date == 8 ==> tag month:August,
        month $date == 9 ==> tag month:September,
        month $date == 10 ==> tag month:October,
        month $date == 11 ==> tag month:November,
        month $date == 12 ==> tag month:December,

        day of month $now == day of month $date ==> tag current-day,
        day of week $date == 1 ==> tag week:Monday,
        day of week $date == 2 ==> tag week:Tuesday,
        day of week $date == 3 ==> tag week:Wednesday,
        day of week $date == 4 ==> tag week:Thursday,
        day of week $date == 5 ==> tag week:Friday,
        day of week $date == 6 ==> tag week:Saturday,
        day of week $date == 7 ==> tag week:Sunday,

        month $now == month $date ==> tag current-month,
        year $now == year $date ==> tag current-year,
      '';
      ".emacs.d/resources/yasnippet" = {
        source = ../../pkgs/forges/github.com/wiedzmin/yasnippet-snippets;
        recursive = true;
      };
      ".authinfo.gpg".source = ./secrets/.authinfo.gpg;
    };
  };
}
