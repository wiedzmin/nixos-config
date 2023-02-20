{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.pim.timetracking;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    pim.timetracking = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable personal time-tracking infra/tools.";
      };
      inactiveSec = mkOption {
        type = types.int;
        default = 60;
        description = "arbtt inactivity treshold.";
      };
      inactiveTag = mkOption {
        type = types.str;
        default = "inactive";
        description = "arbtt tag for idle time. `inactive` is treated specially.";
      };
      rules = mkOption {
        type = types.lines;
        default = "";
        description = "arbtt tagging rules.";
      };
      config = mkOption {
        type = types.lines;
        default = ''
          $idle > ${builtins.toString cfg.inactiveSec} ==> tag ${cfg.inactiveTag},

          -- debug rules
          -- tag apps:$current.program,
          -- tag desktop:$desktop
          -- tag Activity:other_$current.program___$current.title, -- catch-all for new patterns and uncatched apps

          ${cfg.rules}

          (current window $program =~ /.*/) && ($idle <=60) ==> tag total-time:active,
          $idle > 60 ==> tag total-time:inactive,

          -- dates/time nitpicking
          year $date == 2014 ==> tag year:2014,
          year $date == 2015 ==> tag year:2015,
          year $date == 2016 ==> tag year:2016,
          year $date == 2017 ==> tag year:2017,
          year $date == 2018 ==> tag year:2018,
          year $date == 2019 ==> tag year:2019,
          year $date == 2020 ==> tag year:2020,

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

          day of week $date == 1 ==> tag week:Monday,
          day of week $date == 2 ==> tag week:Tuesday,
          day of week $date == 3 ==> tag week:Wednesday,
          day of week $date == 4 ==> tag week:Thursday,
          day of week $date == 5 ==> tag week:Friday,
          day of week $date == 6 ==> tag week:Saturday,
          day of week $date == 7 ==> tag week:Sunday,

          $sampleage <= 168:00 ==> tag current-week, -- current week
          $sampleage <= 24:00 ==> tag current-day, -- current 24h
          $sampleage <= 1:00 ==> tag current-hour, -- current hour

          $time >=  2:00 && $time <  8:00 ==> tag time-of-day:night,
          $time >=  8:00 && $time < 12:00 ==> tag time-of-day:morning,
          $time >= 12:00 && $time < 14:00 ==> tag time-of-day:lunchtime,
          $time >= 14:00 && $time < 18:00 ==> tag time-of-day:afternoon,
          $time >= 18:00 && $time < 22:00 ==> tag time-of-day:evening,
          $time >= 22:00 || $time <  2:00 ==> tag time-of-day:late-evening,

          $time < 1:00 ==> tag hour:00,
          $time >= 1:00 && $time < 2:00 ==> tag hour:01,
          $time >= 2:00 && $time < 3:00 ==> tag hour:02,
          $time >= 3:00 && $time < 4:00 ==> tag hour:03,
          $time >= 4:00 && $time < 5:00 ==> tag hour:04,
          $time >= 5:00 && $time < 6:00 ==> tag hour:05,
          $time >= 6:00 && $time < 7:00 ==> tag hour:06,
          $time >= 7:00 && $time < 8:00 ==> tag hour:07,
          $time >= 8:00 && $time < 9:00 ==> tag hour:08,
          $time >= 9:00 && $time < 10:00 ==> tag hour:09,
          $time >= 10:00 && $time < 11:00 ==> tag hour:10,
          $time >= 11:00 && $time < 12:00 ==> tag hour:11,
          $time >= 12:00 && $time < 13:00 ==> tag hour:12,
          $time >= 13:00 && $time < 14:00 ==> tag hour:13,
          $time >= 14:00 && $time < 15:00 ==> tag hour:14,
          $time >= 15:00 && $time < 16:00 ==> tag hour:15,
          $time >= 16:00 && $time < 17:00 ==> tag hour:16,
          $time >= 17:00 && $time < 18:00 ==> tag hour:17,
          $time >= 18:00 && $time < 19:00 ==> tag hour:18,
          $time >= 19:00 && $time < 20:00 ==> tag hour:19,
          $time >= 20:00 && $time < 21:00 ==> tag hour:20,
          $time >= 21:00 && $time < 22:00 ==> tag hour:21,
          $time >= 22:00 && $time < 23:00 ==> tag hour:22,
          $time >= 23:00 ==> tag hour:23,

          $sampleage <= 1:00 ==> tag recent:1h,
          $sampleage <= 2:00 ==> tag recent:2h,
          $sampleage <= 3:00 ==> tag recent:3h,
          $sampleage <= 4:00 ==> tag recent:4h,
          $sampleage <= 5:00 ==> tag recent:5h,
          $sampleage <= 6:00 ==> tag recent:6h,
          $sampleage <= 7:00 ==> tag recent:7h,
          $sampleage <= 8:00 ==> tag recent:8h,
          $sampleage <= 9:00 ==> tag recent:9h,
          $sampleage <= 10:00 ==> tag recent:10h,
          $sampleage <= 11:00 ==> tag recent:11h,
          $sampleage <= 12:00 ==> tag recent:12h,
          $sampleage <= 13:00 ==> tag recent:13h,
          $sampleage <= 14:00 ==> tag recent:14h,
          $sampleage <= 15:00 ==> tag recent:15h,
          $sampleage <= 16:00 ==> tag recent:16h,
          $sampleage <= 17:00 ==> tag recent:17h,
          $sampleage <= 18:00 ==> tag recent:18h,
          $sampleage <= 19:00 ==> tag recent:19h,
          $sampleage <= 20:00 ==> tag recent:20h,
          $sampleage <= 21:00 ==> tag recent:21h,
          $sampleage <= 22:00 ==> tag recent:22h,
          $sampleage <= 23:00 ==> tag recent:23h,

          month $date == month $now ==> tag current-month,
          year $now == year $date ==> tag current-year,

          {-
          Some "nontrivial" examples (mostly copypaste):
          arbtt-stats -f 'month $date==3 && year $date==2018'
          arbtt-stats  --filter='$date>='`date +"%Y-%m-%d"` --each-category

          if current window ($desktop == "stuff") then current window ($program == "urxvt") ==> tag Program:mutt else tag Desktop:$desktop,

          condition
            isJava = current window $program == ["sun-awt-X11-XFramePeer", "sun-awt-X11-XDialogPeer", "sun-awt-X11-XWindowPeer"]
          in $isJava && current window $title == "I3P" ==> tag Program:I3P,

          format $date =~ ".*-03-19"  ==> tag period:on_a_special_day,
          format $date =~ /.*-03-19/  ==> tag period:on_a_special_day,

          current window $program == "Firefox" ==> {
            (current window $title =~ m!(?:subreddit of | r/)(ranger)! || current window $title =~ m!ranger/(ranger)!) ==> tag Project:$1,
            current window $title =~ m!· ([^/·]+) / ([^/·]+) · GitLab! ==> tag Project:$1__$2,
            current window $title =~ m!([^/ ]+)/([^/@: ]+)(?:$|@|:)! ==> tag Project:$1__$2,
          },
          -}
        '';
        visible = false;
        readOnly = true;
        internal = true;
        description = "Whether to enable personal time-tracking infra/tools.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        tt_capture = mkPythonScriptWithDeps pkgs "tt_capture"
          (with pkgs; [
            nurpkgs.pystdlib
            python3Packages.cbor2
            python3Packages.pytz
            python3Packages.xlib
            xprintidle
          ])
          (builtins.readFile ./scripts/tt_capture.py);
      };
      services.arbtt = {
        enable = true;
        package = pkgs.haskellPackages.arbtt;
      };
      home-manager.users."${user}" = {
        home.file = { ".arbtt/categorize.cfg".text = cfg.config; };
        home.packages = with pkgs;
          [
            haskellPackages.arbtt # for stats viewing
          ] ++ lib.optionals config.attributes.debug.scripts [ tt_capture ];
      };
    })
  ];
}
