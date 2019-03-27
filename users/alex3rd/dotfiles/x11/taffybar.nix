{config, pkgs, lib, ...}:
with import ../const.nix {inherit config pkgs;};
let
    # TODO: try to make simpler / more concise
    currentArch = pkgs.lib.lists.last (pkgs.lib.lists.init (pkgs.lib.strings.splitString "-" builtins.currentSystem));
    currentOs = pkgs.lib.lists.last (pkgs.lib.strings.splitString "-" builtins.currentSystem);
in
{
    system.activationScripts.removeOldTaffybarBinary = ''
        TAFFYBAR_CACHED_BINARY=/home/${userName}/.cache/taffybar/taffybar-${currentOs}-${currentArch}
        if [ -f $TAFFYBAR_CACHED_BINARY ]; then
            rm $TAFFYBAR_CACHED_BINARY
        fi
    '';

    home-manager.users.alex3rd = {
        home.file = {
            ".config/taffybar/taffybar.hs".text = ''
                import Data.Maybe (fromMaybe)
                import Safe (headDef)
                import System.Environment (getArgs)
                import Text.Read (readMaybe)

                import System.Information.CPU
                import System.Information.CPU2
                import System.Taffybar
                import System.Taffybar.Battery
                import System.Taffybar.SimpleClock
                import System.Taffybar.Systray
                import System.Taffybar.TaffyPager
                import System.Taffybar.Widgets.PollingGraph

                cpuLoadCallback = do
                  (userLoad, systemLoad, totalLoad) <- cpuLoad
                  return [ userLoad, totalLoad, systemLoad ]

                cpuTempCallback = do
                  cpuTemp <- getCPUTemp ["cpu0"]
                  return cpuTemp

                main = do
                  args <- getArgs
                  -- let chosenMonitorNumber = fromMaybe 0 $ readMaybe $ headDef "0" args
                  let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                                  , graphLabel = Just ""
                                                  }
                      clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
                      pager = taffyPagerNew defaultPagerConfig
                      tray = systrayNew
                      cpuLoad = pollingGraphNew cpuCfg 0.5 cpuLoadCallback
                      -- cpuTemp = pollingGraphNew cpuCfg 0.5 cpuLoadCallback
                      battery = batteryBarNew defaultBatteryConfig 30
                  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                                        , endWidgets = [ tray, clock, cpuLoad, battery ] -- cpuTemp
                                                        }
            '';
            ".config/taffybar/taffybar.rc".text = ''
                style "default" {
                    font_name = "Iosevka Bold 9"
                }

                widget "Taffybar*" style "default"
            '';
        };
    };
}
