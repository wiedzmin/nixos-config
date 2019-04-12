{config, pkgs, lib, ...}:

{
    home-manager.users.alex3rd = {
        programs.autorandr = {
            enable = true;
            hooks = {
                postswitch = {
                    "rescale-wallpaper" = "${pkgs.rescale-wallpaper}/bin/rescale-wallpaper";
                };
                predetect = {
                    "kill-compton" = "${pkgs.kill-compton}/bin/kill-compton";
                };
            };
            profiles = {
                "mobile" = {
                    fingerprint = {
                        "LVDS-1" = "00ffffffffffff0006af6c100000000000140104901c10780220e5925554922825505400000001010101010101010101010101010101121b56585000193030203600159c100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231323558573031205630200a00ec";
                    };
                    config = {
                        "VGA-1".enable = false;
                        "HDMI-1".enable = false;
                        "DP-1".enable = false;
                        "HDMI-2".enable = false;
                        "HDMI-3".enable = false;
                        "DP-2".enable = false;
                        "DP-3".enable = false;
                        "LVDS-1" = {
                            enable = true;
                            primary = true;
                            position = "0x0";
                            mode = "1366x768";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.10";
                        };
                    };
                };
                "docked-home" = {
                    fingerprint = {
                        "HDMI-2" = "00ffffffffffff001e6dbc594f53010006170103803c2278ea3135a5554ea1260c5054a54b00714f81809500b300a9c0810081c09040023a801871382d40582c450056512100001e000000fd00384b1e530f000a202020202020000000fc003237454133330a202020202020000000ff0033303652414e4e324a3836330a00dd";
                        "HDMI-3" = "00ffffffffffff000469b124010101011d18010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0045374c4d51533037373132380a0023";
                        "LVDS-1" = "00ffffffffffff0006af6c100000000000140104901c10780220e5925554922825505400000001010101010101010101010101010101121b56585000193030203600159c100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231323558573031205630200a00ec";
                    };
                    config = {
                        "VGA-1".enable = false;
                        "HDMI-1".enable = false;
                        "DP-1".enable = false;
                        "DP-2".enable = false;
                        "DP-3".enable = false;
                        "HDMI-2" = {
                            enable = true;
                            position = "0x0";
                            mode = "1920x1080";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.00";
                        };
                        "HDMI-3" = {
                            enable = true;
                            position = "1366x1080";
                            mode = "1920x1080";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.00";
                            rotate = "left";
                        };
                        "LVDS-1" = {
                            enable = true;
                            primary = true;
                            position = "0x1080";
                            mode = "1366x768";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.10";
                        };
                    };
                };
                "docked-office" = {
                    fingerprint = {
                        "HDMI-2" = "00ffffffffffff0009d111804554000015180103803420782e4ca5a7554da226105054a56b8061c0810081809500d1c0b300a9400101283c80a070b023403020360006442100001a000000ff004e354530373434373031390a20000000fd00324c1e5311000a202020202020000000fc0042656e5120424c323431310a20003e";
                        "HDMI-3" = "00ffffffffffff000469b124010101010f17010380372378ea3d15a3544da027125054bfef00714f818081409500a940b300d1c00101283c80a070b023403020360022602100001a000000fd00324c1e5311000a202020202020000000fc0050413234380a20202020202020000000ff0044344c4d51533034313530370a003e";
                        "LVDS-1" = "00ffffffffffff0006af6c100000000000140104901c10780220e5925554922825505400000001010101010101010101010101010101121b56585000193030203600159c100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231323558573031205630200a00ec";
                    };
                    config = {
                        "VGA-1".enable = false;
                        "HDMI-1".enable = false;
                        "DP-1".enable = false;
                        "DP-2".enable = false;
                        "DP-3".enable = false;
                        "HDMI-2" = {
                            enable = true;
                            position = "1366x1200";
                            mode = "1920x1200";
                            gamma = "1.0:0.909:0.833";
                            rate = "59.95";
                            rotate = "left";
                        };
                        "HDMI-3" = {
                            enable = true;
                            position = "0x0";
                            mode = "1920x1200";
                            gamma = "1.0:0.909:0.833";
                            rate = "59.95";
                        };
                        "LVDS-1" = {
                            enable = true;
                            primary = true;
                            position = "0x1200";
                            mode = "1366x768";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.10";
                        };
                    };
                };
                "undocked-parents-dsub" = {
                    fingerprint = {
                        "VGA-1" = "00ffffffffffff004c2d0e0139314a4d100f01036c261e782aee95a3544c99260f5054bfef808180714f010101010101010101010101302a009851002a4030701300782d1100001e000000fd00384b1e510e000a202020202020000000fc0053796e634d61737465720a2020000000ff00485348593430323338330a202000d2";
                        "LVDS-1" = "00ffffffffffff0006af6c100000000000140104901c10780220e5925554922825505400000001010101010101010101010101010101121b56585000193030203600159c100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231323558573031205630200a00ec";
                    };
                    config = {
                        "HDMI-1".enable = false;
                        "HDMI-2".enable = false;
                        "HDMI-3".enable = false;
                        "DP-1".enable = false;
                        "DP-2".enable = false;
                        "DP-3".enable = false;
                        "VGA-1" = {
                            enable = true;
                            position = "0x0";
                            mode = "1280x1024";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.02";
                        };
                        "LVDS-1" = {
                            enable = true;
                            primary = true;
                            position = "0x1024";
                            mode = "1366x768";
                            gamma = "1.0:0.909:0.833";
                            rate = "60.00";
                        };
                    };
                };
            };
        };
    };
}
