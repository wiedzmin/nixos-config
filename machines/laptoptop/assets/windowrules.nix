{ config, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  wmCommon.wsMapping.rules = [
    {
      class = "TelegramDesktop";
      desktop = "im"; # [ref:desktop_im]
    }
    {
      class = mkWSMappingEbookReadersRegexp config.attributes.ebookreader;
      title = "bookshelf ${mkWSMappingEbookReadersExtsRegexp config.content.ebooks.extensions.primary}";
      desktop = "read"; # [ref:desktop_read]
      activate = true;
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http facebook github";
      desktop = "main"; # [ref:desktop_main]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http nixos planet";
      desktop = "main"; # [ref:desktop_main]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http opennews";
      desktop = "main"; # [ref:desktop_main]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "(Oo)rg http youtube";
      desktop = "main"; # [ref:desktop_main]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "(Ee)macs http youtube";
      desktop = "main"; # [ref:desktop_main]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http pravmir";
      desktop = "var"; # [ref:desktop_var]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http dzen";
      desktop = "var"; # [ref:desktop_var]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "nixos github";
      desktop = "main"; # [ref:desktop_main]
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "rambler.ru";
      desktop = "var"; # [ref:desktop_var]
    }
    {
      class = "lxqt-openssh-askpass";
      float = true;
    }
  ];
}
