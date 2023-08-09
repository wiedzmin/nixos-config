{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  wmCommon.wsMapping.rules = [
    {
      class = lib.last config.attributes.vt.default.windowClass;
      title = "main ${config.attributes.machine.name}";
      desktop = "shell";
    }
    {
      class = "TelegramDesktop";
      desktop = "im";
    }
    {
      class = mkWSMappingEbookReadersRegexp config.attributes.ebookreader;
      title = "bookshelf ${mkWSMappingEbookReadersExtsRegexp config.content.ebooks.extensions.primary}";
      desktop = "read";
      activate = true;
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http docs i3wm";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http facebook github";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http nixos planet";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http opennews";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "(Oo)rg http youtube";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "(Ee)macs http youtube";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http pravmir";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "http dzen";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "nixos github";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "documentation";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp config.attributes.browser;
      title = "rambler.ru";
      desktop = "ent";
    }
    {
      class = "lxqt-openssh-askpass";
      float = true;
    }
  ];
}
