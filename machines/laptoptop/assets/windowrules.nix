{ config, inputs, lib, pkgs, ... }:
with import ../../../modules/util.nix { inherit config inputs lib pkgs; };

rec {
  wmCommon.wsMapping.rules = [
    { # TODO: parameterize rule
      class = "Alacritty";
      title = "main laptoptop";
      desktop = "shell";
    }
    {
      class = "TelegramDesktop";
      desktop = "im";
    }
    {
      class = mkWSMappingEbookReadersRegexp;
      title = "bookshelf ${mkWSMappingEbookReadersExtsRegexp}";
      desktop = "read";
      activate = true;
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http blog";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http blogspot";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http docs i3wm";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http facebook github";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http fbclid";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http feedly";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http flake";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http github";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http gnu.org";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http nixos planet";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http opennews";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http radiosputnik.ria.ru";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http ria.ru";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http infox";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http reddit i3wm";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http reddit";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "(Oo)rg http youtube";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "(Ee)macs http youtube";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http pravmir";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http zen yandex";
      desktop = "ent";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "nixos github";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http wikipedia";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http stackoverflow";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "http python";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "documentation";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "gitlab";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "www.google.com";
      desktop = "web";
    }
    {
      class = mkWSMappingBrowsersRegexp;
      title = "rambler.ru";
      desktop = "ent";
    }
    {
      class = "lxqt-openssh-askpass";
      float = true;
    }
  ];
}
