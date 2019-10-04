{ pkgs }: {
  "web-media-controller-host" = builtins.toJSON {
    name = "me.f1u77y.web_media_controller";
    description = "Allows controlling embedded players (YT, etc) via MPRIS";
    path = "${pkgs.wmc-mpris}/bin/web-media-controller";
    type = "stdio";
    allowed_extensions = [ "web-media-controller@f1u77y.me" ];
  };
  "passff-host" = builtins.toJSON {
    name = "passff";
    description = "Host for communicating with zx2c4 pass";
    path = "${pkgs.passff-host}/share/passff-host/passff.py";
    type = "stdio";
    allowed_extensions = [ "passff@invicem.pro" ];
  };
}
