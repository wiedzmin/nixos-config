self: super: {

  tridactyl-native = super.stdenv.mkDerivation rec {
    name = "tridactyl-native-${version}";
    # this is actually the version of tridactyl itself; the native messenger will
    # probably not change with every tridactyl version
    version = "1.16.1";

    src = super.fetchFromGitHub {
      owner = "tridactyl";
      repo = "tridactyl";
      rev = "be119b3d4e82923bb02620aa48555f96be4726d5"; # use hash since version is not tagget yet for some reason
      sha256 = "0lnf1zxkyaw7bk1wilzn9gaq8if05ssg4wfq8m8w5mp44xybav2c";
    };
    sourceRoot = "source/native";

    nativeBuildInputs = [ super.python3.pkgs.wrapPython ];

    buildPhase = ''
      sed -i -e "s|REPLACE_ME_WITH_SED|$out/share/tridactyl/native_main.py|" "tridactyl.json"
    '';

    installPhase = ''
      mkdir -p "$out/lib/mozilla/native-messaging-hosts"
      cp tridactyl.json "$out/lib/mozilla/native-messaging-hosts/"

      mkdir -p "$out/share/tridactyl"
      cp native_main.py "$out/share/tridactyl"
      wrapPythonProgramsIn "$out/share/tridactyl"
    '';

    meta = with super.stdenv.lib; {
      description = "Tridactyl native messaging host application";
      homepage = "https://github.com/tridactyl/tridactyl";
      license = licenses.asl20;
      platforms = platforms.all;
      maintainers = with maintainers; [ timokau ];
    };

  };

}
