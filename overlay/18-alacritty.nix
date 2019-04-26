self: super:

with super.rustPlatform;
let
    rpathLibs = with super; [
        expat
        freetype
        fontconfig
        xorg.libX11
        xorg.libXcursor
        xorg.libXxf86vm
        xorg.libXrandr
        libGL
        xorg.libXi
    ] ++ super.stdenv.lib.optionals super.stdenv.isLinux [
        wayland
        libxkbcommon
    ];
in rec {

    inherit mkDerivation;

    alacritty = super.rustPlatform.buildRustPackage rec {
        pname = "alacritty";
        version = "0.2.9";

        src = super.fetchFromGitHub {
            owner = "jwilm";
            repo = pname;
            rev = "v${version}";
            sha256 = "01wzkpbz6jjmpmnkqswilnn069ir3cx3jvd3j7zsvqdxqpwncz39";
        };

        cargoSha256 = "0h9wczgpjh52lhrqg0r2dkrh5svmyvrvh4yj7p0nz45skgrnl8w9";

        nativeBuildInputs = with super; [
            cmake
            makeWrapper
            pkgconfig
            ncurses
            gzip
        ];

        buildInputs = with super; rpathLibs ++ stdenv.lib.optionals stdenv.isDarwin [
            AppKit CoreFoundation CoreGraphics CoreServices CoreText Foundation OpenGL
            # Needed for CFURLResourceIsReachable symbols.
            cf-private
        ];

        outputs = [ "out" "terminfo" ];

        postPatch = ''
            substituteInPlace copypasta/src/x11.rs \
                --replace Command::new\(\"xclip\"\) Command::new\(\"${self.xclip}/bin/xclip\"\)
        '';

        postBuild = super.stdenv.lib.optionalString super.stdenv.isDarwin "make app";

        installPhase = ''
            runHook preInstall

            install -D target/release/alacritty $out/bin/alacritty

        '' + (if super.stdenv.isDarwin then ''
            mkdir $out/Applications
            cp -r target/release/osx/Alacritty.app $out/Applications/Alacritty.app
        '' else ''
            install -D alacritty.desktop $out/share/applications/alacritty.desktop
            patchelf --set-rpath "${super.stdenv.lib.makeLibraryPath rpathLibs}" $out/bin/alacritty
        '') + ''
            install -D alacritty-completions.zsh "$out/share/zsh/site-functions/_alacritty"
            install -D alacritty-completions.bash "$out/etc/bash_completion.d/alacritty-completions.bash"
            install -D alacritty-completions.fish "$out/share/fish/vendor_completions.d/alacritty.fish"

            install -dm 755 "$out/share/man/man1"
            gzip -c alacritty.man > "$out/share/man/man1/alacritty.1.gz"

            install -dm 755 "$terminfo/share/terminfo/a/"
            tic -x -o "$terminfo/share/terminfo" alacritty.info
            mkdir -p $out/nix-support
            echo "$terminfo" >> $out/nix-support/propagated-user-env-packages

            runHook postInstall
        '';

        dontPatchELF = true;

        meta = with super.stdenv.lib; {
            description = "GPU-accelerated terminal emulator";
            homepage = https://github.com/jwilm/alacritty;
            license = with super.stdenv.lib.licenses; [ asl20 ];
            platforms = [ "x86_64-linux" "x86_64-darwin" ];
        };
    };
}
