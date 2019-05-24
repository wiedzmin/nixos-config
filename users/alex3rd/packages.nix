{ config, pkgs, ... }:
let
    staging = with pkgs; [
        # paperless // see docs
        amber
        cdimgtools
        certigo
        dnsenum
        dnsrecon
        extrace
        fierce
        gallery-dl
        glxinfo
        hdparm
        jd-gui
        jump
        libwhich
        lifelines
        mp3cat
        parinfer-rust
        pforth
        procs
        rmount
        semver-tool
        shell-hist
        sit
        termshark
        usbutils
        websocat
        whatstyle
        xtruss
        zmap
        zzuf
    ];
    debug = with pkgs; [ # binaries for PATH
        fd
        lsd
        xdotool
        xlibs.xwininfo
        xorg.xdpyinfo
    ];
    langClojure = with pkgs; [
        cfr
        clojure
        leiningen
    ];
    langGolangDevTools = with pkgs; [
        # TODO: wait for/find/dismiss github.com/davecheney/graphpkg in nixpkgs
        # TODO: wait for/find/dismiss github.com/davecheney/prdeps in nixpkgs
        # TODO: wait for/find/dismiss github.com/motemen/gore in nixpkgs
        # TODO: wait for/find/dismiss github.com/stretchr/gorc in nixpkgs
        # TODO: wait for/find/dismiss godoctor in nixpkgs
        # TODO: wait for/find/dismiss gomvpkg in nixpkgs
        # TODO: wait for/find/dismiss unbed in nixpkgs
        # TODO: wait for/find/dismiss gitlab.com/opennota/check in nixpkgs
        # asmfmt # FIXME: try to solve problem with priorities (with gotools)
        deadcode
        errcheck
        go-langserver
        go-tools
        gocode-gomod
        goconst
        goconvey
        gocyclo
        godef
        gogetdoc
        golint
        gometalinter
        gomodifytags
        gosec
        gotags
        gotools
        govers
        iferr
        impl
        ineffassign
        interfacer
        maligned
        manul
        reftools
        unconvert
        go-check
    ];
    langGolangInfraTools = with pkgs; [
        dep
        dep2nix
        glide
        go
        vgo2nix
    ];
    langPythonDevTools = with pkgs; [
        python3Packages.virtualenv
        python3Packages.virtualenvwrapper
        yapf
    ];
    forensics = with pkgs; [
        bbe
        binutils
        elfinfo
        flamegraph
        gdb
        gdbgui
        jid
        netsniff-ng
        ngrep
        patchelf
        patchutils
        pcapfix
        radare2
        radare2-cutter
        sysdig
        valgrind
        vmtouch
        vulnix
    ];
    devClients = with pkgs; [
        http-prompt
        httplab
        litecli # TODO: shell automation: fzf for selecting db file, you get the idea
        mycli
        nodePackages.elasticdump
        pgcenter
        pgcli
        redis-tui
        soapui
        wuzz
        zeal
    ];
    vcsGit = with pkgs; [
        git-quick-stats
        git-sizer
        gitAndTools.ghq
        gitAndTools.git-absorb # TODO: review abilities and maybe use in some automation
        gitAndTools.git-extras
        gitAndTools.git-imerge
        gitAndTools.pass-git-helper
    ];
    ide = with pkgs; [
        vim
        icdiff
    ];
    devMisc = with pkgs; [
        cloc
        dotnet-sdk              # for building some binary releases
        glogg
        gron
        hyperfine
        just
        k6
        miniserve
        mkcert
        pciutils
        sloccount
        socat
        sslscan
        tcpreplay
        traceroute
        ttyplot
        visidata # TODO: make overlay
        websocat
        weighttp
        wirelesstools
        xurls
    ];
    monitoring = with pkgs; [
        reflex
        watchexec
        gping
        jnettop
        nethogs
        nload
        speedtest-cli
        gotop
        iotop
        lsof
        psmisc
    ];
    clients = with pkgs; [
        aria2
        qbittorrent
        skype
        slack
        tdesktop
        teamviewer
        w3m
        zoom-us
    ];
    scanner = with pkgs; [ # enable when needed
        deskew
        scantailor-advanced
        simple-scan
        xsane # temporarily kept for debug
    ];
    mediaMisc = with pkgs; [
        (mpv-with-scripts.override ({
            scripts = [ mpvScripts.mpris ];
        }))
        android-file-transfer
        exif
        ffmpeg
        gimp
        haskellPackages.arbtt
        maim
        mimeo
        minitube
        playerctl
        python3Packages.mps-youtube
        xsel # for firefox native clients
        you-get
        youtube-dl
    ];
    shell = with pkgs; [
        archiver
        cabextract
        fpart
        jdupes
        rmlint
        unshield
    ] ++ [
        xsv
    ] ++ [
        bc
        dateutils
        dex
        doitlive
        gcalcli
        loop
        mc
        plan9port
        renameutils
        replace
        shellcheck
        tree
        unicode-paracode
        wtf
        tmsu
    ] ++ [
        rdfind
    ] ++ [
        ntfy
        pv
        up
    ] ++ [
        optimize-nix
    ] ++ [
        eternal-terminal
    ] ++ [
        fpp
        tmux
    ];
    text = with pkgs; [
        calibre
        djview
        djvulibre
        ghostscript
        pandoc
        pdfgrep
        pdfsandwich
        pdftk
        zathura
    ];
    security = with pkgs; [
        (pass.withExtensions (ext: with ext; [ pass-audit pass-import pass-update ]))
        gnupg
        paperkey
        pinentry
        rofi-pass
        srm
    ];
    nix = with pkgs; [
        nix-index # TODO: maybe make easier shell alias
        nix-prefetch
        nix-prefetch-github
        nix-prefetch-scripts
    ] ++ [
        nix-zsh-completions
    ] ++ [
        nodePackages.node2nix
        pypi2nix
    ];
in
{
    home-manager.users.alex3rd = {
        home.packages = langClojure ++
                        langGolangDevTools ++
                        langGolangInfraTools ++
                        langPythonDevTools ++
                        forensics ++
                        devClients ++
                        vcsGit ++
                        ide ++
                        devMisc ++
                        monitoring ++
                        clients ++
                        mediaMisc ++
                        shell ++
                        text ++
                        security ++
                        nix ++
                        debug ++
                        staging;
    };
}
