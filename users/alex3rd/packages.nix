{ config, pkgs, ... }:
with import ./const.nix { inherit config pkgs; };
let
  # TODO: write script to query keywords which would provide packages names either from installed or whole nixpkgs
  stagingInactive = with pkgs; [
    apfs-fuse
    chrome-export
    dia
    drawio
    drm_info
    gopass
    keychain
    libvirt # for `vagrant plugin install vagrant-libvirt`
    nfs-utils # for vagrant
    paperless # see docs
    quassel
    rclone
    rmount # https://github.com/Luis-Hebendanz/rmount
    seturgent
    xdg-user-dirs
    xlsfonts
  ];
  stagingCommon = with pkgs; [
    _3llo
    aerc
    checkbashism
    curlie
    dua
    gitAndTools.git-subrepo
    grab-site # https://github.com/ArchiveTeam/grab-site
    ix
    j4-dmenu-desktop
    lazydocker
    out-of-tree
    pciutils
    psrecord
    quilt
    textql
    wire-desktop
    yj
  ];
  stagingWork = with pkgs; [
    drone
    drone-cli
    jenkins
    python3Packages.deprecated
    python3Packages.unittest-data-provider
    terracognita
    terraform
    tflint
  ];
  stagingPublish = with pkgs; [ ocrmypdf pdfcpu pdfgrep pdfsandwich pdftk ];
  sandbox = with pkgs; [
    # binaries for PATH
    # transmission + service https://transmissionbt.com/ + stig
    gnuplot # ? misc
    hstr # ? misc shell
    ncmpcpp
    uget
    xdotool
    xlibs.xwininfo
    xorg.xdpyinfo
    xprintidle-ng
  ];
  devClojure = with pkgs; [ boot cfr clojure leiningen ];
  devGolangTools = with pkgs; [
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
  devGolangInfra = with pkgs; [ dep dep2nix glide go go2nix vgo2nix ];
  devPythonTools = with pkgs; [ python3Packages.virtualenv python3Packages.virtualenvwrapper yapf ];
  devClients = with pkgs; [
    anydesk
    http-prompt
    httplab
    litecli # TODO: shell automation: skim for selecting db file, you get the idea
    mycli
    nodePackages.elasticdump
    pgcenter
    pgcli
    redis-tui
    soapui
    wuzz
    zeal
  ];
  devVcsGit = with pkgs; [
    git-crecord
    git-sizer
    gitAndTools.ghq
    gitAndTools.git-absorb # TODO: review abilities and maybe use in some automation
    gitAndTools.git-extras
    gitAndTools.git-octopus
    gitAndTools.pass-git-helper
    gitAndTools.topGit
    gitstats
    proposed.gitAndTools.git-quick-stats
  ];
  virt = with pkgs;
    let custom = import ../../pkgs/custom pkgs config;
    in [
      ctop
      custom.docker-machine-export
      custom.docker-machine-import
      dive
      docker-machine
      docker_compose
      kvm
      libcgroup
      promoter
      spice
      spice-gtk
      tigervnc
      vagrant
      virtmanager
      virtviewer
    ];
  virtRare = [ skopeo ];
  forensics = with pkgs; [
    bbe
    binutils
    elfinfo
    flamegraph
    gdb
    gdbgui
    hopper
    # jd-gui
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
    vnstat
    vulnix
  ];
  devIde = with pkgs; [ icdiff vimHugeX ];
  vim_plugins = with pkgs.vimPlugins; [
    bufexplorer
    command-t
    direnv-vim
    editorconfig-vim
    emmet-vim
    fastfold
    jedi-vim
    vim-addon-nix
    vim-fugitive
    vim-gitbranch
    vim-gitgutter
    vim-go
    vim-indent-object
    vim-isort
    vim-javascript
    vim-jinja
    vim-json
    vim-jsonnet
    vim-lastplace
    vim-nerdtree-tabs
    vim-nix
    vim-parinfer
    vim-projectionist
    vim-repeat
    vim-snipmate
    vim-surround
    vim-yapf
    youcompleteme
    zenburn
  ];
  devMisc = with pkgs; [
    certigo
    cloc
    dnsrecon
    dotnet-sdk # for building some binary releases
    extrace
    glogg
    gron
    hyperfine
    ipcalc
    just
    k6
    libwhich
    ltrace
    miniserve
    mkcert
    mr
    patchutils
    sloccount
    socat
    sslscan
    tcpreplay
    tokei
    vcstool
    websocat
    weighttp
    wiggle
    xtruss
    xurls
  ];
  devMiscRare = with pkgs; [ cachix ];
  monitoring = with pkgs; [
    bmon
    gotop
    gping
    iotop
    jnettop
    lsof
    nethogs
    nload
    pagemon
    psmisc
    python3Packages.glances
    reflex
    speedtest-cli
    watchexec
  ];
  miscClients = with pkgs; [ aria2 inetutils qbittorrent skype slack tdesktop w3m-full ];
  miscClientsRare = with pkgs; [ teamviewer zoom-us ];
  scanner = with pkgs; [
    # enable when needed
    deskew
    scantailor-advanced
    simple-scan
    xsane # temporarily kept for debug
  ];
  org = with pkgs; [
    remind # + rem2ics (make overlay)
    wyrd
  ];
  miscMedia = with pkgs;
    [
      # mpv
      # FIXME: make closure for last working version
      # (
      #   mpv-with-scripts.override (
      #     {
      #       scripts = [ mpvScripts.mpris ];
      #     }
      #   )
      # )
      android-file-transfer
      ccextractor
      clipgrab
      desktop-file-utils
      exif
      exiftool
      exiv2
      ffmpeg
      gallery-dl
      haskellPackages.arbtt
      imv
      jmtpfs # consider providing some (shell) automation
      maim
      mimeo
      minitube
      mpd-mpris # TODO: incorporate into user infrastructure
      mps-youtube # TODO: make script + service to backup playlists data
      playerctl
      shared-mime-info
      testdisk
      xsel # for firefox native clients
      you-get
      ytcc
    ] ++ [
      gimp
      ttyplot
      visidata # TODO: make overlay
    ];
  system = with pkgs; [ acpitool dmidecode iw lshw pciutils usbutils wirelesstools ];
  shell = with pkgs;
    [
      # docker-slim # TODO: make package https://github.com/docker-slim/docker-slim
      # tsvutils # TODO: make package https://github.com/brendano/tsvutils
      archiver
      fd
      fpart
      jdupes
      lsd
      lzip
      moreutils
      nq
      pbzip2
      pigz
      ripgrep-all
      rmlint
      sd
      unar
      unshield
    ] ++ [ xsv ] ++ [
      bc
      dateutils
      dex
      doitlive
      # gcalcli
      loop
      mc
      plan9port
      replace
      shellcheck
      tmsu
      tree
      unicode-paracode
      wtf
    ] ++ [ rdfind ] ++ [ most ntfy procs progress pv shell-hist up xe ] ++ [ eternal-terminal ] ++ [ fpp skim tmux ]
    ++ [ tmatrix ];
  text = with pkgs; [
    # calibre
    # python3Packages.weasyprint
    djview
    djvulibre
    enca
    ghostscript
    pandoc
  ];
  security = with pkgs; [
    (pass.withExtensions (ext: with ext; [ pass-audit pass-import pass-update ]))
    clair # https://werner-dijkerman.nl/2019/01/28/scanning-docker-images-with-coreos-clair/
    gnupg
    paperkey
    rofi-pass
    srm
  ];
  nix = with pkgs;
    [
      nix-index # TODO: maybe make easier shell alias
      nix-prefetch
      nix-prefetch-github
      nix-prefetch-scripts
      nixfmt
    ] ++ [ nix-zsh-completions ] ++ [ nodePackages.node2nix pypi2nix ];
in {
  home-manager.users."${userName}" = {
    home.packages = devClients ++ devGolangInfra ++ devGolangTools ++ devIde ++ devMisc ++ devPythonTools ++ devVcsGit
      ++ forensics ++ miscClients ++ miscMedia ++ monitoring ++ nix ++ org ++ sandbox ++ security ++ shell
      ++ stagingCommon ++ stagingPublish ++ stagingWork ++ text ++ virt;
  };
}
