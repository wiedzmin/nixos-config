{ ... }: {
  imports = [
    ./attributes.nix

    ./appearance/colorschemes/zenburn
    ./appearance/emacs
    ./appearance/fonts
    ./appearance/gtk
    ./appearance/wallpaper
    ./appearance/xresources
    ./browsers/chromium
    ./browsers/core
    ./browsers/firefox
    ./browsers/firefox/impl.nix
    ./browsers/nyxt
    ./browsers/qutebrowser
    ./content/ebooks
    ./content/media
    ./content/misc
    ./content/screenshots
    ./controlcenter
    ./dev/ccpp
    ./completion
    ./dev/dbms/misc
    ./dev/dbms/mysql
    ./dev/dbms/pgsql
    ./dev/dbms/sqlite
    ./dev/projectenv
    ./dev/direnv
    ./dev/editorconfig
    ./dev/frontend
    ./dev/git/autofetch
    ./dev/git/autopush
    ./dev/git/batch
    ./dev/git/core
    ./dev/git/forges
    ./dev/git/misc
    ./dev/git/navigation
    ./dev/git/savewip
    ./dev/golang
    ./dev/misc
    ./dev/ml
    ./dev/navigation/codesearch
    ./dev/navigation/projects
    ./dev/python
    ./emacs/core
    ./emacs/edit
    ./emacs/history
    ./emacs/misc
    ./emacs/navigation
    ./email
    ./email/mbsync.nix
    ./gc
    ./knowledgebase
    ./navigation/bookmarks
    ./networking/core
    ./networking/hosts
    ./networking/messengers
    ./networking/nmconnections
    ./networking/ssh
    ./networking/vpn
    ./networking/wireless
    ./nix/cachix
    ./nix/core
    ./nix/dev
    ./nix/navigation
    ./paperworks
    ./pim/orgmode
    ./pim/scheduling
    ./pim/timetracking
    ./security
    ./shell/alacritty
    ./shell/bookmarks
    ./shell/core
    ./shell/prompts/liquid
    ./shell/prompts/ohmyzsh
    ./shell/prompts/starship
    ./shell/tmux
    ./shell/tmux/impl.nix
    ./shell/tools
    ./shell/twopanes
    ./shell/zsh
    ./virt/docker/core
    ./virt/docker/devdns
    ./virt/docker/swarm
    ./virt/libvirt
    ./virt/virtualbox
    ./wm
    ./wm/i3
    ./wm/stumpwm
    ./wm/xmonad
    ./workstation/input/core
    ./workstation/input/mouse
    ./workstation/input/xkeysnail
    ./workstation/lockscreen
    ./workstation/performance
    ./workstation/power/battery
    ./workstation/power/mgmt
    ./workstation/randr
    ./workstation/sound
    ./workstation/systemtraits
    ./workstation/video/backlight
    ./workstation/video/opengl
    ./workstation/video/transparency
  ];
}
