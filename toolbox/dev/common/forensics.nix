{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
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
}
