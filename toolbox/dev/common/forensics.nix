{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        binutils
        datefudge
        diffoscope
        elfinfo
        flamegraph
        gdb
        gdbgui
        idutils
        jid
        jq
        ltrace
        netsniff-ng
        ngrep
        patchelf
        patchutils
        pcapfix
        radare2
        radare2-cutter
        valgrind
    ];
}
