{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        # TODO: wait for/find/dismiss github.com/davecheney/graphpkg in nixpkgs
        # TODO: wait for/find/dismiss github.com/davecheney/prdeps in nixpkgs
        # TODO: wait for/find/dismiss github.com/motemen/gore in nixpkgs
        # TODO: wait for/find/dismiss github.com/stretchr/gorc in nixpkgs
        # TODO: wait for/find/dismiss godoctor in nixpkgs
        # TODO: wait for/find/dismiss gomvpkg in nixpkgs
        # TODO: wait for/find/dismiss unbed in nixpkgs
        # TODO: wait for/find/dismiss gitlab.com/opennota/check in nixpkgs
        asmfmt
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
    ];
}
