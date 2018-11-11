{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        go
        dep2nix
        go2nix

        # TODO: wait for/find/dismiss github.com/davecheney/graphpkg in nixpkgs
        # TODO: wait for/find/dismiss github.com/davecheney/prdeps in nixpkgs
        # TODO: wait for/find/dismiss github.com/motemen/gore in nixpkgs
        # TODO: wait for/find/dismiss github.com/stretchr/gorc in nixpkgs
        # TODO: wait for/find/dismiss go-guru in nixpkgs (+ emacs part)
        # TODO: wait for/find/dismiss godoctor in nixpkgs
        # TODO: wait for/find/dismiss gomvpkg in nixpkgs
        # TODO: wait for/find/dismiss gorename in nixpkgs
        # TODO: wait for/find/dismiss unbed in nixpkgs
        asmfmt
        deadcode
        dep
        errcheck
        glide
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
