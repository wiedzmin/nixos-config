{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [
        cfr
        clojure
        leiningen
    ];
}
