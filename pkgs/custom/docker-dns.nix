{ bash, docker, ... }:
''
    #!${bash}/bin/bash

    # --hostname dns.mageddo
    ${docker}/bin/docker run \
                         --restart=unless-stopped \
                         -p 5380:5380 \
                         -v /var/run/docker.sock:/var/run/docker.sock \
                         -v /etc/resolv.conf:/etc/resolv.conf \
                         defreitas/dns-proxy-server
''
