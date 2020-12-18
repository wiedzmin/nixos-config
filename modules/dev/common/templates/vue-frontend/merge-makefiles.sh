#!/usr/bin/env bash
cat Makefile <(echo) Makefile-common > Makefile.result
rm Makefile-common
mv Makefile.result Makefile
echo "!!! Don't forget to check conflicts/etc in merged targets !!!"
