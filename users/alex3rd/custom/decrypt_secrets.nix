{ bash, config, gnupg, lib, pkgs, ... }:
# TODO: make available at bootstrap
''
  #!${bash}/bin/bash

  SECRETS_DIR=/etc/nixos/users/alex3rd/secrets

  for secret in "$SECRETS_DIR"/*.gpg
  do
      SECRET_NAME=$(basename "$secret")
      DECRYPTED_NAME="''${SECRET_NAME%.*}"
      ${gnupg}/bin/gpg -dq "$SECRETS_DIR/$SECRET_NAME" > "$SECRETS_DIR/$DECRYPTED_NAME"
  done
''
