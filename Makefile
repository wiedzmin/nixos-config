build:
	nixos-rebuild build

build/no-net:
	nixos-rebuild build --option binary-caches ''

switch:
	sudo nixos-rebuild switch

enable/laptoptop:
	ln -svf /etc/nixos/machines/laptoptop/default.nix /etc/nixos/configuration.nix

enable/momcat:
	ln -svf /etc/nixos/machines/momcat/default.nix /etc/nixos/configuration.nix

clean:
	unlink ./result
