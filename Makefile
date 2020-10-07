build:
	nixos-rebuild build --flake .

build/no-net:
	nixos-rebuild build  --flake . --option binary-caches ''

switch:
	sudo nixos-rebuild switch --flake .

enable/laptoptop:
	ln -svf /etc/nixos/machines/laptoptop/default.nix /etc/nixos/configuration.nix

enable/momcat:
	ln -svf /etc/nixos/machines/momcat/default.nix /etc/nixos/configuration.nix

clean:
	unlink ./result
