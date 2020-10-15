build/laptoptop:
	nixos-rebuild build --flake ".#laptoptop"

build/momcat:
	nixos-rebuild build --flake ".#momcat"

build/no-net/laptoptop:
	nixos-rebuild build  --flake ".#laptoptop" --option binary-caches ''

switch/laptoptop:
	sudo nixos-rebuild switch --flake ".#laptoptop"

switch/momcat:
	sudo nixos-rebuild switch --flake ".#momcat"

clean:
	unlink ./result
