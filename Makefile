build/laptoptop:
	nixos-rebuild build --flake ".#laptoptop"

build/laptoptop/debug:
	nixos-rebuild build --flake ".#laptoptop" --show-trace

build/momcat:
	nixos-rebuild build --flake ".#momcat"

build/momcat/debug:
	nixos-rebuild build --flake ".#momcat" --show-trace

build/no-net/laptoptop:
	nixos-rebuild build  --flake ".#laptoptop" --option binary-caches ''

switch/laptoptop:
	sudo nixos-rebuild switch --flake ".#laptoptop"

switch/momcat:
	sudo nixos-rebuild switch --flake ".#momcat"

switch/laptoptop/no-net:
	sudo nixos-rebuild switch --flake ".#laptoptop" --option binary-caches ''

switch/momcat/no-net:
	sudo nixos-rebuild switch --flake ".#momcat" --option binary-caches ''

clean:
	unlink ./result
