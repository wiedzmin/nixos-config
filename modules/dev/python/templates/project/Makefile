.DEFAULT_GOAL := help
.PHONY: help

help:
	@grep -E '^[a-zA-Z0-9/_-]+:.*?' $(MAKEFILE_LIST) | grep -v help \
	| sed -n 's/^\(.*\): \(.*\)##\(.*\)/\1\3/p' \
	| column -t -s ' ' # TODO: investigate reformatting options

publish: generate build lint
	git push

publish/force: generate build lint
	git push --force-with-lease

# TODO: https://www.tweag.io/blog/2020-08-12-poetry2nix/
# TODO: https://github.com/nix-community/poetry2nix

# build:
# 	go build -v ./...

# build/docker:
# 	docker-compose build

# generate:
# 	go generate -v ./...

# test:
# 	go test -v ./...

# test/func:
# 	echo "add actual functional testing commands here"

# lint:
# 	golangci-lint run ./...

# lint/fix:
# 	golangci-lint run --fix ./...

# deps/download:
# 	go mod download

# deps/update:
# 	go get -u ./...

# deps/gc:
# 	go mod tidy

# publish: generate build lint
# 	git push

# publish/force: generate build lint
# 	git push --force-with-lease

# pre-commit/install:
# 	pre-commit install

# pre-commit/update-hooks:
# 	pre-commit autoupdate
