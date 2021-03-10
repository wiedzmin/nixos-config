.DEFAULT_GOAL := help
.PHONY: help

help:
	@grep -E '^[a-zA-Z0-9/_-]+:.*?' $(MAKEFILE_LIST) | grep -v help \
	| sed -n 's/^\(.*\): \(.*\)##\(.*\)/\1\3/p' \
	| column -t -s ' ' # TODO: investigate reformatting options

build: ## Build project
	go build -v ./...

build/docker: ## Build project's docker container
	docker-compose build

generate: ## Run generation commands
	go generate -v ./...

test: ## Run unit tests
	go test -v ./...

test/func: ## Run functional tests
	echo "add actual functional testing commands here"

lint: ## Lint cource code
	golangci-lint run ./...

lint/fix: ## Fix linter complaints
	golangci-lint run --fix ./...

deps/download: ## Download dependencies
	go mod download

deps/update: ## Update dependencies versions
	go get -u ./...

deps/gc: ## Garbage-collect dependencies
	go mod tidy

publish: generate build lint ## Publish source code updates
	git push

publish/force: generate build lint ## Publish source code updates (forcefully)
	git push --force-with-lease

pre-commit/install: ## Install "pre-commit" hooks
	pre-commit install

pre-commit/update-hooks: ## Update "pre-commit" hooks
	pre-commit autoupdate
