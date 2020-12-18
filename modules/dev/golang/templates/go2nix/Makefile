build:
	go build -v ./...

build/docker:
	docker-compose build

generate:
	go generate -v ./...

lint:
	golangci-lint run ./...

lint/fix:
	golangci-lint run --fix ./...

deps/download:
	go mod download

deps/update:
	go get -u ./...

deps/gc:
	go mod tidy

publish: generate build lint
	git push

publish/force: generate build lint
	git push --force-with-lease
