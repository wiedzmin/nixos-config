if [ -z $1 ]; then
  echo "Dockerfile missing"
  exit 1
fi

docker run --rm -i -v $(realpath $1):/tmp/Dockerfile hadolint/hadolint hadolint /tmp/Dockerfile
