if [ -z "$1" ]; then
  echo "Dockerfile missing"
  exit 1
fi

real_path=$(realpath "$1")
docker run --rm -i -v "$real_path:/tmp/Dockerfile" hadolint/hadolint hadolint /tmp/Dockerfile
