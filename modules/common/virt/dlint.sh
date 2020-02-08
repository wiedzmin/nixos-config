if [ -z $1 ]; then
    echo "Dockerfile missing"
    exit 1
fi

@dockerBinary@ run --rm -i -v $(realpath $1):/Dockerfile redcoolbeans/dockerlint
