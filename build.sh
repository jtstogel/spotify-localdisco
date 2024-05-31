stack build

STACK_INSTALL_PATH=$(realpath -s --relative-to=$(pwd) $(stack path --local-install-root))

docker build -t localdisco . --build-arg STACK_INSTALL_PATH="$STACK_INSTALL_PATH"
