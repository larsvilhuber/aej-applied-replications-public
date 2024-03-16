#!/bin/bash

if [[ "$1" == "-h" ]]
then
cat << EOF
$0 (tag) (command)

will start interactive shell for tag  if command empty

or

will run with the command provided 
EOF
exit 0
fi

PWD=$(pwd)
. config.sh
shift

case $USER in
  *vilhuber|*herbert)
  WORKSPACE=$PWD
  ;;
  codespace)
  WORKSPACE=/workspaces
  ;;
esac
  
# build the docker if necessary

docker pull $space/$repo:$tag

OPTIONS="-it --rm --entrypoint /bin/bash -w /home/rstudio"
# OPTIONS="-e DISABLE_AUTH=true  --rm -p 8787:8787"

docker run -v "$WORKSPACE":/home/rstudio $OPTIONS $space/$repo:$tag $@
