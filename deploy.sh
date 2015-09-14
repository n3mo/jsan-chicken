#!/bin/bash
# Produces a directory "jsan" containing a binary distribution for jsan

# Produces compressed directories containing binary distributions for massmine
if [ "$(uname)" == "Darwin" ]
then
    # The executable and linked library
    csc -optimize-level 5 -deploy jsan.scm

    # Add chicken eggs. This may not be necessary every time.
    chicken-install -deploy -p ./jsan args medea

    # Package everything up with the current version number (zip and tarball)
    zip jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.zip -r ./jsan/

    tar czf jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.tar.gz ./jsan/

    # Clean up by moving the results out of the development directory
    mv jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.zip ~/deploy
    mv jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-OSX-10.10.tar.gz ~/deploy

    # Remove the build directory
    rm -rf jsan/

elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]
then
    # The executable and linked library
    csc -optimize-level 5 -deploy jsan.scm

    # Add chicken eggs. This may not be necessary every time.
    chicken-install -deploy -p ./jsan args medea

    # Package everything up with the current version number (zip and tarball)
    zip jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.zip -r ./jsan/

    tar czf jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.tar.gz ./jsan/

    # Clean up by moving the results out of the development directory
    mv jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.zip ~/deploy
    mv jsan-`./jsan/jsan -v | grep -Eo '[0-9]+\.[0-9]+\.[0-9]+'`-linux-x86_64.tar.gz ~/deploy

    # Remove the build directory
    rm -rf jsan/

elif [ -n "$COMSPEC" -a -x "$COMSPEC" ]
then 
  echo $0: this script does not support Windows \:\(
fi

# end of file deploy.sh
