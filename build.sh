#!/bin/bash
set -o errexit
PREGEX="^Package: "
PKG=$(grep $PREGEX DESCRIPTION|sed "s/$PREGEX//")
echo Package from DESCRIPTION: $PKG

echo Installing and generating documentation
R CMD INSTALL .
R -e "inlinedocs::package.skeleton.dx('.')"

RELEASE=$PKG-release
echo Copying $PKG to $RELEASE
cd ..
rm -rf $RELEASE
cp -r $PKG $RELEASE

echo Editing $RELEASE for CRAN submission
grep -v Remotes $PKG/DESCRIPTION > $RELEASE/DESCRIPTION

echo Building $RELEASE
RCMD="R --vanilla CMD"
$RCMD build $RELEASE | tee build.out
PKG_TGZ=$(grep building build.out|sed "s/.*\($PKG.*.tar.gz\).*/\1/")

echo Checking $PKG_TGZ
$RCMD check --as-cran $PKG_TGZ
