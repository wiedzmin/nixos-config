PACKAGE=$1
TMP=`@mktempBinary@ -d`
cd $TMP
{
    @nixShellBinary@ "<nixpkgs>" -A $PACKAGE --run "unpackPhase"
    @mvBinary@ * a
    @cpBinary@ -r a b
    $EDITOR b
} 2>&1 > /dev/null
@diffBinary@ -u --suppress-common-lines a b
