Default(){ var=$1; default=$2; eval "if [ -z \"\$$var\" ]; then export $var=\"\$default\"; fi"; }

Default DEVROOT "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain"
export CC="$DEVROOT/usr/bin/clang"
export CXX="$DEVROOT/usr/bin/clang++"
export LD="$DEVROOT/usr/bin/ld"
export AR="$DEVROOT/usr/bin/ar"
export AS="$DEVROOT/usr/bin/as"
export NM="$DEVROOT/usr/bin/nm"
export RANLIB="$DEVROOT/usr/bin/ranlib"

Default PLATFORM "iPhoneOS"
Default SDKBASE "6.1"
Default SDKMIN "4.3"
export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/$PLATFORM.platform/Developer/SDKs/$PLATFORM$SDKBASE.sdk"
export HEADERSDIR="$HOME/tmp/ios-headers/$PLATFORM/$SDKBASE"
case $PLATFORM in
    (iPhoneOS) arch="-arch armv7 -arch armv7s" ;;
    (*) arch="-arch i386 -arch x86_64"
esac
export CPPFLAGS="$arch -isysroot $SDKROOT -I$SDKROOT/usr/include -I$HEADERSDIR"
# TODO: CFLAGS ?
export LDFLAGS="-L$SDKROOT/usr/lib/"
