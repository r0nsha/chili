# mode = "${$1,,}"

if test $1 = "release" 
then
    cargo build --release
    mkdir -p dist/release
    cd dist/release
    cp ../../target/release/chili chili
    cp -r ../../lib lib
else
    cargo build
    mkdir -p dist/debug
    cd dist/debug
    cp ../../target/debug/chili chili
fi

cp -r ../../lib lib