if test $1 = "release"; then
    dir="dist/release"
    cargo build --release
    rm -rf $dir
    mkdir -p $dir
    cd $dir
    cp ../../target/release/chili chili
else
    dir="dist/debug"
    cargo build
    rm -rf $dir
    mkdir -p $dir
    cd $dir
    cp ../../target/debug/chili chili
fi

cp -r ../../lib lib
