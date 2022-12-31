@echo off

if /I "%~1" == "release" (
    set mode="release"
) else if /I "%~1" == "debug" (
    set mode="debug"
) else (
    echo "Usage: build.bat [release|debug]"
    exit /b 1   
)

set dir="dist\%mode%"

if %mode% == "release" (
    set mode="Release"
    cargo build --release
) else (
    cargo build
)

rmdir /s /q %dir% 2>NUL
mkdir %dir% >NUL
copy llvm\windows\LLVM-C.dll %dir%\LLVM-C.dll >NUL
copy target\%mode%\chili.exe %dir%\chili.exe >NUL
xcopy lib %dir%\lib /E /I >NUL
xcopy llvm\windows\bin\* %dir%\* /E /I >NUL
