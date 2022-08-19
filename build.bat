@echo off

if "%~1" == "release" (
    set mode="release"
) else (
    set mode="debug"
)

set dir="dist\%mode%"

if %mode% == "release" (
    set mode="Release"
    cargo build --release
) else (
    cargo build
)

rd /s /q %dir%
mkdir %dir%
copy LLVM-C.dll %dir%\LLVM-C.dll >NUL
copy target\%mode%\chili.exe %dir%\chili.exe >NUL
xcopy lib %dir%\lib /E /I >NUL
