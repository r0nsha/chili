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
cd %dir%
xcopy /E /I /S /F ..\..\target\%mode%\chili.exe chili.exe
@REM copy -r ..\..\lib lib
