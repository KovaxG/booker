@echo off
echo Cleaning old build...
del /Q build

echo Building program...
ghc Freshmaster.hs
mkdir build

echo Moving files to new directory
move *.exe build
del *.o
del *.hi
pause