@echo off

echo Creating junction for .emacs.d

set batdir=%~dp0
mklink /j %APPDATA%\.emacs.d %batdir%.emacs.d
