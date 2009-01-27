upx -9 ..\Snapshot\CubicExplorer_dev\CubicExplorer.exe

DEL Output\*.* /Q
"C:\Program Files\7-Zip\7z.exe" a -tzip Output\CubicExplorer_0.90rc3.zip ..\Snapshot\CubicExplorer_dev

"C:\Program Files\NSIS\makensis.exe" CE_InstallerScript.nsi

ECHO off
ECHO. 
ECHO.
ECHO.
ECHO === ALL DONE ===
ECHO.