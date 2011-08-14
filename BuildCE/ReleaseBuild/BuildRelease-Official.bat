REM upx -9 ..\Snapshot\CubicExplorer_dev\CubicExplorer.exe

DEL Output\*.* /Q
"C:\Program Files\7-Zip\7z.exe" a -tzip Output\CubicExplorer_0.95.zip ..\Snapshot\CubicExplorer

"C:\Program Files\NSIS\makensis.exe" CE_InstallerScript-Official.nsi

ECHO off
ECHO. 
ECHO.
ECHO.
ECHO === ALL DONE ===
ECHO.
pause