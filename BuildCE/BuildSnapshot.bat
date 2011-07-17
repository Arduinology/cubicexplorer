RD /Q /S Snapshot\CubicExplorer_dev
MD Snapshot\CubicExplorer_dev

copy ..\CubicExplorer.exe Snapshot\CubicExplorer_dev
copy ..\7z.dll Snapshot\CubicExplorer_dev
copy ..\settings.xml Snapshot\CubicExplorer_dev
copy ..\layout.xml Snapshot\CubicExplorer_dev
copy ..\sessions.xml Snapshot\CubicExplorer_dev
copy ..\bookmarks.xml Snapshot\CubicExplorer_dev
copy ..\Documents\Readme.txt Snapshot\CubicExplorer_dev
copy ..\Documents\License.txt Snapshot\CubicExplorer_dev
xcopy ..\locale Snapshot\CubicExplorer_dev\Locale\ /Y /E /EXCLUDE:SnapshotExcludes.txt
xcopy ..\Skins Snapshot\CubicExplorer_dev\Skins\ /Y /E /EXCLUDE:SnapshotExcludes.txt

REM PAUSE