RD /Q /S Snapshot\CubicExplorer
MD Snapshot\CubicExplorer

copy ..\CubicExplorer.exe Snapshot\CubicExplorer

copy Snapshot_configs\settings.xml Snapshot\CubicExplorer
copy Snapshot_configs\layout.xml Snapshot\CubicExplorer
copy Snapshot_configs\sessions.xml Snapshot\CubicExplorer
copy Snapshot_configs\bookmarks.xml Snapshot\CubicExplorer

copy ..\Documents\Readme.txt Snapshot\CubicExplorer
copy ..\Documents\License.txt Snapshot\CubicExplorer
xcopy ..\locale Snapshot\CubicExplorer\Locale\ /Y /E /EXCLUDE:SnapshotExcludes.txt
xcopy ..\Skins Snapshot\CubicExplorer\Skins\ /Y /E /EXCLUDE:SnapshotExcludes.txt

REM PAUSE