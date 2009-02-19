CD ..\source
REM ggdxgettext .\
"D:\Delphi Components\dxgettext\dxgettext" --delphi -o language_files
CD language_files
"D:\Delphi Components\dxgettext\msgremove" default.po -i ignore.po -o tmp.po
"D:\Delphi Components\dxgettext\msgcat" tmp.po include.po -o ..\..\Locale\default.pot
DEL tmp.po
CD ..\..\BuildCE
PAUSE