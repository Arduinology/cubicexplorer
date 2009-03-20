CD ..\source
REM ggdxgettext .\
"E:\Delphi\Delphi Components\dxgettext\dxgettext" --delphi -o language_files
CD language_files
"E:\Delphi\Delphi Components\dxgettext\msgremove" default.po -i ignore.po -o tmp.po
"E:\Delphi\Delphi Components\dxgettext\msgcat" tmp.po include.po -o ..\..\Locale\default.pot
DEL tmp.po
CD ..\..\BuildCE
PAUSE