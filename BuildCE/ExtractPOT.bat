CD ..\source
REM ggdxgettext .\
"C:\Documents and Settings\All Users\Delphi Files\Delphi 3rd Party Components\dxgettext\dxgettext" --delphi -o language_files
CD language_files
"C:\Documents and Settings\All Users\Delphi Files\Delphi 3rd Party Components\dxgettext\msgremove" default.po -i ignore.po -o tmp.po
"C:\Documents and Settings\All Users\Delphi Files\Delphi 3rd Party Components\dxgettext\msgcat" tmp.po include.po -o ..\..\Locale\default.pot
DEL tmp.po
CD ..\..\BuildCE
PAUSE