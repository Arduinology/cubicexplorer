;=== pre process files ==========================================



;================================================================
;=== Compression ================================================
  SetCompressor "LZMA"
  SetCompress "auto"

;================================================================
;=== Include ====================================================

  !include "MUI.nsh"
  !include "FileFunc.nsh"


;================================================================
;=== Initialize =================================================

;---Version info---
  !define VERSION "SVN"
  !define VERSION_STR "SVN Snapshot"

;---Reserved Files---
  ReserveFile "ShortcutPage.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;---Variables---
  Var INI_VALUE

;---general info---

  Name "CubicExplorer"
  Caption 'CubicExplorer ${VERSION_STR}'
  OutFile "Output\CubicExplorer_${VERSION}_Setup.exe"
  InstallDir "$PROGRAMFILES\CubicExplorer_dev"

;---OnInit---
Function .onInit
  !insertmacro MUI_INSTALLOPTIONS_EXTRACT "ShortcutPage.ini"
FunctionEnd

;================================================================
;=== Interface Settings =========================================
  ;!define MUI_HEADERIMAGE_BITMAP_NOSTRETCH
  !define MUI_ICON "Installer_Images\install.ico"
  !define MUI_UNICON "Installer_Images\uninstall.ico"
  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "Installer_Images\header.bmp"
  !define MUI_BGCOLOR "f0f0f0"
  !define MUI_WELCOMEFINISHPAGE_BITMAP "Installer_Images\finnish.bmp"
  !define MUI_ABORTWARNING


;================================================================
;=== Custom Pages ===============================================

Function SelectShortcutsPage
  !insertmacro MUI_HEADER_TEXT "Create Shortcuts" "Choose what shortcuts you want to create."
  !insertmacro MUI_INSTALLOPTIONS_DISPLAY "ShortcutPage.ini"
FunctionEnd

;================================================================
;=== Pages ======================================================

  !insertmacro MUI_PAGE_LICENSE "..\Snapshot\CubicExplorer_dev\License.txt"
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom SelectShortcutsPage
  !insertmacro MUI_PAGE_INSTFILES


  ; !define MUI_FINISHPAGE_RUN "$INSTDIR\CubicExplorer.exe"
  ; !define MUI_FINISHPAGE_RUN_TEXT "Launch CubicExplorer"
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;================================================================
;=== Languages ==================================================

  !insertmacro MUI_LANGUAGE "English"

;================================================================
;=== Functions ==================================================

Function CreateStartMenuShortcuts
  CreateDirectory "$SMPROGRAMS\CubicExplorer_dev"
  CreateShortCut "$SMPROGRAMS\CubicExplorer_dev\CubicExplorer.lnk" "$INSTDIR\CubicExplorer.exe"
  CreateShortCut "$SMPROGRAMS\CubicExplorer_dev\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
FunctionEnd

Function CreateDesktopShortcuts
  CreateShortCut "$DESKTOP\CubicExplorer_dev.lnk" "$INSTDIR\CubicExplorer.exe"
FunctionEnd

;================================================================
;=== Installer Sections =========================================

Section "Install"

  SetOutPath "$INSTDIR"

  ;---Override files---
  SetOverwrite "on"
  File "..\Snapshot\CubicExplorer_dev\CubicExplorer.exe"
  File "..\Snapshot\CubicExplorer_dev\License.txt"
  File "..\Snapshot\CubicExplorer_dev\Readme.txt"
  File /r "..\Snapshot\CubicExplorer_dev\Locale"
  File /r "..\Snapshot\CubicExplorer_dev\Skins"

  ;---Don't override files---
  SetOverwrite "off"
  File "..\Snapshot\CubicExplorer_dev\bookmarks.xml"
  File "..\Snapshot\CubicExplorer_dev\layout.xml"
  File "..\Snapshot\CubicExplorer_dev\settings.xml"
  File "..\Snapshot\CubicExplorer_dev\sessions.xml"
  File "..\Snapshot_configs\Installer\settings.path"

  ;---Create uninstaller---
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ;---Create StartMenu shortcuts---
  !insertmacro MUI_INSTALLOPTIONS_READ $INI_VALUE "ShortcutPage.ini" "Field 1" "State"
  StrCmp $INI_VALUE "1" "" +2
    Call CreateStartMenuShortcuts

  ;---Create Desktop shortcuts---
  !insertmacro MUI_INSTALLOPTIONS_READ $INI_VALUE "ShortcutPage.ini" "Field 2" "State"
  StrCmp $INI_VALUE "1" "" +2
    Call CreateDesktopShortcuts

SectionEnd

;================================================================
;=== Uninstaller Section ========================================

Section "Uninstall"

  Delete "$SMPROGRAMS\CubicExplorer_dev\CubicExplorer.lnk"
  Delete "$SMPROGRAMS\CubicExplorer_dev\Uninstall.lnk"
  RMDir "$SMPROGRAMS\CubicExplorer_dev"
  Delete "$DESKTOP\CubicExplorer_dev.lnk"

  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\CubicExplorer.exe"
  Delete "$INSTDIR\bookmarks.xml"
  Delete "$INSTDIR\settings.xml"
  Delete "$INSTDIR\sessions.xml"
  Delete "$INSTDIR\layout.xml"
  Delete "$INSTDIR\Readme.txt"
  Delete "$INSTDIR\License.txt"
  Delete "$INSTDIR\settings.path"

  RMDir "$INSTDIR"

SectionEnd

;================================================================