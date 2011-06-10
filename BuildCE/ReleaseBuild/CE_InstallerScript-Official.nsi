;=== pre process files ==========================================

;!system 'upx -9 CubicExplorer.exe'

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
  !define VERSION "0.94"
  !define VERSION_STR "0.94"

;---Reserved Files---
  ReserveFile "ShortcutPage.ini"
  !insertmacro MUI_RESERVEFILE_INSTALLOPTIONS

;---Variables---
  Var INI_VALUE

;---general info---

  Name "CubicExplorer"
  Caption 'CubicExplorer ${VERSION_STR}'
  OutFile "Output\CubicExplorer_${VERSION}_Setup.exe"
  InstallDir "$PROGRAMFILES\CubicExplorer"

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

  !insertmacro MUI_PAGE_LICENSE "..\Snapshot\CubicExplorer\License.txt"
  !insertmacro MUI_PAGE_DIRECTORY
  Page custom SelectShortcutsPage
  !insertmacro MUI_PAGE_INSTFILES


  !define MUI_FINISHPAGE_RUN "$INSTDIR\CubicExplorer.exe"
  !define MUI_FINISHPAGE_RUN_TEXT "Launch CubicExplorer"
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

;================================================================
;=== Languages ==================================================

  !insertmacro MUI_LANGUAGE "English"

;================================================================
;=== Functions ==================================================

Function CreateStartMenuShortcuts
  CreateDirectory "$SMPROGRAMS\CubicExplorer"
  CreateShortCut "$SMPROGRAMS\CubicExplorer\CubicExplorer.lnk" "$INSTDIR\CubicExplorer.exe"
  CreateShortCut "$SMPROGRAMS\CubicExplorer\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
FunctionEnd

Function CreateDesktopShortcuts
  CreateShortCut "$DESKTOP\CubicExplorer.lnk" "$INSTDIR\CubicExplorer.exe"
FunctionEnd

;================================================================
;=== Installer Sections =========================================

Section "Install"

  SetOutPath "$INSTDIR"

  ;---Override files---
  SetOverwrite "on"
  File "..\Snapshot\CubicExplorer\CubicExplorer.exe"
  File "..\Snapshot\CubicExplorer\License.txt"
  File "..\Snapshot\CubicExplorer\Readme.txt"
  File /r "..\Snapshot\CubicExplorer\Locale"
  File /r "..\Snapshot\CubicExplorer\Skins"

  ;---Don't override files---
  SetOverwrite "off"
  File "..\Snapshot\CubicExplorer\bookmarks.xml"
  File "..\Snapshot\CubicExplorer\layout.xml"
  File "..\Snapshot\CubicExplorer\settings.xml"
  File "..\Snapshot\CubicExplorer\sessions.xml"

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

  Delete "$SMPROGRAMS\CubicExplorer\CubicExplorer.lnk"
  Delete "$SMPROGRAMS\CubicExplorer\Uninstall.lnk"
  RMDir "$SMPROGRAMS\CubicExplorer"
  Delete "$DESKTOP\CubicExplorer.lnk"

  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\CubicExplorer.exe"
  Delete "$INSTDIR\bookmarks.xml"
  Delete "$INSTDIR\settings.xml"
  Delete "$INSTDIR\sessions.xml"
  Delete "$INSTDIR\layout.xml"
  Delete "$INSTDIR\Readme.txt"
  Delete "$INSTDIR\License.txt"

  RMDir /r "$INSTDIR"

SectionEnd

;================================================================