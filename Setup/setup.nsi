  SetCompressor lzma
# Variables
  !define Product "QuaBlocks"
  !define Version "2.0.0"

# Configuration
  Name "${Product} ${Version}"
  OutFile "..\QuaBlocksSetup.exe"
  InstallDir "$PROGRAMFILES\${Product}"
  InstallDirRegKey HKLM "Software\${Product}" "Install Dir"
  XPStyle On
  
# Modern UI Configuration
  !include "MUI.nsh"
  !define MUI_ICON "images\install.ico"
  !define MUI_UNICON "images\uninstall.ico"
  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "images\install.bmp"
  !define MUI_HEADERIMAGE_UNBITMAP "images\uninstall.bmp"
  !define MUI_HEADERIMAGE_RIGHT
  !define MUI_WELCOMEFINISHPAGE_BITMAP "images\welcome.bmp"
  !define MUI_ABORTWARNING
  !define MUI_WELCOMEPAGE_TITLE_3LINES

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE "License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES

  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES

  !insertmacro MUI_LANGUAGE "English"

# Begin Section
Section "QuaBlocks" SecMain
SectionIn RO
  ;Add files
  SetOutPath "$INSTDIR"
  SetOverWrite On
  File "QuaBlocks.exe"
  File "Changes.txt"
  File "License.txt"

  ;Create start-menu items
  CreateShortCut "$SMPROGRAMS\${Product}.lnk" "$INSTDIR\QuaBlocks.exe" "" "$INSTDIR\QuaBlocks.exe" 0
  CreateShortCut "$DESKTOP\${Product}.lnk" "$INSTDIR\QuaBlocks.exe" "" "$INSTDIR\QuaBlocks.exe" 0

  ; Write installation information to the registry
  WriteRegStr HKLM "Software\${Product}" "Install Dir" "$INSTDIR"
  WriteRegDWORD HKLM "Software\${Product}" "Install Language" $LANGUAGE

  ;Write uninstall information to the registry
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "DisplayIcon" "$INSTDIR\QuaBlocks.exe,0"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "DisplayName" "${Product}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "ModifyPath" "$INSTDIR"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${Product}" "NoModify" 1

  ; Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
SectionEnd

SubSection "Languages" SecLang
   
Section "English" SecLangEn
  ;Add files
  SetOutPath "$INSTDIR"
  SetOverWrite On
  File "QuaBlocksEN.lang"
SectionEnd

Section "Français" SecLangFr
  ;Add files
  SetOutPath "$INSTDIR"
  SetOverWrite On
  File "QuaBlocksFR.lang"
SectionEnd

SubSectionEnd

# Uninstaller Section
Section "Uninstall"
  ;Delete Files And Directory
  Delete "$INSTDIR\*.*"
  RmDir "$INSTDIR"

  ;Delete Shortcuts
  Delete "$SMPROGRAMS\${Product}.lnk"
  Delete "$DESKTOP\${Product}.lnk"

  ;Delete Uninstaller And Unistall Registry Entries
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\${Product}"
  DeleteRegKey HKEY_LOCAL_MACHINE "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\${Product}"
SectionEnd