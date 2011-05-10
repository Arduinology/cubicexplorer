unit CE_FileUtils;

interface

uses
  // Tnt
  TntSysUtils, TntWindows,
  // System Units
  Windows, SysUtils, Dialogs, ShlObj;

type
  TCreateSymbolicLink = function(Link, Target: PWideChar; Flags: DWORD): BOOL; stdcall;

  REPARSE_MOUNTPOINT_DATA_BUFFER = packed record
    ReparseTag: DWORD;
    ReparseDataLength: DWORD;
    Reserved: Word;
    ReparseTargetLength: Word;
    ReparseTargetMaximumLength: Word;
    Reserved1: Word;
    ReparseTarget: array[0..0] of WideChar;
  end;
  TReparseMountPointDataBuffer = REPARSE_MOUNTPOINT_DATA_BUFFER;
  PReparseMountPointDataBuffer = ^TReparseMountPointDataBuffer;
    
const
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;

  REPARSE_MOUNTPOINT_HEADER_SIZE = 8;

  IO_REPARSE_TAG_RESERVED_ZERO  = $000000000;
  IO_REPARSE_TAG_SYMBOLIC_LINK  = IO_REPARSE_TAG_RESERVED_ZERO;
  IO_REPARSE_TAG_RESERVED_ONE   = $000000001;
  IO_REPARSE_TAG_RESERVED_RANGE = $000000001;
  IO_REPARSE_TAG_VALID_VALUES   = $0E000FFFF;
  IO_REPARSE_TAG_HSM            = $0C0000004;
  IO_REPARSE_TAG_NSS            = $080000005;
  IO_REPARSE_TAG_NSSRECOVER     = $080000006;
  IO_REPARSE_TAG_SIS            = $080000007;
  IO_REPARSE_TAG_DFS            = $080000008;
  IO_REPARSE_TAG_MOUNT_POINT    = $0A0000003;

  FILE_ANY_ACCESS = 0;
  FILE_READ_DATA  = 1;
  FILE_WRITE_DATA = 2;

  FILE_DEVICE_FILE_SYSTEM = $0009;

  METHOD_BUFFERED   = 0;
  METHOD_IN_DIRECT  = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER    = 3;    

  FSCTL_SET_REPARSE_POINT    = (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (41 shl 2) or (METHOD_BUFFERED);
  FSCTL_GET_REPARSE_POINT    = (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (42 shl 2) or (METHOD_BUFFERED);
  FSCTL_DELETE_REPARSE_POINT = (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (43 shl 2) or (METHOD_BUFFERED);
  

function CreateJunction(ALink: WideString; ATarget: WideString; ShowErrors:
    Boolean = true): Boolean;

function FileOrFolderExists(APath: WideString): Boolean;

implementation

{-------------------------------------------------------------------------------
  Create Junction
-------------------------------------------------------------------------------}
function CreateJunction(ALink: WideString; ATarget: WideString; ShowErrors:
    Boolean = true): Boolean;
var
  h: Cardinal;
  proc: TCreateSymbolicLink;
  flag: Cardinal;
  s: String;
  sr: TSearchRecW;
  Buffer: PReparseMountPointDataBuffer;
  BufSize: integer;
  TargetName: WideString;
  BytesRead: DWORD;
begin
  Result:= false;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    ATarget:= WideExcludeTrailingPathDelimiter(ATarget);
    ALink:= WideExcludeTrailingPathDelimiter(ALink);
    // Pre check: Make sure Link doesn't exist.
    if WideFindFirst(ALink, faAnyFile or faDirectory, sr) = 0 then
    begin
      if ShowErrors then
      MessageBox(0, 'File/Folder already exists.'#13#10'Please choose another link name.', 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
      WideFindClose(sr);
      Exit;
    end;

    // Pre checks: Make sure target exists. Is target folder of a file?
    if WideFindFirst(ATarget, faAnyFile or faDirectory, sr) = 0 then
    begin
      if (sr.Attr and faDirectory) = faDirectory then
      flag:= 1
      else
      flag:= 0;
      WideFindClose(sr);
    end
    else
    begin
      if ShowErrors then
      MessageBox(0, 'Target does not exist.', 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
      Exit;
    end;

    // Create Symbolic Link in Vista and Win7
    if not false and (Win32MajorVersion >= 6) then
    begin
      h:= GetModuleHandle('kernel32.dll');
      if h <> 0 then
      begin
        @proc:= GetProcAddress(h, 'CreateSymbolicLinkW');
        if assigned(Proc) then
        begin
          Result:= proc(PWideChar(ALink), PWideChar(ATarget), flag);
          if not Result and ShowErrors then // Show error dialog if needed
          begin
            s:= SysErrorMessage(GetLastError);
            MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
          end;
        end;
      end;
    end
    // Create Symbolic Link in 2000 and XP
    else if (Win32MajorVersion > 4) then
    begin
      // Create Link folder
      if not CreateDirectoryW(PWideChar(ALink), nil) then
      begin
        if ShowErrors then
        begin
          s:= SysErrorMessage(GetLastError);
          MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
        end;
        Exit;
      end;

      // Open Link folder
      h:= CreateFileW(PWideChar(ALink),
                      GENERIC_WRITE,
                      0,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS,
                      0);
      if h = INVALID_HANDLE_VALUE then
      begin
        if ShowErrors then
        begin
          s:= SysErrorMessage(GetLastError);
          MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
        end;
        RemoveDirectoryW(PWideChar(ALink));
        Exit;
      end
      else
      begin
        // Create mount point for Link folder
        TargetName:= '\??\' + ATarget;
        BufSize:= (Length(ATarget)+5) * 2 + REPARSE_MOUNTPOINT_HEADER_SIZE+12;
        GetMem(Buffer, BufSize);
        FillChar(Buffer^, BufSize, 0);

        Move(TargetName[1], Buffer^.ReparseTarget, (Length(TargetName)+1) * 2);
        Buffer^.ReparseTag:= IO_REPARSE_TAG_MOUNT_POINT;
        Buffer^.ReparseTargetLength:= Length(TargetName)*2;
        Buffer^.ReparseTargetMaximumLength:= Buffer^.ReparseTargetLength+2;
        Buffer^.ReparseDataLength:= Buffer^.ReparseTargetLength+12;

        BytesRead:= 0;
        Result:= DeviceIoControl(h,
                                 FSCTL_SET_REPARSE_POINT,
                                 Buffer,
                                 Buffer^.ReparseDataLength+REPARSE_MOUNTPOINT_HEADER_SIZE,
                                 nil,
                                 0,
                                 BytesRead,
                                 nil);
        FreeMem(Buffer);
        CloseHandle(h);

        if not Result then
        begin
          RemoveDirectoryW(PWideChar(ALink));
          if ShowErrors then
          begin
            s:= SysErrorMessage(GetLastError);
            MessageBox(0, PChar(s), 'Could not create symbolic link!', MB_ICONEXCLAMATION or MB_OK);
          end;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Check if File Or Folder Exists
-------------------------------------------------------------------------------}
function FileOrFolderExists(APath: WideString): Boolean;
var
  sr: TSearchRecW;
begin
  APath:= WideExcludeTrailingPathDelimiter(APath);
  Result:= WideFindFirst(APath, faAnyFile or faDirectory, sr) = 0;
  if Result then
  WideFindClose(sr);
end;

end.
