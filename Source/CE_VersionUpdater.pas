unit CE_VersionUpdater;

interface

uses
  // CC,CE Units
  CC_Threads,
  // Synapse
  HTTPSend, blcksock, 
  // fcl-xml
  XMLRead, DOM,
  // System Units
  SysUtils, Classes, Windows, XMLWrite;

type
  TCEVersionNumber = record
    Major: Integer;
    Minor: Integer;
    Release: Integer;
    Build: Integer;
  end;

  TCEDownloadType = (dtUpdateConf, dtPayload);
  TCEDownloadData = class
  protected
    fLastProgress: Integer;
  public
    URL: WideString;
    DownloadType: TCEDownloadType;
    HTTP: THTTPSend;
    IsValidDocument: Boolean;
    FileCount: Integer;
    Current: Integer;
    DestDir: WideString;
    Failed: Integer;
    destructor Destroy; override;
  end;

  TCEDownloadProgressMsg = class
  public
    Percent: Integer;
    FileCount: Integer;
    Current: Integer;
  end;

  TCEDownloadDoneEvent = procedure(Sender: TObject; Data: TCEDownloadData) of object;
  TCEDownloadProgressEvent = procedure(Sender: TObject; Percent: Integer; Current: Integer; FileCount: Integer) of object;

  TCEVersionUpdater = class(TObject)
  private
    fUpdateConfURL: WideString;
    fBackupFolder: WideString;
    fCurrentVersion: TCEVersionNumber;
    fCurrentVersionStr: string;
    fOnDownloadProgress: TCEDownloadProgressEvent;
    fOnDownloadUpdateConfDone: TCEDownloadDoneEvent;
    fOnDownloadVersionDone: TCEDownloadDoneEvent;
    fOutputFolder: WideString;
    fVersionFolder: WideString;
    fXML: TXMLDocument;
    procedure SetCurrentVersionStr(const Value: string);
  protected
    procedure ExecuteDownload(Sender: TCCBaseThread); virtual;
    procedure HandleSockStatus(Sender: TObject; Reason: THookSocketReason; const
        Value: String); virtual;
    procedure HandleSyncedMessage(Sender: TCCBaseThread; Msg: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckForNewerVersion: Boolean; virtual;
    procedure DownloadUpdateConf; virtual;
    function DownloadVersion(Version: TCEVersionNumber): Integer; virtual;
    function FindBuildNode(Version: TCEVersionNumber): TDOMNode; virtual;
    function LoadUpdateConfFromFile(AFilePath: WideString): Boolean; virtual;
    function VersionFolderExists(Version: TCEVersionNumber): Boolean;
    procedure UseVersion(Version: TCEVersionNumber); virtual;
    function ValidateVersion(Node: TDOMNode): Boolean;
    property UpdateConfURL: WideString read fUpdateConfURL write fUpdateConfURL;
    property BackupFolder: WideString read fBackupFolder write fBackupFolder;
    property CurrentVersion: TCEVersionNumber read fCurrentVersion;
    property CurrentVersionStr: string read fCurrentVersionStr write
        SetCurrentVersionStr;
    property VersionFolder: WideString read fVersionFolder write fVersionFolder;
    property XML: TXMLDocument read fXML;
  published
    property OutputFolder: WideString read fOutputFolder write fOutputFolder;
    property OnDownloadProgress: TCEDownloadProgressEvent read fOnDownloadProgress
        write fOnDownloadProgress;
    property OnDownloadUpdateConfDone: TCEDownloadDoneEvent read
        fOnDownloadUpdateConfDone write fOnDownloadUpdateConfDone;
    property OnDownloadVersionDone: TCEDownloadDoneEvent read
        fOnDownloadVersionDone write fOnDownloadVersionDone;
  end;

function CompareVersion(Ver1, Ver2: TCEVersionNumber): Integer;

function StrToVersionNumber(Str: String): TCEversionNumber;

function VersionNumberToStr(Version: TCEVersionNumber): String;

function ExtractUrlFileName(const AUrl: string): string;

procedure UpdateFiles(ADestFolder: WideString; ASrcFolder: WideString );

procedure ClearFolder(AFolder: WideString; ARecursive: Boolean = true);

function NeedElevation(AFolder: WideString = ''): Boolean;

function UpdateCEFromZip(AZipPath: WideString; ADestFolderPath: WideString;
    OldApplicationHandle: Integer = 0; CheckElevationNeed: Boolean = false):
    Boolean;

var
  CELastVersionCheck: TDateTime;

implementation

uses
  CE_XmlUtils, Math, MPCommonUtilities, TntClasses, TntSysUtils, TntSystem,
  TntWindows, fCE_UpdateDlg, Controls, CE_VistaFuncs, Messages, dCE_Input,
  Forms, CE_FileUtils, CE_LanguageEngine;

{-------------------------------------------------------------------------------
  Compare Version (Result < 0 if Ver1 is smaller,
                   Result > 0 if Ver1 is bigger,
                   Result = 0 if equal)
-------------------------------------------------------------------------------}
function CompareVersion(Ver1, Ver2: TCEVersionNumber): Integer;
begin
  Result:= Ver1.Major - Ver2.Major;
  if Result = 0 then
  Result:= Ver1.Minor - Ver2.Minor;
  if Result = 0 then
  Result:= Ver1.Release - Ver2.Release;
  if Result = 0 then
  Result:= Ver1.Build - Ver1.Build;
end;

{-------------------------------------------------------------------------------
  Str To Version Number (Str must be in format: '0.90.1.1234')
-------------------------------------------------------------------------------}
function StrToVersionNumber(Str: String): TCEversionNumber;
const
  VERSION_SEPARATOR = '.';
var
  i, p: Integer;
begin
  // Major
  p:= Pos(VERSION_SEPARATOR, Str);
  i:= StrToInt(Copy(Str,1,p-1));
  Delete(Str,1,p);
  Result.Major:= i;
  // Minor
  p:= Pos(VERSION_SEPARATOR, Str);
  i:= StrToInt(Copy(Str,1,p-1));
  Delete(Str,1,p);
  Result.Minor:= i;
  // Release
  p:= Pos(VERSION_SEPARATOR, Str);
  i:= StrToInt(Copy(Str,1,p-1));
  Result.Release:= i;
  // Build
  i:= StrToInt(Copy(Str,p+1,Length(Str)-p));
  Result.Build:= i;
end;

{-------------------------------------------------------------------------------
  Version Number To Str
-------------------------------------------------------------------------------}
function VersionNumberToStr(Version: TCEVersionNumber): String;
begin
  Result:= IntToStr(Version.Major) + '.' +
           IntToStr(Version.Minor) + '.' +
           IntToStr(Version.Release) + '.' +
           IntToStr(Version.Build);
end;

{-------------------------------------------------------------------------------
  ExtractUrlFileName
-------------------------------------------------------------------------------}
function ExtractUrlFileName(const AUrl: string): string;
var 
  i: Integer;
begin 
  i:= LastDelimiter('/', AUrl);
  Result:= Copy(AUrl, i + 1, Length(AUrl) - (i));
end;

{-------------------------------------------------------------------------------
  Update Files
-------------------------------------------------------------------------------}
procedure UpdateFiles(ADestFolder: WideString; ASrcFolder: WideString );

  procedure EnumFolder(AFolder: WideString; SubFolder: WideString);
  var
    sr: TSearchRecW;
    dir: WideString;
  begin
    if WideFindFirst(AFolder + '*', faDirectory or faAnyFile, sr) = S_OK then
    begin
      try
        repeat
          if (sr.Name <> '.') and (sr.Name <> '..') then
          begin
            // sub folder
            if (sr.Attr and faDirectory) = faDirectory then
            begin
              // create dir if it doesn't exist
              dir:= ADestFolder + SubFolder + sr.Name;
              if not WideDirectoryExists(dir) then
              WideCreateDir(dir);
              // enum sub folder
              EnumFolder(AFolder + sr.Name + '\', SubFolder + sr.Name + '\');
            end
            // file
            else
            begin
              dir:= ADestFolder + SubFolder;
              WideCopyFile(AFolder + sr.Name, dir + sr.Name, false);
            end;
          end;
        until WideFindNext(sr) <> S_OK;
      finally
        WideFindClose(sr);
      end;
    end;
  end;

begin
  if WideDirectoryExists(ADestFolder) and WideDirectoryExists(ASrcFolder) then
  begin
    ADestFolder:= WideIncludeTrailingPathDelimiter(ADestFolder);
    ASrcFolder:= WideIncludeTrailingPathDelimiter(ASrcFolder);
    EnumFolder(ASrcFolder, '');
  end;
end;

{-------------------------------------------------------------------------------
  Clear Folder
-------------------------------------------------------------------------------}
procedure ClearFolder(AFolder: WideString; ARecursive: Boolean = true);
var
  sr: TSearchRecW;
begin
  AFolder:= WideIncludeTrailingPathDelimiter(AFolder);
  if WideFindFirst(AFolder + '*', faDirectory or faAnyFile or faHidden, sr) = S_OK then
  begin
    try
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          if (sr.Attr and faDirectory) = faDirectory then
          begin
            if ARecursive then
            begin
              ClearFolder(AFolder + sr.Name, true);
              WideRemoveDir(AFolder + sr.Name);
            end;
          end
          else
          WideDeleteFile(AFolder + sr.Name);
        end;
      until WideFindNext(sr) <> S_OK;
    finally
      WideFindClose(sr);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Need Elevation test
-------------------------------------------------------------------------------}
function NeedElevation(AFolder: WideString = ''): Boolean;
var
  dir, tmpFile: WideString;
  h: Integer;
begin
  Result:= false;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
  begin
    if AFolder <> '' then
    dir:= WideIncludeTrailingPathDelimiter(AFolder)
    else
    dir:= WideExtractFilePath(WideParamStr(0));
    // Generate random file name
    Randomize;
    tmpFile:= 'tmp' + IntToStr(Random($FFFFFF)) + '.exe';
    while WideFileExists(dir + tmpFile) do
    begin
      tmpFile:= 'tmp' + IntToStr(Random($FFFFFF)) + '.exe';
    end;
    tmpFile:= dir + tmpFile;
    // Try to create a file
    h:= WideFileCreate(tmpFile);
    if GetLastError = ERROR_ACCESS_DENIED then
    Result:= true;
    if h > -1 then
    begin
      FileClose(h);
      // Delete created file
      if WideFileExists(tmpFile) then
      WideDeleteFile(tmpFile);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Extract Zip Tp
-------------------------------------------------------------------------------}
function UpdateCEFromZip(AZipPath: WideString; ADestFolderPath: WideString;
    OldApplicationHandle: Integer = 0; CheckElevationNeed: Boolean = false):
    Boolean;
var
  dlg: TCEUpdateDlg;
begin
  Result:= false;
  if CheckElevationNeed and NeedElevation then
  begin
    AZipPath:= GetRedirectedPath(AZipPath);
    WideShellExecute(0,
                     'runas',
                     WideParamStr(0),
                     '/update ' + '"' + AZipPath + '" "' + ADestFolderPath + '" ' + IntToStr(OldApplicationHandle),
                     ADestFolderPath);
    Exit;
  end;

  dlg:= TCEUpdateDlg.Create(nil);
  try
    try
      dlg.OpenArchive(AZipPath);
      dlg.DestinationDir:= ADestFolderPath;
    except
    end;
    Result:= dlg.ShowModal = mrOk;
  finally
    dlg.Free;

    if Result then
    begin
      if TaskDialog(GetActiveWindow,
                   _('Restart needed!'),
                   _('Restart CubicExplorer Now?'),
                   '',
                   TD_ICON_QUESTION,
                   TD_BUTTON_YES+TD_BUTTON_NO) = mrYes then
      begin
        if OldApplicationHandle <> 0 then
        PostMessage(OldApplicationHandle, WM_USER + 101, 102, 0); // Post restart message
      end;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Create an instance of TCEVersionUpdater
-------------------------------------------------------------------------------}
constructor TCEVersionUpdater.Create;
begin
  inherited;
  fXML:= TXMLDocument.Create;
end;

{-------------------------------------------------------------------------------
  Destroy TCEVersionUpdater
-------------------------------------------------------------------------------}
destructor TCEVersionUpdater.Destroy;
begin
  if assigned(fXML) then
  FreeAndNil(fXML);
  inherited;
end;

{-------------------------------------------------------------------------------
  Check For Newer Version
-------------------------------------------------------------------------------}
function TCEVersionUpdater.CheckForNewerVersion: Boolean;
var
  rootNode, buildNode: TDOMNode;
  ver: TCEVersionNumber;
begin
  Result:= false;
  if assigned(fXML.DocumentElement) then
  begin
    rootNode:= FindFirstChildDOMNode(fXML.DocumentElement, 'Updates');
    if assigned(rootNode) then
    begin
      buildNode:= FindFirstChildDOMNode(rootNode, 'Build');
      while assigned(buildNode) and (buildNode is TDOMElement) do
      begin
        try
          ver:= StrToVersionNumber(TDOMElement(buildNode).AttribStrings['version']);
          if CompareVersion(CurrentVersion, ver) < 0  then
          begin
            Result:= true;
            Exit;
          end;
        except
        end;
        buildNode:= FindNextSiblingDOMNode(buildNode, 'Build');
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Download UpdateConf
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.DownloadUpdateConf;
var
  thread: TCCBaseThread;
  data: TCEDownloadData;
begin
  thread:= TCCBaseThread.Create(true);
  thread.FreeOnTerminate:= true;
  thread.OnExecute:= ExecuteDownload;
  thread.OnSyncedMessage:= HandleSyncedMessage;
  thread.Name:= 'Downloader_UpdateConf';
  // Create data
  data:= TCEDownloadData.Create;
  data.URL:= UpdateConfURL;
  data.DownloadType:= dtUpdateConf;
  data.HTTP:= THTTPSend.Create;
  thread.Data:= data;
  thread.FreeDataOnDestroy:= true;
  // Start
  thread.Resume;
end;

{-------------------------------------------------------------------------------
  Download Version (Returns pointer to the thread)
-------------------------------------------------------------------------------}
function TCEVersionUpdater.DownloadVersion(Version: TCEVersionNumber): Integer;
var
  thread: TCCBaseThread;
  data: TCEDownloadData;
  buildNode, fileNode: TDOMNode;
  list: TStrings;
  dir: WideString;
  rootURL: String;
begin
  Result:= 0;
  list:= TStringList.Create;
  list.Delimiter:= ';';
  try
    buildNode:= FindBuildNode(Version);
    if assigned(buildNode) then
    begin
      // Get root URL
      rootURL:= TDOMElement(buildNode).AttribStrings['root'];
      if Length(rootURL) > 0 then
      begin
        if rootURL[Length(rootURL)] <> '/' then
        rootURL:= rootURL + '/';
      end;

      fileNode:= FindFirstChildDOMNode(buildNode, 'File');
      while assigned(fileNode) do
      begin
        list.Add(rootURL + TDOMElement(fileNode).AttribStrings['src']);
        fileNode:= FindNextSiblingDOMNode(fileNode, 'File');
      end;

      // Check/Create main Versions folder
      dir:= WideIncludeTrailingBackslash(VersionFolder);
      if not WideDirectoryExists(dir) then
        if not WideCreateDir(dir) then
        begin
          MessageBox(0, PChar(String(dir)), 'Could not create folder!', MB_ICONERROR or MB_OK);
        end;

      // Check/Create version folder
      dir:= dir + VersionNumberToStr(Version) + '\';
      if not WideDirectoryExists(dir) then
      begin
        if not WideCreateDir(dir) then
        begin
          MessageBox(0, PChar(String(dir)), 'Could not create folder!', MB_ICONERROR or MB_OK);
        end;
      end
      else
      ClearFolder(dir);

      if WideDirectoryExists(dir) then
      begin
        thread:= TCCBaseThread.Create(true);
        Result:= Integer(thread);
        thread.FreeOnTerminate:= true;
        thread.OnExecute:= ExecuteDownload;
        thread.OnSyncedMessage:= HandleSyncedMessage;
        thread.Name:= 'Downloader_Payload';
        // Create data
        data:= TCEDownloadData.Create;
        data.URL:= list.DelimitedText;
        data.DestDir:= dir;
        data.DownloadType:= dtPayload;
        data.HTTP:= THTTPSend.Create;
        data.HTTP.Sock.OnStatus:= HandleSockStatus;
        data.FileCount:= list.Count;
        data.Current:= 0;
        data.fLastProgress:= 0;
        data.Failed:= 0;
        thread.Data:= data;
        thread.FreeDataOnDestroy:= true;
        // Start
        thread.Resume;
      end;
    end;
  finally
    list.Free;
  end;
end;

{-------------------------------------------------------------------------------
  Execute Download (!!! Runs in separate thread !!!)
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.ExecuteDownload(Sender: TCCBaseThread);
var
  data: TCEDownloadData;
  list: TStrings;
  i: Integer;
  fs: TTntFileStream;
  fileName: String;
begin
  data:= TCEDownloadData(Sender.Data);
  try
    if data.DownloadType = dtUpdateConf then
    data.HTTP.HTTPMethod('GET', data.URL)
    else
    begin
      list:= TStringList.Create;
      list.Delimiter:= ';';
      list.DelimitedText:= data.URL;
      data.HTTP.Sock.Tag:= Integer(Sender);
      try
        for i:= 0 to list.Count - 1 do
        begin
          data.Current:= i + 1;
          data.HTTP.HTTPMethod('GET', list.Strings[i]);
          // Create file
          fileName:= ExtractUrlFileName(list.Strings[i]);
          if (data.HTTP.ResultCode = 200) and (fileName <> '') then
          begin
            try
              fs:= TTntFileStream.Create(data.DestDir + fileName, fmCreate);
              try
                fs.Seek(0, soFromBeginning);
                fs.CopyFrom(data.HTTP.Document, 0);
              finally
                fs.Free;
              end;
            except
              data.Failed:= data.Failed + 1;
            end;
          end
          else
          data.Failed:= data.Failed + 1;
        end;
      finally
        list.Free;
      end;
    end;
  finally
    // Send Synced "Done" message
    Sender.SendSyncedMessage(Sender.Data);
  end;
end;

{-------------------------------------------------------------------------------
  Handle Sock Status  (!!! Runs in separate thread !!!)
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.HandleSockStatus(Sender: TObject; Reason:
    THookSocketReason; const Value: String);
var
  thread: TCCBaseThread;
  msg: TCEDownloadProgressMsg;
  http: THTTPSend;
  data: TCEDownloadData;
begin
  if Reason = HR_ReadCount then
  begin
    thread:= TCCBaseThread(TBlockSocket(Sender).Tag);
    http:= THTTPSend(TBlockSocket(Sender).Owner);
    if assigned(thread) then
    begin
      data:= TCEDownloadData(thread.Data);
      if (GetTickCount - data.fLastProgress) > 50 then // Send Progress event at maximum every 50ms.
      begin
        data.fLastProgress:= GetTickCount;
        msg:= TCEDownloadProgressMsg.Create;
        try
          if http.Document.Size > 0 then
          msg.Percent:= Min(Ceil((100 / http.DownloadSize) * http.Document.Size), 100)
          else
          msg.Percent:= 0;

          msg.FileCount:= data.FileCount;
          msg.Current:= data.Current;

          thread.SendSyncedMessage(msg);
        finally
          msg.Free;
        end;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Handle SyncedMessage
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.HandleSyncedMessage(Sender: TCCBaseThread; Msg:
    TObject);
var
  stream: TStream;
  fs: TTntFileStream;
begin
  if Msg is TCEDownloadData then
  begin
    // UpdateConf download done. Load XML from receive stream.
    if TCEDownloadData(Msg).DownloadType = dtUpdateConf then
    begin
      try
        if TCEDownloadData(Msg).HTTP.ResultCode = 200 then
        begin
          stream:= TCEDownloadData(Msg).HTTP.Document;
          try
            stream.Position:= 0;
            if assigned(fXML) then
            FreeAndNil(fXML);

            ReadXMLFile(fXML, stream);
            // Check if valid xml file
            if assigned(FindFirstChildDOMNode(fXML.DocumentElement, 'Updates')) then
            begin
              TCEDownloadData(Msg).IsValidDocument:= true;
              // save UpdateConf to disk for offline use
              if not WideDirectoryExists(VersionFolder) then
              WideCreateDir(VersionFolder);
              if WideDirectoryExists(VersionFolder) then
              begin
                fs:= TTntFileStream.Create(VersionFolder + 'updates.xml', fmCreate);
                try
                  fs.Seek(0, soFromBeginning);
                  fs.CopyFrom(stream, 0);
                finally
                  fs.Free;
                end;
              end;
            end;
          except on EXMLReadError do
            begin
              if not assigned(fXML) then
              begin
                fXML:= TXMLDocument.Create;
                fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
              end
              else if not assigned(fXML.DocumentElement) then
              begin
                fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
              end;
            end;
          end;
        end;
      finally
        if assigned(fOnDownloadUpdateConfDone) then
        fOnDownloadUpdateConfDone(Self, TCEDownloadData(Msg));
      end;
    end
    // Version download is done.
    else
    begin
      if assigned(fOnDownloadVersionDone) then
        fOnDownloadVersionDone(Self, TCEDownloadData(Msg));
    end;
  end
  else if Msg is TCEDownloadProgressMsg then
  begin
    if assigned(fOnDownloadProgress) then
    fOnDownloadProgress(Sender,
                        TCEDownloadProgressMsg(Msg).Percent,
                        TCEDownloadProgressMsg(Msg).Current,
                        TCEDownloadProgressMsg(Msg).FileCount);
  end;
end;

{-------------------------------------------------------------------------------
  Find Build Node
-------------------------------------------------------------------------------}
function TCEVersionUpdater.FindBuildNode(Version: TCEVersionNumber): TDOMNode;
var
  rootNode: TDOMNode;
  s: String;
begin
  if assigned(Xml.DocumentElement) then
  begin
    rootNode:= FindFirstChildDOMNode(Xml.DocumentElement, 'Updates');
    if assigned(rootNode) then
    begin
      s:= VersionNumberToStr(Version);
      Result:= FindFirstChildDOMNode(rootNode, 'Build');
      while assigned(Result) do
      begin
        if TDOMElement(Result).AttribStrings['version'] = s then
        Exit; // -->
        Result:= FindNextSiblingDOMNode(Result, 'Build');
      end;
    end;
  end;
  Result:= nil;
end;

{-------------------------------------------------------------------------------
  Load UpdateConf From File
-------------------------------------------------------------------------------}
function TCEVersionUpdater.LoadUpdateConfFromFile(AFilePath: WideString):
    Boolean;
var
  fs: TStream;
begin
  Result:= false;
  if WideFileExists(AFilePath) then
  begin
    fs:= TTntFileStream.Create(AFilePath, fmOpenRead);
    try
      fs.Seek(0, soFromBeginning);
      try
        if assigned(fXML) then
        FreeAndNil(fXML);
        
        ReadXMLFile(fXML, fs);
        Result:= true;
      except on EXMLReadError do
        if not assigned(fXML) then
        begin
          fXML:= TXMLDocument.Create;
          fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
        end
        else if not assigned(fXML.DocumentElement) then
        begin
          fXML.AppendChild(fXML.CreateElement('CubicExplorer'));
        end;
      end;
    finally
      fs.Free;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  VersionFolderExists
-------------------------------------------------------------------------------}
function TCEVersionUpdater.VersionFolderExists(Version: TCEVersionNumber):
    Boolean;
begin
  Result:= WideDirectoryExists(WideIncludeTrailingPathDelimiter(VersionFolder) + VersionNumberToStr(Version));
end;

{-------------------------------------------------------------------------------
  Set CurrentVersionStr
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.SetCurrentVersionStr(const Value: string);
begin
  fCurrentVersionStr:= Value;
  fCurrentVersion:= StrToVersionNumber(fCurrentVersionStr);
end;

{-------------------------------------------------------------------------------
  UseVersion
-------------------------------------------------------------------------------}
procedure TCEVersionUpdater.UseVersion(Version: TCEVersionNumber);
var
  buildNode, fileNode: TDOMNode;
  dir: WideString;
  path: WideString;
  op: WideString;
begin
  buildNode:= FindBuildNode(Version);
  if assigned(buildNode) then
  begin
    dir:= WideIncludeTrailingBackslash(VersionFolder) + TDOMElement(buildNode).AttribStrings['version'];
    if WideDirectoryExists(dir) then
    begin
      fileNode:= FindFirstChildDOMNode(buildNode, 'File');
      while assigned(fileNode) do
      begin
        path:= TDOMElement(fileNode).AttribStrings['src'];
        path:= WideStringReplace(path, '/', '\', [rfReplaceAll]);
        path:= WideIncludeTrailingPathDelimiter(dir) + path;
        if WideFileExists(path) then
        begin
          op:= TDOMElement(fileNode).AttribStrings['operation'];
          if op = 'extract' then
          begin
            UpdateCEFromZip(path, OutputFolder, CEInput.MsgInput.Handle, true);
          end;
        end;
        fileNode:= FindNextSiblingDOMNode(fileNode, 'File');
      end;
    end;  
  end;
end;

{-------------------------------------------------------------------------------
  Validate Version
-------------------------------------------------------------------------------}
function TCEVersionUpdater.ValidateVersion(Node: TDOMNode): Boolean;
var
  fileNode: TDOMNode;
  dir: WideString;
  path: WideString;
begin
  Result:= false;
  if assigned(Node) then
  begin
    dir:= WideIncludeTrailingBackslash(VersionFolder) + TDOMElement(Node).AttribStrings['version'];
    if WideDirectoryExists(dir) then
    begin
      fileNode:= FindFirstChildDOMNode(Node, 'File');
      while assigned(fileNode) do
      begin
        path:= TDOMElement(fileNode).AttribStrings['src'];
        path:= WideStringReplace(path, '/', '\', [rfReplaceAll]);
        path:= WideIncludeTrailingPathDelimiter(dir) + path;
        if not WideFileExists(path) then
        Exit;
        fileNode:= FindNextSiblingDOMNode(fileNode, 'File');
      end;
      Result:= true;
    end;
  end;
end;

{##############################################################################}

{-------------------------------------------------------------------------------
  Destroy TCEDownloadData
-------------------------------------------------------------------------------}
destructor TCEDownloadData.Destroy;
begin
  if assigned(HTTP) then HTTP.Free;
  inherited;
end;

end.
