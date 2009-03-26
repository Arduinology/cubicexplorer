//******************************************************************************
//  CubicExplorer                                                                             
//  Version: 0.90                                                                             
//                                                                                            
//  The contents of this file are subject to the Mozilla Public License                       
//  Version 1.1 (the "License"); you may not use this file except in                          
//  compliance with the License. You may obtain a copy of the License at                      
//  http://www.mozilla.org/MPL/                                                               
//                                                                                            
//  Software distributed under the License is distributed on an "AS IS"
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//  License for the specific language governing rights and limitations                        
//  under the License.                                                                        
//                                                                                            
//  The Original Code is CE_FindFile.pas.                                                            
//                                                                                            
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).  
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved. 
//                                                                                            
//******************************************************************************

unit CE_FindFile;

interface

uses
  // CE Units

  // Tnt Units
  TntWindows, TntSysUtils, TntClasses,
  // SynEdit
  SynEditSearch,
  // System Units
  Classes, SysUtils, Windows, Forms;
  
type
  TSearchThread = class;

  TFileCriteriaSwitch = (fcsNameText);
  TFileCriteriaSwitchs = set of TFileCriteriaSwitch;

  TFileCriteria = class(TObject)
  protected
    SynSearch: TSynEditSearch;
    procedure InitCriteria;
  public
    CriteriaSwithes: TFileCriteriaSwitchs;
    DOSMask: WideString;
    Attributes: Integer;
    ContainsText: WideString;
    constructor Create;
    destructor Destroy; override;
    function DoesFileMatch(Directory: WideString; SearchRec: TSearchRecW): Boolean;
  end;

  TOnSearchBegin = procedure(Sender: TObject) of object;
  TOnSearchFinished = procedure(Sender: TObject) of object;
  TOnDirectoryChange = procedure(Sender: TObject; NewDirectory: WideString) of object;
  TOnFileMatch = procedure(Sender: TObject; Directory: WideString; FileName: WideString) of object;

  TCEFindFile = class(TObject)
  private
    fDirectories: TTntStringList;
    fOnDirectoryChange: TOnDirectoryChange;
    fOnFileMatch: TOnFileMatch;
    fOnSearchBegin: TOnSearchBegin;
    fOnSearchFinished: TOnSearchFinished;
    fResultCount: Integer;
    fSearchTime: Cardinal;
    fSubDirectories: Boolean;
    fTerminated: Boolean;
    fTempTime: Cardinal;
    SearchThread: TThread;
    procedure ThreadTerminated(Sender: TObject);
  protected
    procedure DoDirectoryChange(NewDirectory: WideString); virtual;
    procedure DoFileMatch(Directory, FileName: WideString); virtual;
    procedure DoSearchBegin; virtual;
    procedure DoSearchFinished; virtual;
    procedure ExecuteSearchW;
  public
    FileCriteria: TFileCriteria;
    constructor Create;
    destructor Destroy; override;
    procedure Abort(WaitToFinish: Boolean = false);
    function IsPaused: Boolean;
    procedure Pause;
    procedure Start;
    property Directories: TTntStringList read fDirectories;
    property ResultCount: Integer read fResultCount;
    property SearchTime: Cardinal read fSearchTime;
    property SubDirectories: Boolean read fSubDirectories write fSubDirectories;
  published
    property OnDirectoryChange: TOnDirectoryChange read fOnDirectoryChange write
        fOnDirectoryChange;
    property OnFileMatch: TOnFileMatch read fOnFileMatch write fOnFileMatch;
    property OnSearchBegin: TOnSearchBegin read fOnSearchBegin write fOnSearchBegin;
    property OnSearchFinished: TOnSearchFinished read fOnSearchFinished write
        fOnSearchFinished;
  end;

  TSearchThread = class(TThread)
  private
    Owner: TCEFindFile;
  protected
    constructor Create(AOwner: TCEFindFile);
    procedure Execute; override;
  end;

implementation

{*------------------------------------------------------------------------------
  Create instance of TCEFindFile
-------------------------------------------------------------------------------}
constructor TCEFindFile.Create;
begin
  inherited;
  FileCriteria:= TFileCriteria.Create;
  fDirectories:= TTntStringList.Create;
  fDirectories.Delimiter:= ',';
  fDirectories.StrictDelimiter:= true;
end;

{*------------------------------------------------------------------------------
  Destroy instance of TCEFindFile
-------------------------------------------------------------------------------}
destructor TCEFindFile.Destroy;
begin
  Abort(true);
  if SearchThread <> nil then
  SearchThread.Terminate;
  FileCriteria.Free;
  fDirectories.Free;
  inherited;
end;

{*------------------------------------------------------------------------------
  Call directory changed event.
-------------------------------------------------------------------------------}
procedure TCEFindFile.DoDirectoryChange(NewDirectory: WideString);
begin
  if Assigned(fOnDirectoryChange) then fOnDirectoryChange(Self, NewDirectory);
end;

{*------------------------------------------------------------------------------
  Call file match event.
-------------------------------------------------------------------------------}
procedure TCEFindFile.DoFileMatch(Directory, FileName: WideString);
begin
  if Assigned(fOnFileMatch) then fOnFileMatch(Self, Directory, FileName);
end;

{*------------------------------------------------------------------------------
  Call search begin event.
-------------------------------------------------------------------------------}
procedure TCEFindFile.DoSearchBegin;
begin
  if Assigned(fOnSearchBegin) then fOnSearchBegin(Self);
end;

{*------------------------------------------------------------------------------
  Call search finished event.
-------------------------------------------------------------------------------}
procedure TCEFindFile.DoSearchFinished;
begin
  if Assigned(fOnSearchFinished) then fOnSearchFinished(Self);
end;

{*------------------------------------------------------------------------------
  Start search.
-------------------------------------------------------------------------------}
procedure TCEFindFile.Start;
begin
  if SearchThread <> nil then
  begin
    if SearchThread.Suspended then
    begin
      fTempTime:= GetTickCount;
      SearchThread.Resume;
    end;
  end
  else
  begin
    fTerminated:= false;
    fResultCount:= 0;
    FileCriteria.InitCriteria;
    DoSearchBegin;
    fSearchTime:= 0;
    fTempTime:= GetTickCount;
    SearchThread:= TSearchThread.Create(Self);
  end;
end;

{*------------------------------------------------------------------------------
  Pause search.
-------------------------------------------------------------------------------}
procedure TCEFindFile.Pause;
begin
  if SearchThread <> nil then
  begin
    if not SearchThread.Suspended then
    begin
      SearchThread.Suspend;
      fTempTime:= GetTickCount - fTempTime;
      Inc(fSearchTime, fTempTime);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Abort search
-------------------------------------------------------------------------------}
procedure TCEFindFile.Abort(WaitToFinish: Boolean = false);
begin
  fTerminated:= true;
  if assigned(SearchThread) then
  begin
    if SearchThread.Suspended then
    begin
      fTempTime:= GetTickCount;
      SearchThread.Resume;
    end;
  end;

  if WaitToFinish then
  begin
    while assigned(SearchThread) do
    begin
      Application.ProcessMessages;
      sleep(20);
    end;
  end;
end;

{*------------------------------------------------------------------------------
  Execute search. The main search loop (Unicode).
-------------------------------------------------------------------------------}
procedure TCEFindFile.ExecuteSearchW;

  procedure EnumFolder(aFolder: WideString);
  var
    sr: TSearchRecW;
  begin
    if fTerminated then
    Exit;

    DoDirectoryChange(aFolder);
    // Enum Files
    try
      if WideFindFirst(aFolder + FileCriteria.DOSMask, FileCriteria.Attributes, sr) = 0 then
      begin
        repeat
          if (sr.Attr and faDirectory) <> faDirectory then
          begin
            if FileCriteria.DoesFileMatch(aFolder, sr) then
            begin
              inc(fResultCount,1);
              DoFileMatch(aFolder,sr.Name);
            end;
          end;
        until (WideFindNext(sr) <> 0) or fTerminated;
      end;
    finally
      WideFindClose(sr);
    end;
    // Enum Folders
    try
      if WideFindFirst(aFolder + '*', faDirectory, sr) = 0 then
      begin
        repeat
          if (sr.Attr and faDirectory) = faDirectory then
          begin
            if (sr.Name <> '.') and (sr.Name <> '..') then
            begin
              if fSubDirectories then
              EnumFolder(aFolder + sr.Name + '\');
            end;
          end;
        until (WideFindNext(sr) <> 0) or fTerminated;
      end;
    finally
      WideFindClose(sr);
    end;
  end;

var
  rootdir: WideString;
  i: Integer;
begin
  for i:= 0 to fDirectories.Count - 1 do
  begin
    rootdir:= fDirectories.Strings[i];
    if not WideDirectoryExists(rootdir) then
    Continue;
    rootdir:= WideIncludeTrailingPathDelimiter(rootdir);
    EnumFolder(rootdir);
  end;
end;

{*------------------------------------------------------------------------------
  Gets called when search thread is terminated.
-------------------------------------------------------------------------------}
procedure TCEFindFile.ThreadTerminated(Sender: TObject);
begin
  fTerminated:= true;
  SearchThread.FreeOnTerminate:= true;
  SearchThread:= nil;
  fTempTime:= GetTickCount - fTempTime;
  Inc(fSearchTime, fTempTime);
  DoSearchFinished;
end;


function TCEFindFile.IsPaused: Boolean;
begin
  if SearchThread <> nil then
  Result:= SearchThread.Suspended
  else
  Result:= false;
end;

{##############################################################################}

{*------------------------------------------------------------------------------
  Create the search thread.
-------------------------------------------------------------------------------}
constructor TSearchThread.Create(AOwner: TCEFindFile);
begin
  inherited Create(True);
  Owner := AOwner;
  Priority := tpNormal;//Owner.ThreadPriority;
  self.FreeOnTerminate:= true;
  OnTerminate := Owner.ThreadTerminated;
  Resume;
end;

{*------------------------------------------------------------------------------
  Execute block of the search thread.
-------------------------------------------------------------------------------}
procedure TSearchThread.Execute;
begin
  try
    Owner.ExecuteSearchW;
  except

  end;
end;






{##############################################################################}

constructor TFileCriteria.Create;
begin
  inherited;
  SynSearch:= TSynEditSearch.Create(nil);
  SynSearch.Whole:= false;
  SynSearch.CaseSensitive:= false;
end;

destructor TFileCriteria.Destroy;
begin
  SynSearch.Free;
  inherited;
end;


{*------------------------------------------------------------------------------
  Initialize criteria search.
-------------------------------------------------------------------------------}
procedure TFileCriteria.InitCriteria;
begin
  if ContainsText = '' then
  Exclude(CriteriaSwithes, fcsNameText)
  else
  begin
    Include(CriteriaSwithes, fcsNameText);
    SynSearch.Pattern:= ContainsText;
  end;
end;

{*------------------------------------------------------------------------------
  Check if the file matches the criteria. Returns true if it matches.
-------------------------------------------------------------------------------}
function TFileCriteria.DoesFileMatch(Directory: WideString; SearchRec:
    TSearchRecW): Boolean;
begin
  Result:= true;
  if fcsNameText in CriteriaSwithes then
  begin
    Result:= SynSearch.FindFirst(SearchRec.Name) > 0;
  end;
end;


end.
