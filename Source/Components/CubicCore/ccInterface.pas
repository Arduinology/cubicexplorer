//******************************************************************************
//  CubicCore
//  Version: 1.00
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
//  The Original Code is ccInterface.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccInterface;

{$INCLUDE Core.inc}

{==============================================================================}
interface

uses
  // CubicCore
  ccThreads,
  // System Units
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Classes;

{==============================================================================}
const
  IID_ICCInterface: TGUID          = '{72643020-62BE-4FD4-B973-E59AFEA2545B}';
  IID_ICCThreadedInterface: TGUID  = '{CE233F45-8A8C-4AE5-AAF3-807079E076E2}';

type
{-------------------------------------------------------------------------------
  ICCInterface
-------------------------------------------------------------------------------}
  ICCInterface = interface(IInterface)
  ['{72643020-62BE-4FD4-B973-E59AFEA2545B}']
    // Finalize
    // - Finalize the instance.
    // - After this call, all methods should be dummies and the object is just
    //   waiting to be freed from memory.
    procedure Finalize; stdcall;

    // IsAlive
    // - Returns FALSE after Finalize is called, else TRUE.
    function IsAlive: Boolean; stdcall;
  end;



{-------------------------------------------------------------------------------
  ICCThreadedInterface
-------------------------------------------------------------------------------}
  ICCThreadedInterface = interface(ICCInterface)
  ['{CE233F45-8A8C-4AE5-AAF3-807079E076E2}']
    // Lock the instance for single thread.
    procedure Lock; stdcall;

    // Unlock the instance.
    procedure UnLock; stdcall;
  end;

{-------------------------------------------------------------------------------
  TCCInterfacedObject
-------------------------------------------------------------------------------}
  TCCInterfacedObject = class(TInterfacedObject, ICCInterface)
  protected
    fIsAlive: Boolean;
  public
    constructor Create; virtual;
    procedure Finalize; virtual; stdcall;
    function IsAlive: Boolean; virtual; stdcall;
  end;

{-------------------------------------------------------------------------------
  TCCThInterfacedObject
-------------------------------------------------------------------------------}
  TCCThInterfacedObject = class(TCCInterfacedObject, ICCThreadedInterface)
  protected
    fLock: TRTLCriticalSection;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Finalize; override; stdcall;
    function IsAlive: Boolean; override; stdcall;
    procedure Lock; virtual; stdcall;
    procedure UnLock; virtual; stdcall;
  end;

implementation
{##############################################################################}
// TCCInterfacedObject

{-------------------------------------------------------------------------------
  Create an instance of TCCInterfacedObject
-------------------------------------------------------------------------------}
constructor TCCInterfacedObject.Create;
begin
  inherited Create;
  fIsAlive:= true;
end;

{-------------------------------------------------------------------------------
  Finalize
-------------------------------------------------------------------------------}
procedure TCCInterfacedObject.Finalize;
begin
  if not fIsAlive then
  Exit;

  fIsAlive:= false;
end;

{-------------------------------------------------------------------------------
  IsAlive
-------------------------------------------------------------------------------}
function TCCInterfacedObject.IsAlive: Boolean;
begin
  Result:= fIsAlive;
end;

{##############################################################################}
// TCCThreadedIntfObject

{-------------------------------------------------------------------------------
  Create an instance of TCCThreadedIntfObject
-------------------------------------------------------------------------------}
constructor TCCThInterfacedObject.Create;
begin
  inherited Create;
  CreateCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  Destroy TCCThreadedIntfObject
-------------------------------------------------------------------------------}
destructor TCCThInterfacedObject.Destroy;
begin
  DestroyCriticalSection(fLock);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------
  Finalize
-------------------------------------------------------------------------------}
procedure TCCThInterfacedObject.Finalize;
begin
  if not fIsAlive then
  Exit;

  Lock;
  fIsAlive:= false;
  UnLock;
end;

{-------------------------------------------------------------------------------
  IsAlive
-------------------------------------------------------------------------------}
function TCCThInterfacedObject.IsAlive: Boolean;
begin
  Lock;
  Result:= fIsAlive;
  UnLock;
end;

{-------------------------------------------------------------------------------
  Lock
-------------------------------------------------------------------------------}
procedure TCCThInterfacedObject.Lock;
begin
  EnterCriticalSection(fLock);
end;

{-------------------------------------------------------------------------------
  UnLock
-------------------------------------------------------------------------------}
procedure TCCThInterfacedObject.UnLock;
begin
  LeaveCriticalSection(fLock);
end;

end.
