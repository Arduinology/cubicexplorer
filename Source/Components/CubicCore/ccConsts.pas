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
//  The Original Code is ccConsts.pas.
//
//  The Initial Developer of the Original Code is Marko Savolainen (cubicreality@gmail.com).
//  Portions created by Marko Savolainen Copyright (C) Marko Savolainen. All Rights Reserved.
//
//******************************************************************************
unit ccConsts;

interface

const
  // Success codes
  S_OK = $00000000;
  S_FALSE = $00000001;
  NOERROR = $00000000;

  // Catastrophic failure
  E_UNEXPECTED = HRESULT($8000FFFF);

  // Not implemented }
  E_NOTIMPL = HRESULT($80004001);

  // Ran out of memory
  E_OUTOFMEMORY = HRESULT($8007000E);

  // One or more arguments are invalid
  E_INVALIDARG = HRESULT($80070057);

  // No such interface supported
  E_NOINTERFACE = HRESULT($80004002);

  // Invalid pointer
  E_POINTER = HRESULT($80004003);

  // Invalid handle
  E_HANDLE = HRESULT($80070006);

  // Operation aborted
  E_ABORT = HRESULT($80004004);

  // Unspecified error
  E_FAIL = HRESULT($80004005);

  // General access denied error
  E_ACCESSDENIED = HRESULT($80070005);

  // The data necessary to complete this operation is not yet available.
  E_PENDING = HRESULT($8000000A);

  NULL_GUID: TGUID = '{00000000-0000-0000-0000-000000000000}';

implementation

end.
