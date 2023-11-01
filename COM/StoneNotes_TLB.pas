unit StoneNotes_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 98336 $
// File generated on 10/31/2023 11:37:13 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: D:\Dev\Projects\Embar\Projects\StoneNotes\COM\StoneNotes (1)
// LIBID: {2EC5FF06-C43E-41C0-ACD9-D0FDE1B6270B}
// LCID: 0
// Helpfile:
// HelpString:
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  StoneNotesMajorVersion = 1;
  StoneNotesMinorVersion = 0;

  LIBID_StoneNotes: TGUID = '{2EC5FF06-C43E-41C0-ACD9-D0FDE1B6270B}';

  IID_IPluginStorageService: TGUID = '{7E303426-C214-460F-97A0-D90553270EE0}';
  CLASS_PluginStorageService: TGUID = '{C2784494-8DCB-4F94-A06A-101CD3BD0434}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IPluginStorageService = interface;
  IPluginStorageServiceDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  PluginStorageService = IPluginStorageService;


// *********************************************************************//
// Interface: IPluginStorageService
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7E303426-C214-460F-97A0-D90553270EE0}
// *********************************************************************//
  IPluginStorageService = interface(IDispatch)
    ['{7E303426-C214-460F-97A0-D90553270EE0}']
    procedure WriteLayoutValue(const AKey: WideString; const AValue: WideString); safecall;
    function ReadLayoutValue(const AKey: WideString): WideString; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IPluginStorageServiceDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7E303426-C214-460F-97A0-D90553270EE0}
// *********************************************************************//
  IPluginStorageServiceDisp = dispinterface
    ['{7E303426-C214-460F-97A0-D90553270EE0}']
    procedure WriteLayoutValue(const AKey: WideString; const AValue: WideString); dispid 201;
    function ReadLayoutValue(const AKey: WideString): WideString; dispid 202;
  end;

// *********************************************************************//
// The Class CoPluginStorageService provides a Create and CreateRemote method to
// create instances of the default interface IPluginStorageService exposed by
// the CoClass PluginStorageService. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoPluginStorageService = class
    class function Create: IPluginStorageService;
    class function CreateRemote(const MachineName: string): IPluginStorageService;
  end;

implementation

uses System.Win.ComObj;

class function CoPluginStorageService.Create: IPluginStorageService;
begin
  Result := CreateComObject(CLASS_PluginStorageService) as IPluginStorageService;
end;

class function CoPluginStorageService.CreateRemote(const MachineName: string): IPluginStorageService;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PluginStorageService) as IPluginStorageService;
end;

end.

