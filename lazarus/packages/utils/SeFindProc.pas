{***************************************************************************}
{* Bugs fixed by Chen Tishler:                                             *}
{*                                                                         *}
{*     01/04/07: Fixed memory leak, there is a problem with icons          *}
{*     01/12/08: Added 'Duplicates' to lists                               *}
{*                                                                         *}
{***************************************************************************}
{  Date: 15.10.98                                            Time: 17:30:40 }
{  Copyright (c)1996-1998                                                   }
{  © by Dr.Plass                                               TFindProc4.0 }
{                                                                           }
{                                              for DELPHI3/4,      Win95/98 }
{  feel free to contact me:                                                 }
{  PP@FH-Zwickau.de                                                         }
{  http://www.fh-zwickau.de/~pp/tm.htm                                      }
{                                                                           }
{  All Rights Reserved.                                                     }
{  This component can be freely used and distributed in commercial and      }
{  private environments, provided this notice is not modified in any way.   }
{                                                                           }
{  --> thanks to Roelof Ridderman for the great help                                                                             }
{                                                                           }
{***************************************************************************}
//
//
//  Methods:
//
//   procedure GetProcessInfo;
//    function GetP_CountUsage(idx : Integer):DWORD;
//    function GetP_ID(idx : Integer): DWORD;
//    function GetP_defHeap(idx : Integer): DWORD;
//    function GetP_ModuleID(idx : Integer): DWORD;
//    function GetP_CountThreads(idx : Integer): DWORD;
//    function GetP_ParentPID(idx : Integer): DWORD;
//    function GetP_IconHandle(Idx : Integer): HIcon;
//    function GetP_Priority(idx : Integer): LongInt;
//    function GetP_dwFlags(idx : Integer): DWORD;
//    function GetP_ExePath(idx : Integer): String;
//    function GetP_PrioStr(idx : Integer):String;
//    function GetP_BinTyp(idx : Integer):String;

//    procedure GetModuleInfo(ProcID : DWORD);
//    function GetM_ModuleID(idx : Integer):DWORD;
//    function GetM_ProcessID(idx : Integer):DWORD;
//    function GetM_GlblcntUsage(idx : Integer):DWORD;
//    function GetM_ProccntUsage(idx : Integer):DWORD;
//    function GetM_modBaseAddr(idx : Integer):DWORD;
//    function GetM_IconHandle(Idx : Integer): HIcon;
//    function GetM_hModule(idx : Integer):HMODULE;
//    function GetM_szExePath(idx : Integer):string;

//  e.g.: whdl := ProcList1.GetP_CountUsage(ListBox1.ItemIndex);
//
//  Properties:
//
//  ModuleList : TStringList ;
//  ProcessList: TStringList ;
//


unit SeFindProc;
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,tlhelp32,shellApi;

const
 cAbout = 'TFindProc4.0: © 1996-1998 by Dr.Peter Plass';

type
 TProcessInfo = class(TObject)
  private
  public
   cntUsage,
   ProcessID,
   DefaultHeapID,
   ModuleID,
   cntThreads,
   ParentProcessID: DWORD;
   PriClassBase   : LongInt;
   dwFlags        : DWORD;
   ExePath        : String;
   PriString      : String[12];
   BinType        : String[12];
   hIco           : HIcon;

   // Chen, fixed memory leak, 01/04/07
   constructor Create;
   destructor Destroy; override;

  end;

 TModuleInfo = class(TObject)
  private
  public
   ModuleID,	        // This module
   ProcessID,	        // owning process
   GlblcntUsage,	// Global usage count on the module
   ProccntUsage: DWORD;	// Module usage count in th32ProcessID's context
   modBaseAddr: PByte; 	// Base address of module in th32ProcessID's context
   modBaseSize: DWORD;  // Size in bytes of module starting at modBaseAddr
   hModule    : HMODULE;// The hModule of this module in th32ProcessID's context
   szExePath  : string;
   hIco       : HIcon;

   // Chen, fixed memory leak, 01/04/07
   constructor Create;
   destructor Destroy; override;
   
  end;

 TProcList = class(TComponent)
  private
 //MObj              : TModuleInfo;
 //PObj              : TProcessInfo;
   fProcessList,
   fModuleList       : TStringList;
   fAbout            : String;

  protected
   procedure FuAbout(value :String);
   function GetPrioStr(prio:DWORD):String;
   function GetBTypStr(btyp:DWORD):String;
  public
   Constructor Create(AOwner : TComponent); override;
   Destructor Destroy; override;

   procedure GetProcessInfo;
    function GetP_CountUsage(idx : Integer):DWORD;
    function GetP_ID(idx : Integer): DWORD;
    function GetP_defHeap(idx : Integer): DWORD;
    function GetP_ModuleID(idx : Integer): DWORD;
    function GetP_CountThreads(idx : Integer): DWORD;
    function GetP_ParentPID(idx : Integer): DWORD;
    function GetP_Priority(idx : Integer): LongInt;
    function GetP_dwFlags(idx : Integer): DWORD;
    function GetP_IconHandle(Idx : Integer): HIcon;
    function GetP_PrioStr(idx : Integer): String;
    function GetP_BinTyp(idx : Integer): String;
    function GetP_ExePath(idx : Integer): String;

   procedure GetModuleInfo(ProcID : DWORD);
    function GetM_ModuleID(idx : Integer)    :DWORD;
    function GetM_ProcessID(idx : Integer)   :DWORD;
    function GetM_GlblcntUsage(idx : Integer):DWORD;
    function GetM_ProccntUsage(idx : Integer):DWORD;
    function GetM_modBaseAddr(idx : Integer) :DWORD;
    function GetM_IconHandle(Idx : Integer)  :HIcon;
    function GetM_modBaseSize(idx : Integer) :DWORD;
    function GetM_hModule(idx : Integer):HMODULE;
    function GetM_szExePath(idx : Integer):string;

  published
   Property About      : String read FAbout write FuAbout;
   Property ModuleList : TStringList read fModuleList write fModuleList;
   Property ProcessList: TStringList read fProcessList write fProcessList;
  end;

procedure Register;

implementation

//---------------------------------------------------------------------------
// Chen, fixed memory leak, 01/04/07

constructor TProcessInfo.Create;
begin
  inherited;
  hIco := 0;
end;

destructor TProcessInfo.Destroy;
begin
  if (hIco <> 0) then
     CloseHandle(hIco);

  inherited;
end;

constructor TModuleInfo.Create;
begin
  inherited;
  hIco := 0;
end;

destructor TModuleInfo.Destroy;
begin
  if (hIco <> 0) then
     CloseHandle(hIco);

  inherited;     
end;

//---------------------------------------------------------------------------
Constructor TProcList.Create(AOwner : TComponent);
Begin
 inherited Create(AOwner);
 fModuleList                := TStringList.Create;
 fModuleList.sorted         := True;
 fModuleList.Duplicates     := dupAccept;  // Chen, 01/12/08
 fProcessList               := TStringList.Create;
 fProcessList.sorted        := True;
 fProcessList.Duplicates    := dupAccept;  // Chen, 01/12/08
 fAbout                     := '';
End;

Destructor TProcList.Destroy;
var i:Integer;
Begin

 for i := 0 to fModuleList.Count-1 do
	fModuleList.Objects[i].Free;
 fModuleList.Clear;
 fModuleList.Free;

 for i := 0 to fProcessList.Count-1 do
	fProcessList.Objects[i].Free;
 fProcessList.Clear;
 fProcessList.Free;
// MObj.Free;
// MObj := NIL;
//PObj.Free;
//PObj := NIL;
 inherited Destroy;
End;

procedure TProcList.GetModuleInfo(ProcID : DWORD);

 procedure AddModule(me32:TMODULEENTRY32);
 var
  tw     : Word;
  ts     : array[0..MAX_PATH] of char;
  MObj   : TModuleInfo;

  begin
   StrCopy(ts,me32.szExePath);
   tw               := 0;

   MObj             := TModuleInfo.Create;

   //Chen: Removed because I could not fix the memory leak caused by repeated calls, 01/04/07
   //MObj.hIco := ExtractIcon(hInstance,me32.szExePath,0);
   //if MObj.hIco=0 then
   // MObj.hIco       := ExtractAssociatedIcon(hInstance,ts,tw);
   
   MObj.ModuleId    := me32.th32ModuleID;
   MObj.ProcessID   := me32.th32ProcessID;
   MObj.GlblcntUsage:= me32.GlblcntUsage;
   MObj.ProccntUsage:= me32.ProccntUsage;
   MObj.modBaseAddr := me32.modBaseAddr;
   MObj.modBaseSize := me32.modBaseSize;
   Mobj.hModule     := me32.hModule;
   MObj.szExePath   := String(me32.szExePath);
   fModuleList.AddObject(String(me32.szModule), MObj);
  end;

var
 i:Integer;
 snap   : THandle;
 me32   : TMODULEENTRY32;
begin
 snap := 0;
 for i := 0 to fModuleList.Count-1 do
	fModuleList.Objects[i].Free;
 fModuleList.Clear;
 try
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,ProcID);
  if snap <> 0 then begin
   me32.dwSize        := SizeOf(TMODULEENTRY32);
   if Module32First(snap, me32) then begin
    AddModule(me32);
    while Module32Next(snap, me32) do
     AddModule(me32);
   end;
  end;
 finally
  CloseHandle(snap);
 end;
end;

procedure TProcList.GetProcessInfo;
var
 Fi     : TSHFileInfo;
 tw     : Word;
 ts     : array[0..MAX_PATH] of char;
 PObj   : TProcessInfo;
 procedure AddProcess(pe32:TPROCESSENTRY32);
  begin
    tw                  := 0;
    StrCopy(ts,pe32.szExeFile);
    PObj                := TProcessInfo.Create;

    //Chen: Removed because I could not fix the memory leak caused by repeated calls, 01/04/07
    //PObj.hIco           := ExtractIcon(hInstance,pe32.szExeFile,0);
    //if PObj.hIco=0 then
    // PObj.hIco          := ExtractAssociatedIcon(hInstance,ts,tw);

    PObj.cntUsage       := pe32.cntUsage;
    PObj.ProcessID      := pe32.th32ProcessID;
    PObj.DefaultHeapID  := pe32.th32DefaultHeapID;
    PObj.ModuleID       := pe32.th32ModuleID;
    PObj.cntThreads     := pe32.cntThreads;
    PObj.ParentProcessID:= pe32.th32ParentProcessID;
    PObj.PriClassBase   := pe32.pcPriClassBase;
    Pobj.dwFlags        := pe32.dwFlags;
    PObj.ExePath        := String(pe32.szExeFile);
    PObj.PriString      := GetPrioStr(pe32.pcPriClassBase);
    PObj.BinType        := GetBTypStr(SHGetFileInfo(pe32.szExeFile,0,FI,SizeOf(FI),SHGFI_EXETYPE));
    fProcessList.AddObject(ExtractFileName(pe32.szExeFile),PObj);
  end;
var
 i:Integer;
 snap   : THandle;
 pe32   : TPROCESSENTRY32;
begin
 snap := 0;
 for i := 0 to fProcessList.Count-1 do
	fProcessList.Objects[i].Free;
 fProcessList.Clear;
 try
  snap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if snap <> 0 then begin
   pe32.dwSize := SizeOf(TPROCESSENTRY32);
   if Process32First(snap, pe32) then begin
    AddProcess(pe32);
    while Process32Next(snap, pe32) do
     AddProcess(pe32);
   end;
  end;
 finally
  CloseHandle(snap);
 end;
End;

function TProcList.GetPrioStr(prio:DWORD):String;
begin
 Result := '';
 case prio of
  4 : Result := '[Idle    ]';
  8 : Result := '[Normal  ]';
  13: Result := '[High    ]';
  24: Result := '[RealTime]';
 else
  Result := '[Unknown]';
 end;
end;

function TProcList.GetBTypStr(btyp:DWORD):String;
const
 IMAGE_DOS_SIGNATURE    = $5A4D; // MZ
 IMAGE_OS2_SIGNATURE    = $454E; // NE
 IMAGE_VXD_SIGNATURE    = $454C; // LE
 IMAGE_NT_SIGNATURE     = $0000; // PE00
begin
 if btyp = 0 then Result:=''
 else
  case LoWord(btyp) of
   IMAGE_DOS_SIGNATURE:    Result:='MS-Dos';
   IMAGE_VXD_SIGNATURE:    Result:='Vxd';
   IMAGE_OS2_SIGNATURE,
   17744,
   IMAGE_NT_SIGNATURE:
    begin
     case HiWord(btyp) of
      1024: Result:='32-Bit';
      768,
      778:  Result:='16-Bit';
     else
      Result:='hi:'+IntTOStr(hiword(btyp));
     end;
    end;
    else
     Result:='lo:'+IntToStr(LoWord(btyp));
  end;
end;

procedure TProcList.FuAbout(value:String);
begin
 if value <> fAbout then begin
  fAbout := cAbout;
  MessageDlg('Komponente: TFindProc4.0            '+ #10#13+ #10#13+
             '© 1996-1998 by Dr.Peter Plass       '+ #10#13+
             'PP@fh-zwickau.de                    '+ #10#13+
             'http://www.fh-zwickau.de/~pp/tm.htm ',
              mtInformation, [mbOk],0);
  end;
end;

function TProcList.GetP_CountUsage(idx : Integer):DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).cntUsage;
end;
function TProcList.GetP_ID(idx : Integer): DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).ProcessID;
end;
function TProcList.GetP_defHeap(idx : Integer): DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).DefaultHeapID;
end;
function TProcList.GetP_ModuleID(idx : Integer): DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).ModuleID;
end;
function TProcList.GetP_CountThreads(idx : Integer): DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).cntThreads;
end;
function TProcList.GetP_ParentPID(idx : Integer): DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).ParentProcessID;
end;
function TProcList.GetP_Priority(idx : Integer): LongInt;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).PriClassBase;
end;
function TProcList.GetP_dwFlags(idx : Integer): DWORD;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).dwFlags;
end;
function TProcList.GetP_ExePath(idx : Integer): String;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).ExePath;
end;
function TProcList.GetP_PrioStr(idx : Integer):String;
begin
 Result:= TProcessInfo(fProcessList.Objects[idx]).PriString
end;
function TProcList.GetP_BinTyp(idx : Integer):String;
begin
 Result:=TProcessInfo(fProcessList.Objects[idx]).BinType;
end;
function TProcList.GetP_IconHandle(idx : Integer): HIcon;
begin
 Result := TProcessInfo(fProcessList.Objects[idx]).hIco;
end;
function TProcList.GetM_ModuleID(idx : Integer):DWORD;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).ModuleID;
end;
function TProcList.GetM_ProcessID(idx : Integer):DWORD;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).ProcessID;
end;
function TProcList.GetM_GlblcntUsage(idx : Integer):DWORD;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).GlblcntUsage;
end;
function TProcList.GetM_ProccntUsage(idx : Integer):DWORD;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).ProccntUsage;
end;
function TProcList.GetM_modBaseAddr(idx : Integer):DWORD;
begin
 Result :=DWORD(TModuleInfo(fModuleList.Objects[idx]).modBaseAddr);
end;
function TProcList.GetM_modBaseSize(idx : Integer):DWORD;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).modBaseSize;
end;
function TProcList.GetM_hModule(idx : Integer):HMODULE;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).hModule;
end;
function TProcList.GetM_szExePath(idx : Integer):string;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).szExePath;
end;
function TProcList.GetM_IconHandle(idx : Integer):HIcon;
begin
 Result := TModuleInfo(fModuleList.Objects[idx]).hIco;
end;

procedure Register;
begin
 RegisterComponents('SESAM', [TProcList]);
end;
end.


