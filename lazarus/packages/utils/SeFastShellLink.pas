{*************************************************************}
{*            CHANGED BY CHEN TISHLER 24/05/01               *}
{*                                                           *}
{*************************************************************}
{            FastShellLink component for Delphi 3 and higher  }
{ Version:   1.0                                              }
{ E-Mail:    info@utilmind.com                                }
{ WWW:       http://www.utilmind.com                          }
{ Created:   March 7, 2000                                    }
{ Modified:  March 7, 2000                                    }
{ Legal:     Copyright (c) 2000, UtilMind Solutions           }
{*************************************************************}
{ This component make installation of the shell links to your }
{ programs and files fast and easily. FastShellLink alow      }
{ automatic installation of the shell links to Desktop,       }
{ System Menu, Programs Menu, Startup Menu and My Documents   }
{ shell folders.                                              }
{*************************************************************}
{ PROPERTIES:                                                 }
{   LinkName - Name of the link (e.g. "MyProgramName")        }
{   LinkTarget - location of your program/file.               }
{   ParamString - Arguments for starting the program.         }
{   RunAs - specifies how your program will shown at starting }
{           (Normal window, Minimized or Maximized            }
{   WorkingDirectory - specifies a working directory of your  }
{                      program.                               }
{                                                             }
{   CREATEIN.                                                 }
{      CreatePath - if True, FastShellLink will automatically }
{                   create path for your shell link even if it}
{                   does not exist.                           }
{      ShellFolder - specifies where installed shell link will}
{                    appeared - on Desktop, System menu etc.  }
{      Subfolder - optional subfolder for shell link.         }
{                 For example, if Subfolder is 'MyProgramName'}
{                 and ShellFolder is ciProgramsMenu then      }
{                 will created submenu "Programs/MyProgramName"}
{                 and shell link will installed there.        }
{*************************************************************}
{                     IMPORTANT NOTE:                         }
{ This software is provided 'as-is', without any express or   }
{ implied warranty. In no event will the author be held       }
{ liable for any damages arising from the use of this         }
{ software.                                                   }
{ Permission is granted to anyone to use this software for    }
{ any purpose, including commercial applications, and to      }
{ alter it and redistribute it freely, subject to the         }
{ following restrictions:                                     }
{ 1. The origin of this software must not be misrepresented,  }
{    you must not claim that you wrote the original software. }
{    If you use this software in a product, an acknowledgment }
{    in the product documentation would be appreciated but is }
{    not required.                                            }
{ 2. Altered source versions must be plainly marked as such,  }
{    and must not be misrepresented as being the original     }
{    software.                                                }
{ 3. This notice may not be removed or altered from any       }
{    source distribution.                                     }
{*************************************************************}

unit SeFastShellLink;

interface

uses
  Windows, SysUtils, Classes, Controls, Registry,
  ActiveX, ShlObj, ComObj, Menus;

type
  TFastShellRoot = (srCurrentUser, srLocalMachine);
  TFastShellFolder = (sfDesktop, sfStartMenu, sfProgramsMenu, sfStartup, sfMyDocuments);

  TCreateIn = class(TPersistent)
  private
    FCreatePath: Boolean;
    FShellRoot: TFastShellRoot;
    FShellFolder: TFastShellFolder;
    FSubfolder: String;

    procedure SetSubfolder(Value: String);
  public
  published
    property CreatePath: Boolean read FCreatePath write FCreatePath;
    property ShellRoot: TFastShellRoot read FShellRoot write FShellRoot;
    property ShellFolder: TFastShellFolder read FShellFolder write FShellFolder;
    property Subfolder: String read FSubfolder write SetSubfolder;
  end;

  TRunAs = (raNormal, raMinimized, raMaximized);

  TFastShellLink = class(TComponent)
  private
    FCreateIn: TCreateIn;
    FLinkName: String;
    FLinkTarget: String;
    FParamString: String;
    FWorkingDirectory: String;
    FRunAs: TRunAs;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute;
  published
    property CreateIn: TCreateIn read FCreateIn write FCreateIn;
    property LinkName: String read FLinkName write FLinkName;
    property LinkTarget: String read FLinkTarget write FLinkTarget;
    property ParamString: String read FParamString write FParamString;
    property WorkingDirectory: String read FWorkingDirectory write FWorkingDirectory;
    property RunAs: TRunAs read FRunAs write FRunAs;
  end;

procedure Register;

implementation

procedure TCreateIn.SetSubfolder(Value: String);
begin
  if FSubfolder <> Value then
   begin
    if Value = '\' then Value := '';
    if Value <> '' then
     if Value[Length(Value)] <> '\' then Value := Value + '\';
    FSubfolder := Value;
   end;
end;

constructor TFastShellLink.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FCreateIn := TCreateIn.Create;
  FCreateIn.CreatePath := True;
  FLinkName := 'My New ShellLink';
end;

destructor TFastShellLink.Destroy;
begin
  FCreateIn.Free;

  inherited Destroy;
end;

procedure TFastShellLink.Execute;
var
  ShellObject: IUnknown;
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  FileName: WideString;

  Reg: TRegistry;
  RegStr: String;
  i: Integer;
begin
  // Geting folders location from Registry first
  Reg := TRegistry.Create;
  with Reg do
   try
     if (FCreateIn.FShellRoot = srLocalMachine) then
         RootKey := HKEY_LOCAL_MACHINE
     else RootKey := HKEY_CURRENT_USER;

     OpenKey('\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', True);
     case FCreateIn.FShellFolder of
       sfDesktop: RegStr := 'Desktop';
       sfStartMenu: RegStr := 'Start Menu';
       sfProgramsMenu: RegStr := 'Programs';
       sfStartup: RegStr := 'Startup';
       sfMyDocuments: RegStr := 'Personal';
      end;

     if (FCreateIn.FShellRoot = srLocalMachine) then
        RegStr := 'Common ' + RegStr;
              
     RegStr := ReadString(RegStr) + FCreateIn.FSubfolder;
   except
   end;
  Reg.Free;

  // and creating the path for ShellLink (if needed)
  if FCreateIn.FCreatePath then
   for i := 1 to Length(RegStr) do
    if RegStr[i] = '\' then
     CreateDir(Copy(RegStr, 1, i));

  // then creating Shell link
  ShellObject := CreateComObject(CLSID_ShellLink);
  ShellLink := ShellObject as IShellLink;
  PersistFile := ShellObject as IPersistFile;

  with ShellLink do
   begin
    SetArguments(PChar(FParamString));
    SetPath(PChar(FLinkTarget));
    SetWorkingDirectory(PChar(WorkingDirectory));
    case FRunAs of
      raMaximized: i := sw_ShowMaximized;
      raMinimized: i := sw_ShowMinNoActive;
      else i := sw_ShowNormal;
    end;
   end;

  // and finally saving the link
  FileName := RegStr + '\' + FLinkName + '.lnk';
  PersistFile.Save(PWChar(FileName), False);
end;

procedure Register;
begin
  RegisterComponents('SESAM', [TFastShellLink]);
end;

end.
