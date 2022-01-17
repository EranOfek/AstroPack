unit gui_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, ComboEx, Process;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnCloseAll: TBitBtn;
    BtnCloseAll1: TBitBtn;
    BtnLiveLeft: TBitBtn;
    BtnLiveLeft1: TBitBtn;
    BtnLiveRight: TBitBtn;
    BtnLiveRight1: TBitBtn;
    BtnRecordLeft: TBitBtn;
    BtnRecordLeft1: TBitBtn;
    BtnRecordRight: TBitBtn;
    BtnRecordRight1: TBitBtn;
    BtnRunMain: TBitBtn;
    BtnRunMain1: TBitBtn;
    BtnSdCard: TBitBtn;
    BtnSdCard1: TBitBtn;
    BtnStartup: TBitBtn;
    BtnSendYml: TBitBtn;
    ComboBoxXmlFileName: TComboBox;
    ComboBoxYmlFileName: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MemoLog: TMemo;
    MemoXml: TMemo;
    MemoYml: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelLog: TPanel;
    PanelLog1: TPanel;
    PanelLog2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    TabSheet1: TTabSheet;
    TabSheetGcsInterface: TTabSheet;
    TabSheetSimulator: TTabSheet;
    TimerPollGuiMsg: TTimer;
    procedure BtnCloseAll1Click(Sender: TObject);
    procedure BtnLiveLeft1Click(Sender: TObject);
    procedure ComboBoxXmlFileNameChange(Sender: TObject);
    procedure ComboBoxYmlFileNameChange(Sender: TObject);
    procedure TimerPollGuiMsgTimer(Sender: TObject);
  private

  public

    procedure SendGuiMsg(AText: String);
    procedure ProcessGuiFile(FileName: String);
  end;

var
  Form1: TForm1;

const
  ScriptPath = '/home/user/dev/opsci.git/src/scripts/';


implementation

{$R *.lfm}

{ TForm1 }

procedure RunScript(Script: AnsiString);
var
  AProcess: TProcess;
  Cmd, s: ansistring;
begin
  Script := ScriptPath + '/' + Script;
  Cmd := Script + ';sleep 2';
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/usr/bin/xterm';  //Cmd;  //'/bin/bash';
  AProcess.Parameters.Add('-e');
  AProcess.Parameters.Add(Cmd);
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  //AProcess.Free;

  //if RunCommand('/bin/bash',['-c',Script], s, [{poUsePipes}{, poWaitOnExit}]) then
  //   writeln(s);


end;


procedure LoadFilesList(Path: String;  Mask: String;  List: TStrings);
var
  ListOfFiles: TStringList;
begin
  ListOfFiles := TStringList.Create;
  try
    FileUtil.FindAllFiles(ListOfFiles, Path, Mask, False);
    List.Assign(ListOfFiles);
  finally
    ListOfFiles.Free;
  end;
end;

const
  LineBreak: String = Char(#10);


function NewFileName: String;
var
  Path: String;
  FileName: String;
begin
  Path := 'c:\gcs\gui\';
  FileName := Path + FormatDateTime('YYYY_MM_DD__HH_MM_SS_zzz', Now);
  Result := FileName;
end;

procedure TForm1.SendGuiMsg(AText: String);
var
   FileName: String;
   Lines: TStringList;
begin
  FileName := NewFileName();

  MemoXml.Lines.SaveToFile(FileName + '.xml');
  MemoYml.Lines.SaveToFile(FileName + '.yml');

  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    Lines.SaveToFile(FileName + '.msg.yml');
  finally
    Lines.Free;
  end;
end;


procedure TForm1.BtnLiveLeft1Click(Sender: TObject);
var
  Path: String;
begin
  Path := 'D:\Ultrasat\AstroPack.git\python\gcs\gcs_msg_xml';
  LoadFilesList(Path, '*.xml', ComboBoxXmlFileName.Items);
  LoadFilesList(Path, '*.yml', ComboBoxYmlFileName.Items);
end;

procedure TForm1.BtnCloseAll1Click(Sender: TObject);
var
  Yml: String;
begin
  Yml :=
     'Msg:'                                + LineBreak +
     '  Cmd: SendFile'                     + LineBreak +
     LineBreak;
  SendGuiMsg(Yml);
end;

procedure TForm1.ComboBoxXmlFileNameChange(Sender: TObject);
var
  FileName: String;
begin
  FileName := ComboBoxXmlFileName.Items[ComboBoxXmlFileName.ItemIndex];
  MemoXml.Lines.LoadFromFile(FileName);
end;

procedure TForm1.ComboBoxYmlFileNameChange(Sender: TObject);
var
  FileName: String;
begin
  FileName := ComboBoxXmlFileName.Items[ComboBoxYmlFileName.ItemIndex];
  MemoYml.Lines.LoadFromFile(FileName);
end;

//function GetKey(Lines: TStrings:


function ExtractBetween(const Value, A, B: string): string;
var
  aPos, bPos: Integer;
begin
  result := '';
  aPos := Pos(A, Value);
  if aPos > 0 then begin
    aPos := aPos + Length(A);
    bPos := Pos(B, Value, aPos);
    if bPos > 0 then begin
      Result := Trim(Copy(Value, aPos, bPos - aPos));
    end;
  end;
end;



procedure TForm1.ProcessGuiFile(FileName: String);
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);


  finally
    Lines.Free;
  end;
end;



procedure TForm1.TimerPollGuiMsgTimer(Sender: TObject);
var
  Path: String;
  FileName: String;
  FileList: TStringList;
begin
  TimerPollGuiMsg.Enabled := false;

  Path := 'c:\gcs\gui_in';
  FileList := TStringList.Create;
  try
    LoadFilesList(Path, '*.yml', FileList);
    FileList.Sort;
    if FileList.Count > 0 then
    begin
        FileName := FileList.Strings[0];
        ProcessGuiFile(FileName);
    end;

  finally
    FileList.Free;
  end;

  TimerPollGuiMsg.Enabled := true;
end;

end.

