
unit LogFile;

interface

uses
    Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms;
  
type

  // TLogPanel
  TLogFile = class(TObject)
  private
    FThreadList : TThreadList;         // List of lines
    FMaxLines : Integer;               // Max number of lines
    FMaxLength : Integer;              // Max line length (of lines in List)
    FUpdateInterval : Integer;
    FAddAtEnd : Boolean;               // true to Add add end of list, otherwise top of list
    FAutoScroll : Boolean;
    FPaused : Boolean;
    FDefColor : TColor;                // Default text color
    FDefBack : TColor;                 // Default text background
    FTimeFormat : String;              // DateTime format string
    FTimeMS : Boolean;                 // True to display milliseconds
    FTimeTickCount : Boolean;          // True to display TickCount instead of DateTime
    FTimeDiff : Boolean;               // True to display elapsed time since last log
    FLastTickCount : DWord;            // TickCount of last log message
    FScrollLines : Integer;
    FScrollLength : Integer;
    FPaintBox : TPaintBox;
    FHScrollBar : TScrollBar;
    FVScrollBar : TScrollBar;
    FUpdateTimer : TTimer;



  	AnsiString  	AppName;                //
  	AnsiString      ModuleName;             //
  	AnsiString		ModuleParam1; 			// (27/04/15)
      AnsiString      IniName;                //
      AnsiString  	AppStr;                 //
      AnsiString  	ShortAppStr;            //
      String  	    FileName;               //
      String 		    DefaultLogDir;          //
      String          DefaultCaptureDir;      //
      DWORD           Flags;                  //
      int     	    Pid;                    //
      int     	    MaxFileSize;            //
      int     	    CheckSizeCounter;       //
      bool 		    Lock;                   //
      bool            UseHeader;              //
      bool            UseAppStr;              // (12/04/10)
  	bool            UseTime;                // (12/04/10)
  	TSMCriticalSection*	CritSect;			// (21/07/14)
  	TLogPanel*			LogPanel;			// (21/07/14)



  protected
    procedure ClearList;


    function TimeStr : String;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Constructor, default log file size is 10 MB
    __fastcall TLogger(String FName = "", int MSize = 10000000, DWORD AFlags = 0);
    __fastcall TLogger(TComponent* AOwner, String FName = "", int MSize = 10000000, DWORD AFlags = 0);

    procedure __fastcall Init(String FName = "", int MSize = 10000000, DWORD AFlags = 0);



    procedure Log(String Msg, TColor AColor);

    procedure Log(String Msg, DWORD Flags = 0, const char* szExt = NULL, TColor AColor = clBlack);

    // (21/07/14)
    function DoLog(String AFileName, AnsiString Msg) : Boolean;

    procedure Err(String Msg);

    procedure Logva(const char* fmt, ...);

    function Printva(const char* fmt, ...) : String;

    function TimeStr(DWORD Flags = 0, bool UseFlags = false) : String;

    procedure InitHeader(TLogFileHeader* Header);

    function ReadHeader(HANDLE hFile, TLogFileHeader* Header) : Boolean;

    function WriteHeader(HANDLE hFile, TLogFileHeader* Header) : Boolean;

    DWORD CalcHeaderChecksum(TLogFileHeader* Header);

    function CaptureScreen(String CaptureFileName) : Boolean;

    Graphics::TBitmap* CreateBitmapFromDC(HDC hDC, int TrgWidth, int TrgHeight,
        int SrcX=0, int SrcY=0, int TrgX=0, int TrgY=0);

    procedure Dump(BYTE* Data, int Len, String AText);

    function DumpStr(BYTE* Data, int Len, String AText) : String;

    function DumpStrText(BYTE* Data, int Len, String AText) : ;

    static String GetValidFileName(String AText) : String;

      // Add message to log
      procedure Add(MsgText: String; TextColor: TColor = $7FFFFFFF;
        BackColor: TColor = $7FFFFFFF);
      procedure Clear;


  published
    property MaxLines: Integer read FMaxLines  write FMaxLines;
    property UpdateInterval: Integer read FUpdateInterval write FUpdateInterval;
    property AddAtEnd: Boolean read FAddAtEnd write FAddAtEnd;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property DefColor: TColor read FDefColor write FDefColor;
    property DefBack: TColor read FDefBack write FDefBack;
    property TimeFormat: String read FTimeFormat write FTimeFormat;
    property TimeMS: Boolean read FTimeMS write FTimeMS;
    property TimeTickCount: Boolean read FTimeTickCount write FTimeTickCount;
    property TimeDiff: Boolean read FTimeDiff write FTimeDiff;
    property Paused: Boolean read FPaused write FPaused;
  end;


implementation

// TLogPanelMsg
constructor TLogPanelMsg.Create(APrompt: String; AText: String;
      ATextColor: TColor; ABackColor: TColor);
begin
  Prompt := APrompt;
  Text := AText;
  TextColor := ATextColor;
  BackColor := ABackColor;
end;


// TLogPanel
constructor TLogPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FThreadList := TThreadList.Create;

  MaxLines        := 2500;             // (28/12/11) changed from 500 to 2500
  FMaxLength      := 0;
  FUpdateInterval := 50;               // (28/12/11) changed from 0 to 50, to be thread-safe
  TimeFormat      := 'hh:nn:ss';
  TimeMS          := false;
  TimeTickCount   := false;
  TimeDiff        := false;
  Paused          := false;


  FUpdateTimer := TTimer.Create(Self);
  FUpdateTimer.Enabled := false;
  FUpdateTimer.OnTimer := @UpdateTimerEvent;
end;


destructor TLogPanel.Destroy;
begin
    ClearList;
    FThreadList.Destroy;
end;


procedure TLogPanel.Add(MsgText: String; TextColor: TColor = $7FFFFFFF;
    BackColor: TColor = $7FFFFFFF);
var
  List: TList;
  Msg, OldMsg: TLogPanelMsg;
  Prompt, AText: String;

begin
  try
    Prompt := TimeStr;
    AText   := Prompt + MsgText;

    if (FMaxLength < Length(AText)) then
      FMaxLength := Length(AText);

    if (TextColor = $7FFFFFFF) then
      TextColor := FDefColor;

    if (BackColor = $7FFFFFFF) then
      BackColor := FDefBack;

    Msg := TLogPanelMsg.Create(Prompt, MsgText, TextColor, BackColor);

    // Add to list
    List := FThreadList.LockList;

    if FAddAtEnd then
    begin
      // Remove first line if list is full
      if (List.Count >= FMaxLines) then
      begin
        OldMsg := TObject(List.Items[0]) as TLogPanelMsg;
        OldMsg.Destroy;
        List.Delete(0);
      end;

      // Add at end
      List.Add(Msg);
    end
    else
    begin
      // Remove last line if list is full
      if (List.Count >= FMaxLines) then
      begin
        OldMsg := TObject(List.Items[List.Count-1]) as TLogPanelMsg;
        OldMsg.Destroy;
        List.Delete(List.Count-1);
      end;

      // Insert at top
      List.Insert(0, Msg);
    end;

    FThreadList.UnlockList;

    //
    if FAutoScroll and not FPaused then
    begin
      // Trigger timer or update now
      if (FUpdateInterval > 0) then
      begin
        FUpdateTimer.Interval := FUpdateInterval;
        FUpdateTimer.Enabled  := true;
      end
      else
      begin
        SetScrollBars;
        FPaintBox.Invalidate;
      end;
    end;

  finally
  end;

end;


procedure TLogPanel.ClearList;
var
  List: TList;
  Msg: TLogPanelMsg;

begin
  List := FThreadList.LockList;

  while (List.Count > 0) do
  begin
    Msg := TObject(List.Items[0]) as TLogPanelMsg;
    Msg.Destroy;
    List.Delete(0);
  end;
  List.Clear;
  FThreadList.UnlockList;

  FMaxLength := 0;
end;


procedure TLogPanel.Clear;
begin
  ClearList;
  SetScrollBars;
  Invalidate;
end;


procedure TLogPanel.UpdateTimerEvent(Sender: TObject);
begin
    FUpdateTimer.Enabled := false;

end;




function TLogPanel.TimeStr : String;
var
  Str: String;
  Tick: DWord;
  DT: TDateTime;
  Hour, Minute, Second, MS: Word;
begin
    Tick := GetTickCount();
    if (FLastTickCount = 0) then
        FLastTickCount := Tick;

    if FTimeTickCount then
    begin
      Str := Format('%10u', [Tick]);
    end
    else if (FTimeFormat <> '') then
    begin
        DT := Now();
        Str := FormatDateTime(FTimeFormat, DT);

        // Add Miliseconds
        if FTimeMS then
        begin
            DecodeTime(DT, Hour, Minute, Second, MS);
            Str := Str + Format('.%02d', [MS div 10]);
        end;
    end;

    if FTimeDiff then
    begin
		  if (Str = '') then
        Str := Str + Format('[%6u]', [Tick - FLastTickCount])
      else
        Str := Str + Format(' [%6u]', [Tick - FLastTickCount]);
    end;

    FLastTickCount := Tick;

    if (Str <> '') then
        Str := Str + ' > ';

    Result := Str;
end;










// (23/04/2019)
static int InstanceNum = -1;


function LogDirectoryExists(Path: String) : Boolean;

static function LogCreatePath(Path: Stringh) : Boolean
begin
    String Dir;

    while (Path <> "") do
    begin
        int Pos = Path.Pos("\\");
        if (Pos == 0) Pos = Path.Length();
        String Subdir = Path.SubString(1, Pos);
        Path.Delete(1, Pos);

        Dir = Dir + Subdir;
        if (!LogDirectoryExists(Dir))
			CreateDir(Dir);
    end;

    return LogDirectoryExists(Dir);
end;

static function LogDirectoryExists(String Path) : Boolean;
begin
    DWORD Attr = GetFileAttributes(Path.c_str());
    return (Attr != 0xFFFFFFFF) && ((Attr & FILE_ATTRIBUTE_DIRECTORY) != 0);
end;

//===========================================================================
//                                  Log API
//===========================================================================
#define SLOGEXP

// More flags
#define SLOGF_CLEAR_FILE        0x00000000          // Clear file
#define SLOGF_FORCE_PATH        0x00000000          // Create path if not exist
#define SLOGF_COLOR             0x00000000          // Log text and background color

//
// Current format:
//
//      Date     Time         Flags    Pid  App           Text
//      23/10/02 11:23:46.435 00000004 1088 SLoader.exe > Log started
//
//
//
//===========================================================================
//                                  Log API
//===========================================================================
TLogger* DefaultLog = NULL;

function SetDefaultLogFile(String FileName, int MaxSize) : Boolean;
begin
    if (DefaultLog)
        delete DefaultLog;

    DefaultLog = new TLogger(Application, FileName, MaxSize);

    return true;
end;


procedure CreateDefaultLog();
begin
    if (DefaultLog)
        return;

    TIniFileEx* IniFile = new TIniFileEx(SESAM_INIFILE);

	//DefaultLogDir = IniFile->ReadString ("SystemLog", "DefaultDir", "C:\\Ntsys\\History");

    String FileName = IniFile->ReadString ("SystemLog", "FileName", "C:\\Ntsys\\Log\\sys.log");
    int MaxSize  = IniFile->ReadInteger("SystemLog", "MaxSize", 1000000);

    delete IniFile;

    DefaultLog = new TLogger(FileName, MaxSize);
end;


// Write message to log file
function LogFileLog(String FileName, String Msg, DWORD Flags) : Boolean;
begin
    TLogFile* Log = new TLogFile(FileName);

    Flags |= SLOGF_NO_START;

	Log->Log(Msg, Flags);
    delete Log;

    return true;
end;


static procedure Init()
begin
	if (!DefaultLog)
		CreateDefaultLog();
end;


function SLIBEXP SeLog(LPCTSTR szMsg) : Boolean;
begin
	Init();

	DefaultLog->Log(szMsg);

	return TRUE;
end;


function SeErr(DWORD dwFlags, const char* fmt, ...) : Boolean;
begin
	char buf[1024];

	va_list arglist;
	va_start(arglist, fmt);
	vsprintf(buf, fmt, arglist);
	va_end(arglist);

	Init();
	DefaultLog->Log(buf);

	return TRUE;
end;

//============================================================================
//							        TLogFile Class
//============================================================================

static String HostName;
static function GetHostName() : String;
begin
	// Get host name from Windows
    if (HostName == "") {
        char szBuf[MAX_COMPUTERNAME_LENGTH+1];
        DWORD size = sizeof(szBuf);
		if (GetComputerNameA(szBuf, &size))
            HostName = String(szBuf);

        else
            HostName = "?";
    }

    return HostName;
end;



TLogFile.TLogger(String FName, int MSize, DWORD AFlags) :
    TComponent(NULL)
begin
    Init(FName, MSize, AFlags);
end;


TLogFile.TLogger(TComponent* AOnwer, String FName, int MSize, DWORD AFlags) :
	TComponent(AOnwer)
begin
    Init(FName, MSize, AFlags);
end;


procedure TLogFile.Init(String FName, int MSize, DWORD AFlags);
begin
	// (21/07/14)
	CritSect = new TSMCriticalSection;;
	LogPanel = NULL;

	// Store flags
	Flags = AFlags;

    // Get default log directory
    TIniFileEx* IniFile = new TIniFileEx(SESAM_INIFILE);
    DefaultLogDir = IniFile->ReadString("Path", "Log", "C:\\Ntsys\\Log");
    if (DefaultLogDir.SubString(DefaultLogDir.Length(), 1) != "\\")
        DefaultLogDir += "\\";

    DefaultCaptureDir = IniFile->ReadString("Path", "Capture", "C:\\Ntsys\\Log\\Capture");
    if (DefaultCaptureDir.SubString(DefaultCaptureDir.Length(), 1) != "\\")
		DefaultCaptureDir += "\\";

    delete IniFile;

    // Get module data
	AppName     = ExtractFileName(ParamStr(0));

	ModuleName  = ChangeFileExt(AppName, "");

	// (23/04/2019)
	bool UseCmdLine = true;
	bool IsInstance = (ParamStr(1).CompareIC("/SVdSrvr") == 0);
	if (IsInstance) {
		InstanceNum = ParamStr(2).ToIntDef(0);
		UseCmdLine = false;
	}

	// Add also instance number, this will affect all log files
	// created by this process (23/04/2019)
	if (InstanceNum > -1) {
		ModuleName += "-InstNum-" + AnsiString(InstanceNum);
    }

	// (27/04/15)
	bool IsDriver = AppName.SubString(1, 3).UpperCase() == "DRV";

    // Set IniName as first command line parameter or module name (26/01/10)
    if (ParamStr(1) != "")
        IniName = ChangeFileExt(ExtractFileName(ParamStr(1)), "");
    else
		IniName = ModuleName;

	if (UseCmdLine) {
		// Get first command line parameter (27/04/15)
		// This is required when we have multiple processes, for example SVdDecMP4.exe
		String Param = ParamStr(1);
		for (int i=1;  i <= Param.Length();  i++) {
			if (TPath::IsValidFileNameChar(Param[i]))
				ModuleParam1 += Param[i];
			else
				ModuleParam1 += "_";
		}

		// Not for drivers (27/04/15)
		if (!ModuleParam1.IsEmpty() && !IsDriver) {
			ModuleName += "-" + ModuleParam1;
		}
	}

	//
	Pid         = getpid();
    char szPid[32];
    sprintf(szPid, "%4d", Pid);
    AppStr      = "[" + GetHostName() + "] " + ModuleName + ":" + String(szPid);
    ShortAppStr = ModuleName + ":" + String(szPid);

	// Replace $MODDIR$ with module name (27/04/15)
	int ModDirP = FName.Pos("$MODDIR$");
	if (ModDirP > 0) {
		if (IsDriver && !IniName.IsEmpty()) {
			FName.Delete(ModDirP, 8);
			FName.Insert(IniName + "\\" + IniName, ModDirP);
		}
		else {
			FName.Delete(ModDirP, 8);
			FName.Insert(ModuleName + "\\" + ModuleName, ModDirP);
        }
	}

	// Change $MOD$ to $DRV$ for drivers (27/04/15)
	int ModP = FName.Pos("$MOD$");
	if ((ModP > 0) && IsDriver) {
		FName.Delete(ModP, 5);
		FName.Insert("$DRV$", ModP);
	}

	// Replace $MOD$ with module name (03/02/08)
	ModP = FName.Pos("$MOD$");
	if (ModP > 0) {
		FName.Delete(ModP, 5);
		FName.Insert(ModuleName, ModP);
    }

    // Replace $DRV$ with first command line parameter (which is the INI file
    // name for drivers)
    if (FName == "$DRV$") {
        if (ParamStr(1) != "")
            FName = "Drivers\\" + ExtractFileName(ParamStr(1));
        else FName = "Drivers\\" + ChangeFileExt(AppName, ".");
    }

    // (26/01/10)
    else {
		int P = FName.Pos("$DRV$");
        if (P > 0) {
            FName.Delete(P, 5);
            FName.Insert("Drivers\\" + IniName, P);
        }
    }

    // Same as with "_Err"
	if (FName == "$DRVERR$") {
        if (ParamStr(1) != "")
            FName = "Drivers\\" + ChangeFileExt(ExtractFileName(ParamStr(1)), "") + "_Err";
        else FName = "Drivers\\" + ChangeFileExt(AppName, "_Err");;
    }

    // Use application name for file name
    if (FName == "")
        FName = ChangeFileExt(AppName, ".");

    //
    FName = ChangeFileExt(FName, ".log");
    if (FName.Pos("\\") == 0) {

        // Add backslash and end of string
        if (DefaultLogDir.SubString(DefaultLogDir.Length(), 1) != "\\")
			DefaultLogDir += "\\";

        FileName = DefaultLogDir + ExtractFileName(FName);
    }
    else FileName = FName;

    // (01/07/07)
	if (FileName.SubString(1, 1) != "\\" && FileName.SubString(2, 1) != ":") {

        // Add backslash and end of string
        if (DefaultLogDir.SubString(DefaultLogDir.Length(), 1) != "\\")
            DefaultLogDir += "\\";

        FileName = DefaultLogDir + FileName;
    }

    // Create directories if not exist
	String Path = ExtractFilePath(FileName);
    if (Path != "")
		LogCreatePath(Path);

    MaxFileSize         = MSize;
    CheckSizeCounter    = 0;
    Lock                = false;
	UseHeader           = true;  // !!!!(Flags & SLOGF_HEADER);
    UseAppStr           = true;  // (12/04/10)
    UseTime             = true;  // (12/04/10)

	// Delete file
    if (AFlags & SLOGF_NEW_FILE) {
        DeleteFile(FileName);
    }

    if ((AFlags & SLOGF_NO_START) == 0) {

        String CmdLine;

        for (int i=0;  i <= ParamCount();  i++)
            CmdLine += ParamStr(i) + " ";

        Log("******************** LOG STARTED ********************", SLOGF_START);
		Log("Host: " + GetHostName() + L", pid: " + String().sprintf(L"%d", Pid) + L", Cmd: " + CmdLine, SLOGF_START);

        // Log EXE file time and size (15/12/13)
        if (FileUtil) {
			String ExeFile  = ParamStr(0);
            TDateTime DT    = FileUtil->GetFileTimeDT(ExeFile);
            __int64 Size    = FileUtil->GetFileSize(ExeFile);
			Log(ExeFile + " - UTC: " + DT.FormatString("yyyy/mm/dd hh:nn:ss") + " - Size: " + String(Size) + " bytes");

            String Disk = ExeFile.SubString(1, 2).UpperCase();
            if (Disk.SubString(2, 1) == ":") {
                __int64 DiskSpace = FileUtil->DiskTotalSpace(Disk);
                __int64 FreeSpace = FileUtil->DiskFreeSpace(Disk);
				Log("Drive " + Disk + " - free space: " + FileUtil->FileSizeToString(FreeSpace) + " - size: " + FileUtil->FileSizeToString(DiskSpace));
            }

            if (Disk != "C:") {
                Disk = "C:";
                __int64 DiskSpace = FileUtil->DiskTotalSpace(Disk);
                __int64 FreeSpace = FileUtil->DiskFreeSpace(Disk);
                Log("Drive " + Disk + " - free space: " + FileUtil->FileSizeToString(FreeSpace) + " - size: " + FileUtil->FileSizeToString(DiskSpace));
			}
		}
	}
end;


TLogger::~TLogger()
begin
	//
	if (CritSect)
		delete CritSect;
end;


//============================================================================
// When szExt is specified, we log to another file (09/03/09).
// For example: Test.log, with szExt="Err", will log to TestErr.log
// This allows us to log to multiple files without the need to create more
// than one TLogger object.
procedure TLogFile.Log(String MsgStr, DWORD Flags, const char* szExt, TColor AColor);
begin
	// Replaced by CritSect (21/07/14)
	//if (Lock) return;
	//Lock = true;

	// (21/07/14)
	if (LogPanel) {
		try {
			LogPanel->Add(MsgStr, AColor);
		}
		catch (Exception& E) {
		}
	}

    //
    DWORD ThreadId = GetCurrentThreadId();

    // Optional extension, to allow separate log files with the same basic name.
    // For example: Test.log, with szExt="Err", will log to TestErr.log (09/03/09)
	String AFileName = FileName;
    if (szExt && szExt[0])
        AFileName = ChangeFileExt(AFileName, String(szExt) + ".log");

    // Prepare string
    // Must be AnsiString, because we write the .c_str() to the file using WriteFile() below (fixed 09/05/11)
    AnsiString Msg;

    // Full
    // 02/06/11 13:49:17.820 00000000[SKY]4516<DrvRTSP.exe>{1564} ...
    // 02/06/11 13:49:17.820 DrvRTSP:4516:1564 ...

    if (UseTime)
		Msg = TimeStr(Flags);

    if (UseAppStr)
		Msg += ShortAppStr + String().sprintf(L":%04X> ", ThreadId);

	//
    Msg += MsgStr;

    // Replace characters to prevent line breaks
	char* m = Msg.c_str();
    while (*m) {
		if (*m == 13 || *m == 10) *m = ' ';
		m++;
    }

	// End of line
    Msg += "\r\n";

	// (21/07/14)
	DoLog(AFileName, Msg);

	// Log messages to general log file (22/10/10)
	if (ThreadId == MainThreadId) {
		if (MainThreadLogEnabled) {
			if (!MainThreadLogFile) {
				MainThreadLogFile = new TLogFile(Application, "$MOD$_MainThreadLog", MainThreadLogSize);
			}

			//
            if (MainThreadLogFile) {
                MainThreadLogFile->Log(MsgStr, Flags);
            }
        }
	}

	//Lock = false;

    // Log also to separte file per thread (29/05/11)
    if (ThreadLogEnabled) {
        if (szExt == NULL) {
            char szThreadId[128];
            sprintf(szThreadId, "_%08X_Th_%08X", Pid, ThreadId);
            Log(MsgStr, Flags, szThreadId);
        }
    }
end;


procedure TLogFile.Log(String Msg, TColor AColor);
begin
	Log(Msg, 0, NULL, AColor);
end;


function TLogFile.DoLog(String AFileName, AnsiString Msg) : Boolean;
begin
	// Lock, with timeout to avoid process (21/07/14)
	TSMCriticalSectionLocker Locker(CritSect, true, 100);
	if (!Locker.Locked)
		return false;

	if (Locker.LockTime > 1)
		Msg += AnsiString().sprintf("[*ThreadLockTime: %d] ", Locker.LockTime);

	bool Result = false;

	// Get length
	int MsgLen = Msg.Length();

	// (02/02/15)
	if (!FileName.IsEmpty())
		AFileName = FileName;

	// Open/create
	HANDLE hFile = CreateFileW(AFileName.c_str(), GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

    // File is open
    if (hFile != INVALID_HANDLE_VALUE) {

        // Get file size
        DWORD FileSize = ::GetFileSize(hFile, NULL);

        TLogFileHeader Header;

        // Using header, initialize and read header
        if (UseHeader) {

			// New file, initialize header
            if (FileSize == 0) {
                InitHeader(&Header);
                WriteHeader(hFile, &Header);
            }

            // File already exists, read header, initialize if failed (corrupt file?)
			else if (!ReadHeader(hFile, &Header)) {
                InitHeader(&Header);

				// Data is appended at end of file
                Header.dwFilePointer = FileSize;
            }

			// extend file size by Header.dwAllocBlockSize bytes
            if ((int)Header.dwFilePointer >= ((int)FileSize-MsgLen)) {
				int Count = Header.dwAllocBlockSize;

                TAutoBuffer ABuf(32768);
                BYTE* Buf = ABuf.Buf;
                int Size = ABuf.Size;
				memset(Buf, 0, Size);

                // Seek to end
                SetFilePointer(hFile, 0, 0, FILE_END);

                // Fill file with zeros
                while (Count > 0) {
                    DWORD BytesToWrite = Size;
                    if (BytesToWrite > (DWORD)Count)
                        BytesToWrite = (DWORD)Count;

                    DWORD BytesWritten;
                    if (WriteFile(hFile, Buf, BytesToWrite, &BytesWritten, NULL)) {
                        if (BytesWritten < BytesToWrite)
                            break;
                    }
					else break;

                    Count -= BytesToWrite;
                }
            }

            if (Header.dwFilePointer == 0)
				Header.dwFilePointer = Header.dwSize;

            // Seek to position
			SetFilePointer(hFile, Header.dwFilePointer, 0, FILE_BEGIN);
        }

        // No header, simply seek to end of file
		else {
            SetFilePointer(hFile, 0, NULL, FILE_END);
        }

		// Write Msg
		DWORD BytesWritten;
		WriteFile(hFile, Msg.c_str(), MsgLen,  &BytesWritten, NULL);

		// Update file header
		if (UseHeader) {

			// Update header
			Header.dwFilePointer += MsgLen;

			WriteHeader(hFile, &Header);
		}

		// Close file
		CloseHandle(hFile);

		//
		Result = true;

		// File is too big, switch files
		if (FileSize > ((DWORD)MaxFileSize/2)) {
			String OldName = ChangeFileExt(AFileName, ".old");
			DeleteFile(OldName);
			RenameFile(AFileName, OldName);
		}
	}

	// Failed to open file
	else {
	}

	return Result;
}
//============================================================================
procedure TLogger::Logva(const char* fmt, ...)
begin
    va_list arglist;
    va_start(arglist, fmt);
	Log( Printva((char*)fmt, arglist) );
	va_end(arglist);
}
//----------------------------------------------------------------------------
function TLogger::Printva(const char* fmt, ...) : String;
begin
    char buf[4096];

    try {
        va_list arglist;
        va_start(arglist, fmt);
        vsprintf(buf, fmt, arglist);
        va_end(arglist);
    }
    catch (...) {
        try {
            sprintf(buf, "Error in SesamErrva: %s", fmt);
        }
        catch (...) {
            sprintf(buf, "Error in SesamErrva: Invalid argument");
		}
    }

    return String(buf);
end;


function TLogFile.TimeStr(DWORD Flags, bool UseFlags) : String;
begin
    unsigned short year, month, day, hour, minute, second, ms;
    char sz[128];

    TDateTime DT = Now();
    DT.DecodeDate(&year, &month, &day);
    DT.DecodeTime(&hour, &minute, &second, &ms);

    if (UseFlags)
		return String().sprintf(L"%02d/%02d/%02d %02d:%02d:%02d.%03d %08X ", day, month, year % 100, hour, minute, second, ms, Flags);
	else
		return String().sprintf(L"%02d/%02d/%02d %02d:%02d:%02d.%03d ", day, month, year % 100, hour, minute, second, ms);
end;


procedure TLogFile.InitHeader(TLogFileHeader* Header);
begin
    ZeroMemory(Header, sizeof(*Header));

    Header->dwSignature1    = 0x11031973;
    Header->dwSignature2    = 0x17052005;
    Header->dwSize          = 0;
    Header->dwFlags         = 0;
    Header->dwMaxFileSize   = 0;
    Header->dwAllocBlockSize= MaxFileSize / 8;
    Header->dwFilePointer   = 0;
    Header->dwChecksum      = 0;

end;


function TLogFile.ReadHeader(HANDLE hFile, TLogFileHeader* Header) : Boolean;
begin
    bool Result = false;

    DWORD FilePointer = SetFilePointer(hFile, 0, 0, FILE_CURRENT);
    SetFilePointer(hFile, 0, 0, FILE_BEGIN);

    char    Buf[256];
    DWORD   BytesRead;

    if (ReadFile(hFile, Buf, sizeof(Buf), &BytesRead, 0)) {
        TLogFileHeader H;
        ZeroMemory(&H, sizeof(H));

        Buf[sizeof(Buf)-1] = 0;
        sscanf(Buf, "%08X,%08X,%08X,%08X,%08X,%08X,%08X,%08X", &H.dwSignature1, &H.dwSignature2, &H.dwSize, &H.dwFlags, &H.dwMaxFileSize,
            &H.dwAllocBlockSize, &H.dwFilePointer, &H.dwChecksum);

        DWORD Checksum = CalcHeaderChecksum(&H);

        // Validate checksum
        if (Checksum == H.dwChecksum && (H.dwSignature1 == 0x11031973) && (H.dwSignature2 == 0x17052005)) {
            *Header = H;
            Result = true;
        }
    }

    SetFilePointer(hFile, FilePointer, 0, FILE_BEGIN);

    return Result;
end;


procedure TLogFile.WriteHeader(HANDLE hFile, TLogFileHeader* Header) : Boolean;
begin
    bool Result = false;

    TLogFileHeader& H = *Header;

    DWORD FilePointer = SetFilePointer(hFile, 0, 0, FILE_CURRENT);
    SetFilePointer(hFile, 0, 0, FILE_BEGIN);

    char    Buf[256];
    DWORD   BytesWritten;

    // String length: 128 chars
    sprintf(Buf, "%08X,%08X,%08X,%08X,%08X,%08X,%08X,%08X,00000000 !!! HEADER: DO NOT CHANGE THIS LINE !!! **************",
        H.dwSignature1, H.dwSignature2, H.dwSize, H.dwFlags, H.dwMaxFileSize, H.dwAllocBlockSize,
        H.dwFilePointer, CalcHeaderChecksum(&H));

    Buf[126] = '\r';
    Buf[127] = '\n';
    Buf[128] = 0;

    if (Header->dwSize == 0) {
        Header->dwSize = strlen(Buf);
    }

    if (WriteFile(hFile, Buf, Header->dwSize, &BytesWritten, 0)) {
        Result = true;
    }

    SetFilePointer(hFile, FilePointer, 0, FILE_BEGIN);

    return Result;
end;


function TLogFile.CalcHeaderChecksum(TLogFileHeader* Header) : DWord;
begin
    DWORD Checksum = Header->dwSignature1 ^ Header->dwSignature2 ^ Header->dwSize ^ Header->dwFlags ^
        Header->dwMaxFileSize ^ Header->dwAllocBlockSize ^ Header->dwFilePointer;

    return Checksum;
end;


//===========================================================================
//                             Screen Capture
//===========================================================================
bool TLogFile.CaptureScreen(String CaptureFileName)
begin
    bool Result = false;

    // File name not specified, create it from log file and current time
    if (CaptureFileName == "") {
        TDateTime NowDT = Now();
        String TimeStr = NowDT.FormatString("yyyy-mm-dd_hh-nn-ss");
        CaptureFileName = DefaultCaptureDir + ChangeFileExt(ExtractFileName(FileName), TimeStr + ".bmp");
    }

    // Name specified without directory name
    else if (ExtractFilePath(FileName) == "") {
        CaptureFileName = DefaultCaptureDir + ChangeFileExt(CaptureFileName, ".bmp");
    }

	Graphics::TBitmap* Bitmap = CreateBitmapFromDC( GetDC(GetDesktopWindow()), Screen->Width, Screen->Height );

    try {
        ForceDirectories(ExtractFilePath(CaptureFileName));
        Bitmap->SaveToFile(FileName);
        Result = true;
    }
    catch (Exception& E) {
    }

    delete Bitmap;

    return Result;
end;


Graphics::TBitmap* TLogFile.CreateBitmapFromDC(HDC hDC,
	int TrgWidth, int TrgHeight, int SrcX, int SrcY, int TrgX, int TrgY)
begin
	// Create bitmap compatible with the DC
	HBITMAP hBitmap = CreateCompatibleBitmap(hDC, TrgWidth, TrgHeight);

	// Create compatible DC and select target bitmap
	HDC hTrgDC = CreateCompatibleDC(hDC);
	SelectObject(hTrgDC, hBitmap);

	// Copy from source DC to the bitmap selected on memory DC
	BitBlt(hTrgDC, TrgX, TrgY, TrgWidth, TrgHeight, hDC, SrcX, SrcY, NOTSRCCOPY);

	// Delete the DC
	DeleteDC(hTrgDC);

	Graphics::TBitmap *Bitmap = new Graphics::TBitmap;
	Bitmap->Handle = hBitmap;

	return Bitmap;
end;


procedure TLogFile.Dump(BYTE* Data, int Len, String AText);
begin
    AText = DumpStr(Data, Len, AText);
    Log(AText);
end;


function TLogFile.DumpStr(BYTE* Data, int Len, String AText) : String;
begin
    static char Hex[16] = "0123456789ABCDEF";

    if (Len > 10000)
        Len = 10000;

    TAutoBuffer ABuf(3*Len + 16);
    char* sz = (char*)ABuf.Buf;
    char* s = sz;

    //
    for (int i=0;  i < Len;  i++) {
        *s++ = Hex[ (*Data) >> 4 ];
		*s++ = Hex[ (*Data) & 0x0F ];
        *s++ = ' ';
        Data++;
    }

    //
    *s = 0;
    AText += String(sz);

    return AText;
end;


function TLogFile.DumpStrText(BYTE* Data, int Len, String AText) : String;
begin
	if (Len > 10000)
		Len = 10000;

    TAutoBuffer ABuf(Len + 16);
    char* sz = (char*)ABuf.Buf;
    char* s = sz;

	//
	for (int i=0;  i < Len;  i++) {
		char Ch = *Data++;
		if (Ch < 32) {
			Ch = '.';
		}
		*s++ = Ch;
	}

	//
	*s = 0;
	AText += String(sz);

	return AText;
end;


// (04/02/15)
function TLogFile.GetValidFileName(String AText) : String;
begin
	String FileName = AText;

	// Remove invalid chars
	String ValidChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.";
	for (int i=1;  i <= FileName.Length();  i++) {
		if (ValidChars.Pos(FileName[i]) < 1)
			FileName[i] = '_';
	}

	return FileName;
end;



end.

