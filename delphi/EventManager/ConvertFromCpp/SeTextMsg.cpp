//---------------------------------------------------------------------------
#pragma hdrstop

#include "SeTextMsg.h"
#include <stdio.h>

#include "SeLogFile.h"
//---------------------------------------------------------------------------
static TLogFile* LogFile = NULL;

static void Log(String S)
{
	if (!LogFile)
		LogFile = new TLogFile("$MOD$-SeTextMsg");

	LogFile->Log(S);
}
//---------------------------------------------------------------------------
#pragma package(smart_init)

//------------------------------------------------------------------------------
TSTextMsg::TSTextMsg(const char* szStr)
{
   Init();
   if (szStr)
	  Text = szStr;
}
//------------------------------------------------------------------------------
TSTextMsg::TSTextMsg(String Str)
{
   Init();
   Text = Str;
}
//---------------------------------------------------------------------------
void TSTextMsg::Init()
{
   Separator        = '\n';
   LinesSeparator   = 1;
   Pos              = 0;
}
//---------------------------------------------------------------------------
void TSTextMsg::Assign(const TSTextMsg& Msg)
{
   Text             = Msg.Text;
   Separator        = Msg.Separator;
   Pos              = Msg.Pos;

   AnsiText			= Text;
}
//---------------------------------------------------------------------------
void TSTextMsg::Clear()
{
   Text = "";
   AnsiText = "";
}
//---------------------------------------------------------------------------
void TSTextMsg::Start(const char* szStr)
{
   Text = String(szStr);
   AnsiText = Text;
}
//---------------------------------------------------------------------------
void TSTextMsg::PutRaw(const String& Str)
{
   //
   Text += String(Separator) + Str;

   // Convert to AnsiString
   AnsiText = Text;
}
//---------------------------------------------------------------------------
// (01/12/2015)
void TSTextMsg::PutEsc(const String& Str)
{
	String FStr = Escape(Str);
	PutRaw(FStr);
}
//---------------------------------------------------------------------------
// (01/12/2015)
void TSTextMsg::Put(const String& Str)
{
	PutRaw(Str);
}
//---------------------------------------------------------------------------
// (01/12/2015)
// Must clean CR/LF etc. otherwise we get a message the protocol (01/12/2015)
// This might happen when sending PanelText, like in TSesamInterface::SetPanelText()
String TSTextMsg::Escape(String Str)
{
   static char Hex[16] = "0123456789ABCDEF";

   int Len = Str.Length();

   //
   String AStr;
   AStr.SetLength(5*Len);

   //
   wchar_t* s = Str.c_str();
   wchar_t* a = AStr.c_str();

   int L = 0;
   int i = 0;
   while (i < Len) {
		WideChar C = s[i];
		if (C < 32) {
			a[L++] = '\\';
			a[L++] = '0';
			a[L++] = 'x';
			a[L++] = Hex[C >> 4];
			a[L++] = Hex[C & 0x0F];
		}
		else {
			a[L++] = C;
		}
		i++;
   }

   AStr.SetLength(L);

   { //if (L != Len) {
		String Un = UnEscape(AStr);
		if (Un != Str) {
			Log("TSTextMsg::Escape: Bad conversion");
			Log("Str:  " + Str);
			Log("AStr: " + AStr);
			Log("Un:   " + Un);
		}
   }

   return AStr;
}
//---------------------------------------------------------------------------
static int HexToInt(WideChar C)
{
	int Value;

	if ((C >= '0') && (C <= '9'))
		Value = C - '0';
	else if ((C >= 'A') && (C <= 'F'))
		Value = C - ('A' - 10);
	else
		Value = -1;

	return Value;
}
//---------------------------------------------------------------------------
// (01/12/2015)
String TSTextMsg::UnEscape(String Str)
{
   int Len = Str.Length();
   String AStr;
   AStr.SetLength(Len);

   //
   wchar_t* s = Str.c_str();
   wchar_t* a = AStr.c_str();

   int L = 0;
   int i = 0;
   while (i < Len) {
		WideChar C = s[i];

		if ((i <= Len-5) && (C == '\\') && (s[i+1] == '0') && s[i+2] == 'x') {
			int H1 = HexToInt(s[i+3]);
			int H2 = HexToInt(s[i+4]);

			if ((H1 > -1) && (H2 > -1)) {
				int X = (H1 << 4) | H2;
				a[L++] = WideChar(X);
				i += 5;
			}
			else {
				a[L++] = C;
				i++;
			}
		}
		else {
			a[L++] = C;
			i++;
		}
   }

   //
   if (L != Len) {
	   AStr.SetLength(L);
   }

   return AStr;
}
//---------------------------------------------------------------------------
void TSTextMsg::Put(const char* szStr)
{
   Put(String(szStr));
}
//---------------------------------------------------------------------------
#ifdef never
// Replaced by the code above (01/12/2015)
void TSTextMsg::Put(const char* szStr)
{
   if (szStr)
	   Text = Text + String(Separator) + String(szStr);
   else
	   Text = Text + String(Separator) + String("");

   // Convert to AnsiString
   AnsiText = Text;
}
//---------------------------------------------------------------------------
void TSTextMsg::Put(const String& Str)
{
   Put(AnsiString(Str).c_str());
}
#endif
//---------------------------------------------------------------------------
void TSTextMsg::PutInt(int i)
{
   char Buf[32];

   sprintf(Buf, "%d", i);
   Put(Buf);
}
//---------------------------------------------------------------------------
void TSTextMsg::PutDWORD(DWORD dw)
{
   char Buf[32];

   sprintf(Buf, "%08X", dw);
   Put(Buf);
}
//---------------------------------------------------------------------------
void TSTextMsg::PutFloat(double f)
{
   char Buf[256];

   sprintf(Buf, "%lf", f);
   Put(Buf);
}
//---------------------------------------------------------------------------
void TSTextMsg::PutDateTime(TDateTime DT)
{
	PutFloat((double)DT);
}
//---------------------------------------------------------------------------
TDateTime TSTextMsg::GetDateTime()
{
	try {
		double D = GetFloat(0);
		return (TDateTime)D;
	}
	catch (Exception& E) {
		return 0;
	}
}
//---------------------------------------------------------------------------
void TSTextMsg::PutLines(const String& Lines)
{
	String Str;
	TStringList* List = new TStringList;
	List->Text = Lines;

	for (int i=0;  i < List->Count;  i++) {
		String Item = List->Strings[i];

		//
		Str = Str + Item;
		if (i < List->Count-1)
			Str += LinesSeparator;
	}

	Put(Str);
	delete List;
}
//---------------------------------------------------------------------------
void TSTextMsg::PutStringsText(TStrings* List)
{
    Put(List->CommaText);
}
//---------------------------------------------------------------------------
 void TSTextMsg::PutStringsText(String ListText)
{
    TStringList* List = new TStringList;
    List->Text = ListText;
    Put(List->CommaText);
    delete List;
}
//---------------------------------------------------------------------------
String TSTextMsg::GetLines()
{
	String Lines;
	String Str = Get();

	for (const wchar_t* p = Str.c_str();  *p;  p++) {
		if (*p == LinesSeparator)
			Lines += "\n";
		else
			Lines += *p;
	}

	return Lines;
}
//---------------------------------------------------------------------------
void TSTextMsg::First()
{
   Pos = 0;
}
//------------------------------------------------------------------------------
bool TSTextMsg::Get(char* Buf, int BufSize)
{
   AnsiText = Text;
   const char* TextBuf = AnsiText.c_str();

   if (!TextBuf[Pos]) {
	  *Buf = 0;
	  return false;
   }

   // Copy string
   int Len = 0;
   while (TextBuf[Pos] && TextBuf[Pos] != Separator) {

	  // Make sure we do not copy more data than BufSize
	  Len++;
	  if (BufSize > 0 && Len >= BufSize)
		 break;

	  // Copy char
	  *Buf++ = TextBuf[Pos++];
   }

   // Put string terminator
   *Buf = 0;

   // Skip separator
   if (TextBuf[Pos])
	  Pos++;

   return true;
}
//------------------------------------------------------------------------------
// Get next string
String TSTextMsg::GetRaw()
{
	String Str;

    TAutoBuffer ABuf(4*Text.Length() + 16);
    char* Buf = (char*)ABuf.Buf;

	if (Get(Buf, ABuf.Size)) {
		Str = String(Buf);
	}

	return Str;
}
//------------------------------------------------------------------------------
// Get next string
String TSTextMsg::Get()
{
	String Str = GetRaw();
	return Str;
}
//------------------------------------------------------------------------------
// Get next string
String TSTextMsg::GetEsc()
{
	String Str = GetRaw();
	String AStr = UnEscape(Str);
	return AStr;
}
//------------------------------------------------------------------------------
String TSTextMsg::GetToEnd()
{
   return String( GetText() + GetPos() );
}
//------------------------------------------------------------------------------
int TSTextMsg::GetInt(int Default)
{
   char Buf[1024];

   if (Get(Buf, sizeof(Buf))) {
	  int i;
	  if (sscanf(Buf, "%d", &i) == 1)
		 return i;
   }

   return Default;
}
//------------------------------------------------------------------------------
DWORD TSTextMsg::GetDWORD(DWORD Default)
{
   char Buf[1024];

   if (Get(Buf, sizeof(Buf))) {
	  DWORD dw;
	  if (sscanf(Buf, "%08X", &dw) == 1)
         return dw;
   }

   return Default;
}
//------------------------------------------------------------------------------
double TSTextMsg::GetFloat(double Default)
{
   char Buf[1024];

   if (Get(Buf, sizeof(Buf))) {
      // Replace ',' with '.' for compatibility with old versions of Local-Server software
      // Note that this is not required for new versions because now PutFloat use sprintf()
      char* p = strchr(Buf, ',');
      if (p) *p = '.';

	  double f;
	  if (sscanf(Buf, "%lf", &f) == 1)
         return f;
   }

   return Default;
}
//------------------------------------------------------------------------------
// Skip specified number of strings
bool TSTextMsg::Skip(int Count)
{
   const wchar_t* TextBuf = Text.c_str();

   while (Count-- > 0) {

	  // Skip text before separator
      while (TextBuf[Pos] && TextBuf[Pos] != Separator)
         Pos++;

      if (!TextBuf[Pos])
		 return false;

      // Skip separator
      Pos++;
   }

   return true;
}
//------------------------------------------------------------------------------
const char* TSTextMsg::GetText()
{
   AnsiText = Text;
   return AnsiText.c_str();
}
//------------------------------------------------------------------------------
AnsiString TSTextMsg::GetAnsiText()
{
   AnsiText = Text;
   return AnsiText;
}
//------------------------------------------------------------------------------
int TSTextMsg::GetPos()
{
	return Pos;
}
//------------------------------------------------------------------------------
// Put CommaText or empty string if Lines is NULL (07/11/2010)
void TSTextMsg::PutStrings(TStringList* Lines)
{
    Put(Lines ? Lines->CommaText : String(""));
}
//------------------------------------------------------------------------------
// Get CommaText to Lines, if Lines=NULL, create new TStringList (07/11/2010)
void TSTextMsg::GetStrings(TStringList*& Lines)
{
    String LText = Get();
    if (LText.Length() > 0) {
        if (!Lines)
			Lines = new TStringList;

        Lines->CommaText = LText;
    }
    else if (Lines)
        Lines->CommaText = LText;
}
//------------------------------------------------------------------------------

