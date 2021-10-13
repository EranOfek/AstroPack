//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SeSuperMatchEx.h"
#include "SeSuperMsk.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)

// Since SuperMatch is slow, first check if Mask contains '*' or '?' (31/05/12)
bool PACKAGE SuperMatchPro(String Value, String Mask)
{
	WideChar* m = Mask.c_str();
	if (wcschr(m, '*') || wcschr(m, '?'))
		return SuperMatch(Value, Mask);
	else
		return (Value == Mask);
}
//---------------------------------------------------------------------------
// Match Value with mask list
bool PACKAGE SuperMatchList(String Value, TStrings* MaskList, String* Match)
{
	for (int i=0;  i < MaskList->Count;  i++) {
		if (SuperMatchPro(Value, MaskList->Strings[i])) {
			if (Match)
				*Match = MaskList->Strings[i];

			return true;
		}
	}

	return false;
}
//---------------------------------------------------------------------------
// Match Value with mask list
bool PACKAGE SuperMatchListText(String Value, String MaskListText, String* Match)
{
    TStringList* MaskList = new TStringList;
    MaskList->Text = MaskListText;
    bool Result = SuperMatchList(Value, MaskList, Match);
    delete MaskList;

	return Result;
}
//---------------------------------------------------------------------------
// Search Value in list of mask separated by ';' or ',', for example: "ABC*;001??;?X?"
// Optionally ignore case and return the matached mask.
bool PACKAGE SuperMatchMulti(String Value, String Mask, bool IgnoreCase, String* Match)
{
    if (Mask == "" || Mask == "*" || Mask == "*.*")
        return true;

	if (IgnoreCase) {
        Value = Value.UpperCase();
        Mask  = Mask.UpperCase();
    }

    while (Mask.Length() > 0) {
        int P = Mask.Pos(";");

		// Support also ',' (06/06/11, and fixed bug 13/09/12)
        int P2 = Mask.Pos(",");
        if (P2 > 0) {
            if ((P2 < P) || (P == 0))
                P = P2;
		}
        
        if (P == 0) P = Mask.Length() + 1;
        if (Mask.Length() > 0 && SuperMatchPro(Value, Mask.SubString(1, P-1))) {
            if (Match) *Match = Mask.SubString(1, P-1);
            return true;
        }

        Mask.Delete(1, P);
    }

    return false;
}
//---------------------------------------------------------------------------
// Match Value for include/exclude list
bool PACKAGE SuperMatchListInEx(String Value, TStrings* InList, TStrings* ExList)
{
    // Check if Value is in the lists
    bool In = InList ? SuperMatchList(Value, InList) : false;
	bool Ex = ExList ? SuperMatchList(Value, ExList) : false;

    // Result
    bool Match = (In && !Ex);

    return Match;
}
//---------------------------------------------------------------------------
// Match Value for include/exclude list
bool PACKAGE SuperMatchMultiInEx(String Value, String InList, String ExList)
{
    // Check if Value is in the lists
	bool In = SuperMatchMulti(Value, InList, true, NULL);
    bool Ex = (ExList.Length() > 0) && SuperMatchMulti(Value, ExList, true, NULL);

    // Result
    bool Match = (In && !Ex);

	return Match;
}
//---------------------------------------------------------------------------
// Match Value for include/exclude list
bool PACKAGE SuperMatchMultiInExList(String Value, TStrings* InList, TStrings* ExList)
{
	bool In = false;
	bool Ex = false;

	// Check if Value is in the lists
	for (int i=0;  i < InList->Count;  i++) {
		In = SuperMatchMulti(Value, InList->Strings[i], true, NULL);
		if (In)
			break;
	}

	// Check if Value is in the lists}
	for (int i=0;  i < ExList->Count;  i++) {
		Ex = SuperMatchMulti(Value, ExList->Strings[i], true, NULL);
		if (Ex)
			break;
	}

	// Result
	bool Match = (In && !Ex);

	return Match;
}
//---------------------------------------------------------------------------
#ifdef never

// (10/11/2015)
//
// [Section]
// Timeout=1000
//
// Usage:
//
// String Timeout = IniReadStringMask(Ini, "Video", "FrameTimeout", CameraName, 10000);
//
// Scans all lines of specified section [Video], looking for FrameTimeout= parameter
// than match CameraName, using default of
class TFMemIniFileEx : public TFMemIniFile {

	__fastcall TFMemIniFileEx(String FileName);

	String __fastcall IniReadStringMask(TStrings* Lines, String Key, String AName, String Default)

	String MaskSeparator;
};


__fastcall TFMemIniFileEx::TFMemIniFileEx(String FileName) :
	TFMemIniFile(FileName)
{
	MaskSeparator = "|";
}

String __fastcall TFMemIniFileEx::IniReadStringMask(String Section, String Ident, String Key, String Default)
{
	TStringList* Lines = new TStringList;
	ReadSection(Section, Lines);
	String Value = SuperMatchIniReadString(Lines, Ident, Key, Default);
	delete Lines;

	return Value;
}

String SuperMatchIniReadString(TStrings* Lines, String Ident, String Key, String Default, String MaskSeparator)
{
	//
	String Value = Lines->Values[Ident];

	// Get general default
	if (Value.IsEmpty())
		Value = Default;

	// Scan all section lines
	for (int i=0;  i < Lines->Count; i++) {
		String Line = Lines->Strings[i];

		// Ignore empty lines, section header, and comments
		if (!Line.IsEmpty() && (Line[1] != '[') && (Line[1] != ';')) {
			int P = Line.Pos("=");
			if (P > 1) {
				// Parse line
				String AParam = Line.SubString(1, P-1).Trim();
				String AValue = Line.SubString(P+1, Line.Length());
				String AMask;

				if (SuperMatchMultiIdent(AParam, Ident, Key, MaskSeparator)) {
					Value = AValue;
					break;
				}
			}
		}
	}

	return Value;
}
//---------------------------------------------------------------------------
// Timeout=100
// Timeout|SRV:CAM01*,SRV:CAM02*=200
//
bool SuperMatchMultiIdent(String Param, String Ident, String Key, String MaskSeparator)
{
	bool Result = false;

	if (Param.CompareIC(Ident) == 0) {
		Result = true;
	} {
	else
		String AIdent = Ident + MaskSeparator;
		if (Param.SubString(1, AIdent.Length()).CompareIC(AIdent) == 0) {
		String Mask = Param.SubString(AIdent.Length()+1, Param.Length());

		if (SuperMatchMulti(Key, Mask, true)) {
			Result = true;
		}
	}

	return Reuslt;
}
#endif

