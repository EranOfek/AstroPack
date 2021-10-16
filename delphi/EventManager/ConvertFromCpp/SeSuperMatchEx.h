//---------------------------------------------------------------------------
#ifndef SeSuperMatchExH
#define SeSuperMatchExH
//---------------------------------------------------------------------------
// Since SuperMatch is slow, first check if Mask contains '*' or '?' (31/05/12)
bool PACKAGE SuperMatchPro(String Value, String Mask);
//---------------------------------------------------------------------------
// Match Value with mask list
bool PACKAGE SuperMatchList(String Value, TStrings* MaskList, String* Match = NULL);
//---------------------------------------------------------------------------
// Match Value with mask list
bool PACKAGE SuperMatchListText(String Value, String MaskListText, String* Match = NULL);
//---------------------------------------------------------------------------
// Search Value in list of mask separated by ';', for example: "ABC*;001??;?X?"
// Optionally ignore case and return the matached mask.
bool PACKAGE SuperMatchMulti(String Value, String Mask, bool IgnoreCase = true, String* Match = NULL);
//---------------------------------------------------------------------------
// Match Value for include/exclude list
bool PACKAGE SuperMatchListInEx(String Value, TStrings* InList, TStrings* ExList);
//---------------------------------------------------------------------------
bool PACKAGE SuperMatchMultiInEx(String Value, String InList, String ExList);
//---------------------------------------------------------------------------
// (08/01/13)
bool PACKAGE SuperMatchMultiInExList(String Value, TStrings* InList, TStrings* ExList);
//---------------------------------------------------------------------------
#endif

