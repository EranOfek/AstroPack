//---------------------------------------------------------------------------
// To do: add support for SQL queries:
//
//  If dataset is ZQuery (ZDataSet), use Locate
//  If dataset is gbDataSet, use LocateExt
//

#include <vcl.h>
#pragma hdrstop

#include "SeDbSearch.h"
#include "SeSuperMsk.hpp"
#include "SesamAps.h"
#include "SeMultiLang.h"
#include "SeSuperMatchEx.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ElBtnCtl"
#pragma link "ElBtnEdit"
#pragma link "ElCheckCtl"
#pragma link "ElCLabel"
#pragma link "ElCombos"
#pragma link "ElEdits"
#pragma link "ElLabel"
#pragma link "ElPanel"
#pragma link "ElPgCtl"
#pragma link "ElPopBtn"
#pragma link "ElXPThemedControl"
#pragma link "ElCGControl"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
TDBSearchForm *DBSearchForm = NULL;

//---------------------------------------------------------------------------
#define VAL_ALL         9999

//---------------------------------------------------------------------------
struct TFormSearchPair {
    TForm*          AForm;              // Form that opens the search window
    TDBSearchForm*  SearchForm;         // Search window itself

    TFormSearchPair();
    ~TFormSearchPair();
};
//---------------------------------------------------------------------------
TFormSearchPair::TFormSearchPair()
{
	AForm       = NULL;
    SearchForm  = NULL;
}
//---------------------------------------------------------------------------
TFormSearchPair::~TFormSearchPair()
{
	//
}
//---------------------------------------------------------------------------
static TList* FormSearchList = new TList;

//===========================================================================
//                  DBSearch API Functions (with TXDBGrid)
//===========================================================================

// Show DBSearch form for specified DBGrid
void PACKAGE __fastcall ShowDBSearch(TForm* AForm, TXDBGrid* ADBGrid, bool ARestoreOnClose, TNotifyEvent AOnGetText)
{
    TFormSearchPair* Pair = NULL;

    // Search form in list
    for (int i=0;  i < FormSearchList->Count;  i++) {
        TFormSearchPair* P = (TFormSearchPair*)FormSearchList->Items[i];
		if (P->AForm == AForm) {
            Pair = P;
            break;
		}
    }

    // Add to list
    if (!Pair) {
        Pair = new TFormSearchPair;
        FormSearchList->Add(Pair);

        Pair->AForm = AForm;
        Pair->SearchForm = new TDBSearchForm(Application);

        Pair->SearchForm->SetDBGrid(ADBGrid);
		Pair->SearchForm->DBGridDrawColumnCellGetText = AOnGetText;
        Pair->SearchForm->Show();
        Pair->SearchForm->ComboText->SetFocus();
        Pair->SearchForm->RestoreOnClose = ARestoreOnClose;
    }
    else {
        Pair->SearchForm->Show();
    }
}
//---------------------------------------------------------------------------
// Show DBSearch form for specified DBGrid
void PACKAGE __fastcall ShowDBSearch(TForm* AForm, TXDBGrid* ADBGrid)
{
    ShowDBSearch(AForm, ADBGrid, false);
}
//---------------------------------------------------------------------------
void PACKAGE __fastcall CloseDBSearch(TForm* AForm, TXDBGrid* ADBGrid)
{
    TFormSearchPair* Pair;
    int Index = -1;

    // Search form in list
    for (int i=0;  i < FormSearchList->Count;  i++) {
        Pair = (TFormSearchPair*)FormSearchList->Items[i];
        if (Pair->AForm == AForm) {

            if (Pair->SearchForm) {
                if (ADBGrid)
                    Pair->SearchForm->RestoreDBGrid(ADBGrid);
                else
                    Pair->SearchForm->RestoreDBGrid(Pair->SearchForm->DBGrid);
			}

            delete Pair->SearchForm;
            delete Pair;
            Index = i;
			break;
        }
    }

    if (Index > -1)
        FormSearchList->Delete(Index);
}

//===========================================================================
//                             TXColumnConf
//===========================================================================
struct TXColumnConf {
    TXColumnConf();

    TColor      TextColor;
    TColor      BackColor;
};
//---------------------------------------------------------------------------
inline TXColumnConf::TXColumnConf()
{
    TextColor = clBlack;
    BackColor = clWhite;
}
//---------------------------------------------------------------------------
TStringList* FieldTextColor = new TStringList;


    String DefaultDateTimeFormat;

//    DefaultDateTimeFormat = SesamDBModule->IniFile->ReadString("DateTime", "DefaultReportFormat", "hh:nn:ss dd/mm");

//===========================================================================
//                          TDBSearchForm
//===========================================================================
__fastcall TDBSearchForm::TDBSearchForm(TComponent* Owner)
    : TForm(Owner)
{
    MLangUseForm(this);
    LoadElTheme(this);

    SearchCase      = false;
	SearchFound     = false;

    SearchType      = gstPartial;
    FilterType      = gstPartial;

    ShowHourGlass   = false;
    FilterActive    = false;
	FilterCase      = false;

    RestoreOnClose  = false;
    Filter          = true;


    ComboType->Items->Clear();
    ComboType->Items->Add("Partial");
    ComboType->Items->Add("Match start");
    ComboType->Items->Add("Exact");
    ComboType->Items->Add("Mask");
    ComboType->ItemIndex = 0;

    ComboFilterType->Items->Clear();
    ComboFilterType->Items->Add("Partial");
    ComboFilterType->Items->Add("Match start");
	ComboFilterType->Items->Add("Exact");
	ComboFilterType->Items->Add("Mask");
	ComboFilterType->ItemIndex = 0;

	//
	TXColumnConf* C = new TXColumnConf;
	C->TextColor = clBlue;
	FieldTextColor->AddObject("Name", (TObject*)C);

	//
	PageControl->ActivePageIndex = 0;

	// Don't use TopMost when running from debugger
	if (IsDebuggerPresent())
		FormStyle = fsNormal;

	// (02/05/12)
	TimerClose->Interval = GetPrivateProfileIntA("DBSearch", "AutoCloseTime", 300*1000, SESAM_INIFILE);
}
//---------------------------------------------------------------------------
__fastcall TDBSearchForm::~TDBSearchForm()
{
    //
}
//===========================================================================
//                              GetFieldNEx
//===========================================================================

// Get field, no exception if field does not exist
String __fastcall TDBSearchForm::GetFieldNEx(TDataSet* DSet, String FieldName)
{
    String Value;

    try {
		if (DSet->FindField(FieldName) && !DSet->FieldValues[FieldName].IsNull())
            Value = DSet->FieldValues[FieldName];
    }
    catch (Exception& E) {
    }

    return Value;
}
//===========================================================================
String __fastcall TDBSearchForm::GetDBGridFieldValue(TXDBGrid* DBGrid, int DataCol, TXColumn* Column, String FieldName)
{
    String Value;

    // (16/01/07)
    if ((FieldName == "") && Column)
        FieldName = Column->FieldName;

    if (!DBGridDrawColumnCellGetText)
        return GetFieldNEx(DBGrid->DataSource->DataSet, FieldName);

    try {

        if (!Column) {
            for (int i=0;  i < DBGrid->Columns->Count;  i++) {
                if (DBGrid->Columns->Items[i]->FieldName == FieldName) {
                    DataCol = i;
                    Column = DBGrid->Columns->Items[i];
                    break;
                }
            }
        }

        if (Column) {

            TRect Rect;
            TGridDrawState State;

			TDBGridColumnDrawData DrawData(DBGrid, Rect, DataCol, Column, State);

            // Call user event to get text and colors
			DBGridDrawColumnCellGetText((TObject*)&DrawData);

            Value = DrawData.Text;
        }
    }
    catch (Exception& E) {
    }

    return Value;
}
//===========================================================================
//                              Match Record
//===========================================================================
// Match record with specified field
bool __fastcall TDBSearchForm::MatchRecord(TDataSet* DataSet, TXDBGrid* DBGrid, String Field, String Text, TDBGridSearchType SearchType, bool CaseSens)
{
    bool Match;

    String Data = GetDBGridFieldValue(DBGrid, NULL, 0, Field);

    String Mask;
    if (!CaseSens)
        Text = UpperCase(Text);

         if (SearchType == gstPartial)  Mask = "*" + Text + "*";
    else if (SearchType == gstStart)    Mask = Text + "*";
    else                                Mask = Text;

    if (SearchType == gstExact) {
        if (CaseSens)  Match = (Data == Mask);
        else           Match = (Data.UpperCase() == Mask);
    }
    else {
        if (CaseSens)  Match = SuperMatchPro(Data, Mask);
        else           Match = SuperMatchPro(Data.UpperCase(), Mask);
    }

    return Match;
}
//---------------------------------------------------------------------------
// Match record with all fields assign to DBGrid
bool __fastcall TDBSearchForm::MatchRecordDBGrid(TDataSet* DataSet, TXDBGrid* DBGrid, String AText, TDBGridSearchType SearchType, bool CaseSens, String& FoundField)
{
    if (AText == "")
        return false;

    bool    Match = false;
    String  Mask;

    if (!CaseSens)
        AText = UpperCase(AText);

    //
         if (SearchType == gstPartial)  Mask = "*" + AText + "*";
    else if (SearchType == gstStart)    Mask = AText + "*";
    else                                Mask = AText;

    for (int i=0;  i < DBGrid->Columns->Count;  i++) {
        TXColumn* Column = DBGrid->Columns->Items[i];
        String Data = GetDBGridFieldValue(DBGrid, i, Column, "");

        if (SearchType == gstExact) {
            if (CaseSens)  Match = (Data == Mask);
            else           Match = (Data.UpperCase() == Mask);
        }
        else {
            if (CaseSens)  Match = SuperMatchPro(Data, Mask);
            else           Match = SuperMatchPro(Data.UpperCase(), Mask);
        }

        if (Match) {
            FoundField = Column->FieldName;
            break;
        }
    }

    return Match;
}
//===========================================================================
//                              Filter Record
//===========================================================================
bool __fastcall TDBSearchForm::FilterRecord(TDataSet* DataSet, TXDBGrid* DBGrid)
{
    bool Match;

    String FoundField;

    if (FilterField == "")
        Match = MatchRecordDBGrid(DataSet, DBGrid, FilterText, FilterType, FilterCase, FoundField);
    else Match = MatchRecord(DataSet, DBGrid, FilterField, FilterText, FilterType, FilterCase);

    return Match;
}

//===========================================================================
//                                 Search
//===========================================================================
// Move to next/prior record
bool __fastcall TDBSearchForm::DataSetNext(TDataSet* DataSet, bool Dir, bool Wrap, int& EndCount)
{
    if (Dir) {
        if (DataSet->Eof) {
            EndCount++;
            if (Wrap)
                DataSet->First();
        }
        else DataSet->Next();
    }
    else {
        if (DataSet->Bof) {
            EndCount++;
            if (Wrap)
                DataSet->Last();
        }
        else DataSet->Prior();
    }

    return true;
}
//---------------------------------------------------------------------------
// Search
bool __fastcall TDBSearchForm::DoSearch(TDataSet* DataSet, TXDBGrid* DBGrid, String Field, String Text, TDBGridSearchOper Oper)
{
    if (!DataSet->Active)
        return false;

    if (Text == "")
        return false;

    bool Found = false;

    DataSet->DisableControls();
    TBookmarkStr Bookmark = ((TAutoBookmarkDataSet*)DataSet)->GetBMStr();
    //TBookmark Bookmark = DataSet->GetBookmark();

    bool Dir = true;
    bool ANext = false;
    int  EndCount = 0;

    if (Oper == gsoNext)
        ANext = true;
    else if (Oper == gsoPrev) {
        ANext = true;
        Dir = false;
    }

    while (true) {
        if (ANext) {
            DataSetNext(DataSet, Dir, true, EndCount);
            ANext = false;
        }

        // Break condition
        if (EndCount > 1)
            break;

        bool Match;
        if (SearchField.Length() == 0)
            Match = MatchRecordDBGrid(DataSet, DBGrid, Text, SearchType, SearchCase, SearchFoundField);
        else {
            Match = MatchRecord(DataSet, DBGrid, SearchField, Text, SearchType, SearchCase);
            if (Match)
                SearchFoundField = SearchField;
        }

        if (Match) {
            Found = true;
            break;
        }

        // Next/prev record
        DataSetNext(DataSet, Dir, true, EndCount);
    }

    DataSet->EnableControls();

    SearchFound = Found;

    if (!Found) {
        ((TAutoBookmarkDataSet*)DataSet)->SetBMStr(Bookmark);
        //DataSet->GotoBookmark(Bookmark);
    }

    //DataSet->FreeBookmark(Bookmark);

    return Found;
}
//---------------------------------------------------------------------------
// Search
bool __fastcall TDBSearchForm::Search(TDataSet* DataSet, TXDBGrid* DBGrid, String Field, String Text, TDBGridSearchOper Oper)
{
    if (!DataSet->Active)
        return false;

    if (Text == "")
        return false;

    TCursor SaveCursor;

    if (ShowHourGlass) {
        SaveCursor = Screen->Cursor;
        Screen->Cursor = crHourGlass;
    }

    bool Result = false;

    DWORD Start = GetTickCount();

    // Save and disable AfterScroll event to prevent actions like status-bar update
    // during search process
    TDataSetNotifyEvent SaveAfterScroll = DataSet->AfterScroll;
    DataSet->AfterScroll = NULL;

    try {
        Result = DoSearch(DataSet, DBGrid, Field, Text, Oper);
    }
    catch (Exception& E) {
    }

    // Restore events
    DataSet->AfterScroll = SaveAfterScroll;

    // Now call the AfterScroll event so details will be updated (19/12/05)
    if (DataSet->AfterScroll)
        DataSet->AfterScroll(DataSet);

    // Restore cursor
    if (ShowHourGlass)
        Screen->Cursor = SaveCursor;

    DWORD Time = GetTickCount() - Start;

    // For next search operation
    ShowHourGlass = (Time > 500);

    return Result;
}
//---------------------------------------------------------------------------
// Load list of DBGrid fields
bool __fastcall TDBSearchForm::LoadComboDBGridColumns(TElComboBox* ComboBox, TXDBGrid* DBGrid, bool All)
{
    ComboBox->Items->Clear();

    if (All)
        ComboBox->Items->AddObject("(All fields)", (TObject*)VAL_ALL);

    for (int i=0;  i < DBGrid->Columns->Count;  i++) {
        TXColumn* Column = DBGrid->Columns->Items[i];
        ComboBox->Items->AddObject(Column->Title->Caption, (TObject*)i);
    }

    return true;
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::UpdateFilter()
{
	if (DataSet && DataSet->Active) {
		if (FilterActive) {
            DataSet->Filtered = false;
            DataSet->Filtered = true;
        }
        else {
            // First set to false to force refresh (14/11/11)
            DataSet->Filtered = false;
            DataSet->Filtered = SaveFiltered;
        }
    }
}
//---------------------------------------------------------------------------
#ifdef never
void __fastcall TDBSearchForm::SetFieldConf(String FieldName, TColor TextColor, TColor BackColor)
{
    TXColumnConf* C = new TXColumnConf;
    C->TextColor = TextColor;
    C->BackColor = BackColor;
    FieldTextColor->AddObject(FieldName, (TObject*)C);
}
#endif
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ShowSearch(TXDBGrid* ADBGrid)
{
    if (ADBGrid)
        SetDBGrid(ADBGrid);

    Show();
    ComboText->SetFocus();
}
//===========================================================================
//                                 SetDBGrid
//===========================================================================
void __fastcall TDBSearchForm::SetDBGrid(TXDBGrid* ADBGrid)
{
    DBGrid          = ADBGrid;
    DataSource      = DBGrid->DataSource;
    DataSet         = DataSource->DataSet;
    SaveFiltered    = DataSet->Filtered;

    if (DataSource->OnDataChange != DataSourceDataChange) {
        SaveOnDataChange = DataSource->OnDataChange;
        DataSource->OnDataChange = DataSourceDataChange;
    }

    if (Filter && DataSet->OnFilterRecord != DataSetFilterRecord) {
        SaveOnFilterRecord = DataSet->OnFilterRecord;
        DataSet->OnFilterRecord = DataSetFilterRecord;
    }

	//
	SaveOnDrawColumnCell = DBGrid->OnDrawColumnCell;
	SaveOnTitleClick = DBGrid->OnTitleClick;

	//
    DBGrid->OnDrawColumnCell = DBGridDrawColumnCell;

    // Replace title click only for TTable (we should add here also support for ZQuery SQL)
    TTable* Table = dynamic_cast<TTable*>(DataSet);
    if (Table) {
        DBGrid->OnTitleClick = DBGridTitleClick;
    }

    //
    bool All = false;
    if (DataSet->RecordCount < 1000 || DBGrid->Columns->Count < 5)
        All = true;

    LoadComboDBGridColumns(ComboField, DBGrid, All);
    ComboField->ItemIndex = 0;
    ComboFieldChange(ComboField);

    LoadComboDBGridColumns(ComboFilterField, DBGrid, All);
    ComboFilterField->ItemIndex = 0;
    ComboFilterFieldChange(ComboFilterField);


    ShowHourGlass = false;
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::SetDBGridEvents(TXDBGrid* ADBGrid)
{
    TTable* Table = dynamic_cast<TTable*>(DataSet);

    // OnDataChange
    if (DataSource->OnDataChange != DataSourceDataChange) {
        SaveOnDataChange = DataSource->OnDataChange;
        DataSource->OnDataChange = DataSourceDataChange;
    }

    // OnFilterRecord
    if (DataSet->OnFilterRecord != DataSetFilterRecord) {
        SaveOnFilterRecord = DataSet->OnFilterRecord;
        DataSet->OnFilterRecord = DataSetFilterRecord;
    }

    // OnDrawColumnCell
    if (DBGrid->OnDrawColumnCell != DBGridDrawColumnCell) {
		SaveOnDrawColumnCell = DBGrid->OnDrawColumnCell;
        DBGrid->OnDrawColumnCell = DBGridDrawColumnCell;
    }

    // OnTitleClick
    if (Table) {
        if (DBGrid->OnTitleClick != DBGridTitleClick) {
            SaveOnTitleClick = DBGrid->OnTitleClick;
            DBGrid->OnTitleClick = DBGridTitleClick;
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::RestoreDBGrid(TXDBGrid* ADBGrid)
{
    if (ADBGrid != DBGrid)
        return;

    // Restore events
    RestoreDBGridEvents(ADBGrid);

    // Disconnect from data sources
    DBGrid      = NULL;
    DataSource  = NULL;
    DataSet     = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::RestoreDBGridEvents(TXDBGrid* ADBGrid, bool RestoreFilter)
{
    //
    TBookmarkStr Bookmark = ((TAutoBookmarkDataSet*)DataSet)->GetBMStr();
    //TBookmark Bookmark = DataSet->GetBookmark();

    if (DataSource->OnDataChange == DataSourceDataChange)
        DataSource->OnDataChange = SaveOnDataChange;

	if (DBGrid->OnDrawColumnCell == DBGridDrawColumnCell)
		DBGrid->OnDrawColumnCell = SaveOnDrawColumnCell;

    if (DBGrid->OnTitleClick == DBGridTitleClick)
        DBGrid->OnTitleClick = SaveOnTitleClick;

    if ((DataSet->OnFilterRecord == DataSetFilterRecord) && RestoreFilter)
        DataSet->OnFilterRecord = SaveOnFilterRecord;

    ((TAutoBookmarkDataSet*)DataSet)->SetBMStr(Bookmark);
    //DataSet->GotoBookmark(Bookmark);
    //DataSet->FreeBookmark(Bookmark);
}
//===========================================================================
//                                  Sorting
//===========================================================================
void __fastcall TDBSearchForm::SetTableSort(TTable* Table, String FieldName, bool Descending)
{
    try {
        try {
            // Create primary index if not exist
            if (Table->IndexDefs->Count == 0) {
                Table->Active = false;
                Table->AddIndex("0", Table->FieldDefs->Items[0]->Name, TIndexOptions() << ixPrimary << ixUnique, "");  //BCB5
            }
        }
        catch (Exception& E) {
            Table->Active = true;
            //ShowMessageBox("Failed to create primary index: " + E.Message);
            return;
        }

        // Create Index Name
		// To create decending index, table level must be 7.
        String IndexName = (Descending ? "DIx" : "Ix") + FieldName;

        // Check if Index Exists - search index name in index definition of table
        bool IndexExists = false;
        for (int i=0;  i < Table->IndexDefs->Count;  i++) {
            TIndexDef* TmpIndex = (TIndexDef*)Table->IndexDefs->Items[i];
            if (TmpIndex->Name == IndexName) {
                IndexExists = true;
                break;
            }
        }

        // Create new secondary Index
        // Table must have primary index in order to allow secondary!
        if (!IndexExists) {
            Screen->Cursor = crHourGlass;
            Table->Active = false;

            try {
                if (Descending)
                    Table->AddIndex(IndexName, FieldName, TIndexOptions() << ixCaseInsensitive << ixDescending, "");
                else
                    Table->AddIndex(IndexName, FieldName, TIndexOptions() << ixCaseInsensitive, "");
            }
            catch (Exception& E) {
            }

            // Set active index
            try {
                Table->IndexName = IndexName;
            }
            catch (Exception& E) {
			}

            Screen->Cursor = crDefault;
            Table->Active = true;
        }
        else {
            Table->IndexName = IndexName;
        }
    }
    catch (Exception& E) {
    }
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::SetDBGridSort(TXDBGrid* Grid, TXColumn* Column)
{
    TTable* Table = dynamic_cast<TTable*>(Grid->DataSource->DataSet);

    String FieldName = Column->FieldName;

    bool Descending = false;

    // TTable
    if (Table) {
        SetTableSort(Table, FieldName, Descending);
    }

    // Other TDataSet
    else {
    }

    // Because descending sort is possible only for tables with level 7, we
    // currently use only standard sorting, to avoid table restructure (05/11/00).

	// Select descending sort if ...
    //if (Table->IndexName != "")
    //    if (Table->IndexName == ("Ix" + FieldName) && Table->IndexName.SubString(1, 3) != "DIx")
    //        Descending = true;
}
//===========================================================================
//                                VCL Events
//===========================================================================
void __fastcall TDBSearchForm::BtnFindClick(TObject *Sender)
{
    Search(DataSet, DBGrid, "", SearchText);

    if (ComboText->Items->IndexOf(SearchText) < 0)
        ComboText->Items->Add(SearchText);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::BtnNextClick(TObject *Sender)
{
    Search(DataSet, DBGrid, SearchField, SearchText, gsoNext);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::BtnPrevClick(TObject *Sender)
{
    Search(DataSet, DBGrid, SearchField, SearchText, gsoPrev);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::BtnCloseClick(TObject *Sender)
{
    Close();
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ComboTextChange(TObject *Sender)
{
	if (ComboText->ItemIndex > -1)
        SearchText = ComboText->Items->Strings[ComboText->ItemIndex];
    else
        SearchText = ComboText->Text;

    Search(DataSet, DBGrid, SearchField, SearchText, gsoSearch);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ComboFieldChange(TObject *Sender)
{
    int Value = (int)ComboField->Items->Objects[ComboField->ItemIndex];

    if (Value == VAL_ALL)
        SearchField = "";
    else {
        TXColumn* Column = DBGrid->Columns->Items[Value];
        SearchField = Column->FieldName;
    }


    Search(DataSet, DBGrid, SearchField, SearchText);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ComboTypeChange(TObject *Sender)
{
    SearchType = (TDBGridSearchType)ComboType->ItemIndex;
    Search(DataSet, DBGrid, SearchField, SearchText);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::CheckBoxCaseClick(TObject *Sender)
{
    SearchCase = CheckBoxCase->Checked;
    Search(DataSet, DBGrid, SearchField, SearchText);
}
//===========================================================================
//                            Filter Events
//===========================================================================
void __fastcall TDBSearchForm::CheckBoxFilterClick(TObject *Sender)
{
    FilterActive = CheckBoxFilter->Checked;

    PanelFilterSign->Visible = FilterActive;

    UpdateFilter();
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ComboFilterTextChange(TObject *Sender)
{
    if (ComboFilterText->ItemIndex > -1)
        FilterText = ComboFilterText->Items->Strings[ComboFilterText->ItemIndex];
    else
        FilterText = ComboFilterText->Text;

    CheckBoxFilter->Checked = (FilterText != "");

    UpdateFilter();
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ComboFilterFieldChange(TObject *Sender)
{
    int Value = (int)ComboFilterField->Items->Objects[ComboFilterField->ItemIndex];

    if (Value == VAL_ALL)
        FilterField = "";
    else {
        TXColumn* Column = DBGrid->Columns->Items[Value];
		FilterField = Column->FieldName;
    }

    UpdateFilter();
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::ComboFilterTypeChange(TObject *Sender)
{
    FilterType = (TDBGridSearchType)ComboType->ItemIndex;
    UpdateFilter();
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::CheckBoxFilterCaseClick(TObject *Sender)
{
    FilterCase = CheckBoxFilterCase->Checked;
    UpdateFilter();
}
//===========================================================================
//                          Grid Draw Utilities
//===========================================================================
TField* __fastcall TDBSearchForm::FindDBGridColumn(TXDBGrid* DBGrid, String FieldName)
{
    for (int i=0;  i < DBGrid->Columns->Count;  i++) {
        if (DBGrid->Columns->Items[i]->FieldName == FieldName)
            return DBGrid->Columns->Items[i]->Field;
    }

    return NULL;
}
//---------------------------------------------------------------------------
Variant __fastcall TDBSearchForm::GetDBGridColumnValue(TXDBGrid* DBGrid, String FieldName)
{
    TField* F = FindDBGridColumn(DBGrid, FieldName);
	if (F)
        return F->Value;
    else
        return NULL;
}
//---------------------------------------------------------------------------
String __fastcall TDBSearchForm::GetDBGridColumnString(TXDBGrid* DBGrid, String FieldName)
{
    TField* F = FindDBGridColumn(DBGrid, FieldName);
    if (F && !F->Value.IsNull())
        return F->Value;
    else
        return "";
}
//---------------------------------------------------------------------------
String __fastcall TDBSearchForm::FormatDateTimeField(TDateTime DT, String Format)
{
    if ((double)DT != 0) {
        try {
			if (Format != "")
                return DT.FormatString(Format);
            else
                return DT.FormatString(DefaultDateTimeFormat);
        }
		catch (Exception& E) {
        }
    }

    return "";
}
//---------------------------------------------------------------------------
String __fastcall TDBSearchForm::FormatField(TField* Field, String Format)
{
	if (Field && !Field->IsNull) {
        if (Field->DataType == ftDateTime)
            return FormatDateTimeField(Field->AsDateTime, Format);

        return Field->AsString;
    }
    return "";
}

//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::DrawColumnText(TDBGridColumnDrawData& Data)
{
	if ((Data.State.Contains(gdSelected) || Data.State.Contains(gdFocused)) && !Data.FrameSelected) {
		if (SearchFound) {

            if (Data.Column && Data.Column->Field && Data.Column->Field->FieldName == SearchFoundField) {
                Data.XDBGrid->Canvas->Font->Color   =  clWhite;
                Data.XDBGrid->Canvas->Brush->Color  =  clRed;
            }
			else {
                Data.XDBGrid->Canvas->Font->Color   =  clWhite;
                Data.XDBGrid->Canvas->Brush->Color  =  clGreen;
            }
        }
		else {
            Data.XDBGrid->Canvas->Font->Color   =  clHighlightText;
            Data.XDBGrid->Canvas->Brush->Color  =  clHighlight;
        }
    }
    else {
        Data.XDBGrid->Canvas->Font->Color   =  Data.TextColor;
        Data.XDBGrid->Canvas->Brush->Color  =  Data.BkgColor;
    }

    int Xoffset = 2;
    int Yoffset = (Data.Rect.Bottom - Data.Rect.Top - Data.XDBGrid->Canvas->TextHeight(Data.Text)) / 2;
    Data.XDBGrid->Canvas->FillRect(Data.Rect);
    Data.XDBGrid->Canvas->TextOut(Data.Rect.Left + Xoffset, Data.Rect.Top + Yoffset, Data.Text);

	// Draw only two horizontal lines around selected cells
	if ((Data.State.Contains(gdSelected) || Data.State.Contains(gdFocused)) && Data.FrameSelected) {
		int W = 3;

		Data.XDBGrid->Canvas->Pen->Color = clHighlight;
		Data.XDBGrid->Canvas->Pen->Width = W;

		Data.XDBGrid->Canvas->MoveTo(Data.Rect.Left, Data.Rect.Top+1);
		Data.XDBGrid->Canvas->LineTo(Data.Rect.Right, Data.Rect.Top+1);

		Data.XDBGrid->Canvas->MoveTo(Data.Rect.Left, Data.Rect.Bottom-W+1);
		Data.XDBGrid->Canvas->LineTo(Data.Rect.Right, Data.Rect.Bottom-W+1);
	}
}
//===========================================================================
//                              DataSet Events
//===========================================================================
void __fastcall TDBSearchForm::DataSetFilterRecord(TDataSet *DataSet, bool &Accept)
{
    if (FilterActive) {

        // First filter with old event (14/11/11)
        if (SaveOnFilterRecord)
            SaveOnFilterRecord(DataSet, Accept);

        // Now apply our filter
        if (Accept)
			Accept = FilterRecord(DataSet, DBGrid);
    }
    else {
        if (SaveOnFilterRecord)
            SaveOnFilterRecord(DataSet, Accept);
        else Accept = true;
    }
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::DataSourceDataChange(TObject *Sender, TField *Field)
{
    if (!Field)
        SearchFound = false;

    if (SaveOnDataChange)
		SaveOnDataChange(Sender, Field);
}
//===========================================================================
//                              Grid Events
//===========================================================================
void __fastcall TDBSearchForm::DBGridTitleClick(TXColumn *Column)
{
    try {
        if (!Column || !Column->Field)
			return;

        SetDBGridSort(DBGrid, Column);
    }
    catch (Exception& E) {
    }
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::DBGridDrawColumnCell(TObject *Sender,
	  const TRect &Rect, int DataCol, TXColumn *Column,
	  TGridDrawState State)
{
	TDBGridColumnDrawData DrawData(Sender, Rect, DataCol, Column, State);

    try {
		if (!Column)
            return;

        // Call user event to get text and colors
		if (DBGridDrawColumnCellGetText) {

            try {
				DBGridDrawColumnCellGetText((TObject*)&DrawData);
            }
			catch (Exception& E) {
            }

        }

		else {
            // Format field text
            DrawData.Text = FormatField(Column->Field, "");
		}


        // 
        if (Column && Column->Field) {

            int Index = FieldTextColor->IndexOf(Column->Field->FieldName);
            if (Index > -1) {
                TXColumnConf* Conf = (TXColumnConf*)FieldTextColor->Objects[Index];
                DrawData.TextColor = Conf->TextColor;
                DrawData.BkgColor = Conf->BackColor;
            }
		}

		// DBGrid has its own Draw event, use it (14/05/13)
		if (SaveOnDrawColumnCell) {
			SaveOnDrawColumnCell(Sender, Rect, DataCol, Column, State);

			// Draw only two horizontal lines around selected cells
			if ((State.Contains(gdSelected) || State.Contains(gdFocused))) {
				int W = 3;

				DBGrid->Canvas->Pen->Color = clHighlight;
				DBGrid->Canvas->Pen->Width = W;

				DBGrid->Canvas->MoveTo(Rect.Left,  Rect.Top+1);
				DBGrid->Canvas->LineTo(Rect.Right, Rect.Top+1);

				DBGrid->Canvas->MoveTo(Rect.Left,  Rect.Bottom-W+1);
				DBGrid->Canvas->LineTo(Rect.Right, Rect.Bottom-W+1);
			}
		}

		// Draw cell
		else {
			DrawData.FrameSelected = false;
			DrawColumnText(DrawData);
		}
	}
	catch (Exception& E) {
	}
}
//===========================================================================
//                                Form Events
//===========================================================================
void __fastcall TDBSearchForm::FormKeyPress(TObject *Sender, char &Key)
{
    //
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::FormKeyDown(TObject *Sender, WORD &Key,
      TShiftState Shift)
{
	// (02/05/12)
	TimerClose->Enabled = false;
	TimerClose->Enabled = true;

	// Enter - Find
	if (Key == VK_RETURN) {
    	Key = 0;
        BtnFindClick(this);
    }

    // F3 - Next
	else if (Key == VK_F3) {
    	Key = 0;
        BtnNextClick(this);
    }

	// Escape - Close
	else if (Key == VK_ESCAPE) {
        Key = 0;
        Close();
    }
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::FormShow(TObject *Sender)
{
	TimerClose->Enabled = true;

	//
	if (DBGrid)
		SetDBGridEvents(DBGrid);
}
//---------------------------------------------------------------------------
void __fastcall TDBSearchForm::FormClose(TObject *Sender,
	  TCloseAction &Action)
{
	// (02/05/12)
	TimerClose->Enabled = false;

    // Restore events except filter
    if (DBGrid && RestoreOnClose) {
        RestoreDBGridEvents(DBGrid);  //14/11/11 , false);
    }

    // Clear filter data
    // Canceled 14/11/11
    #ifdef never
    FilterActive    = false;
    FilterCase      = false;
    FilterField     = "";
    FilterText      = "";
	#endif

    // Always cancel filter when closing form (14/11/11)
    CheckBoxFilter->Checked = false;
	//FilterActive = false;
	//UpdateFilter();
}
//---------------------------------------------------------------------------
// (02/05/12)
void __fastcall TDBSearchForm::TimerCloseTimer(TObject *Sender)
{
	//
	TimerClose->Enabled = false;

	Close();
}
//---------------------------------------------------------------------------

