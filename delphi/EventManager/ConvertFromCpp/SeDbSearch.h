//---------------------------------------------------------------------------
#ifndef SeDbSearchH
#define SeDbSearchH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <Db.hpp>
#include <DBGrids.hpp>
#include <DBTables.hpp>
#include <ExtCtrls.hpp>
#include <Grids.hpp>
#include <ComCtrls.hpp>

#include "ElBtnCtl.hpp"
#include "ElBtnEdit.hpp"
#include "ElCheckCtl.hpp"
#include "ElCLabel.hpp"
#include "ElCombos.hpp"
#include "ElEdits.hpp"
#include "ElLabel.hpp"
#include "ElPanel.hpp"
#include "ElPgCtl.hpp"
#include "ElPopBtn.hpp"
#include "ElXPThemedControl.hpp"
#include "XDBGrids.hpp"

#include "SeDBGridDraw.h"
#include "ElCGControl.hpp"
//---------------------------------------------------------------------------
class PACKAGE TDBSearchForm : public TForm
{
__published:	// IDE-managed Components
    TElPageControl *PageControl;
    TElTabSheet *TabSheetFind;
    TElTabSheet *TabSheetFilter;
    TElPanel *PanelFilterSign;
    TElPanel *ElPanel1;
    TElPanel *ElPanel2;
    TElLabel *Label1;
    TElLabel *Label2;
    TElLabel *Label3;
    TElComboBox *ComboField;
    TElComboBox *ComboType;
    TElComboBox *ComboText;
    TElGraphicButton *BtnFind;
    TElGraphicButton *BtnNext;
    TElGraphicButton *BtnPrev;
    TElCheckBox *CheckBoxCase;
    TElGraphicButton *BtnClose;
    TElLabel *Label4;
    TElLabel *LabelFilterText;
    TElLabel *Label6;
    TElComboBox *ComboFilterType;
    TElComboBox *ComboFilterField;
    TElComboBox *ComboFilterText;
    TElCheckBox *CheckBoxFilterCase;
    TElCheckBox *CheckBoxFilter;
	TTimer *TimerClose;
    void __fastcall BtnFindClick(TObject *Sender);
    void __fastcall BtnNextClick(TObject *Sender);
    void __fastcall BtnPrevClick(TObject *Sender);

    void __fastcall ComboTextChange(TObject *Sender);
    void __fastcall ComboFieldChange(TObject *Sender);
    void __fastcall ComboTypeChange(TObject *Sender);
    void __fastcall CheckBoxCaseClick(TObject *Sender);

    void __fastcall CheckBoxFilterClick(TObject *Sender);
    void __fastcall ComboFilterTextChange(TObject *Sender);
    void __fastcall ComboFilterFieldChange(TObject *Sender);
    void __fastcall ComboFilterTypeChange(TObject *Sender);
    void __fastcall CheckBoxFilterCaseClick(TObject *Sender);
    void __fastcall BtnCloseClick(TObject *Sender);
    void __fastcall FormKeyPress(TObject *Sender, char &Key);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall FormKeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift);
    void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall TimerCloseTimer(TObject *Sender);

private:	// User declarations
public:		// User declarations
    //-----------------------------------------------------------------------
    // Search type
    enum TDBGridSearchType { gstPartial = 0, gstStart, gstExact, gstRegExp };
    //-----------------------------------------------------------------------
    TDBGridSearchType SearchType;
    TDBGridSearchType FilterType;
    //-----------------------------------------------------------------------
    // Search operation
    enum TDBGridSearchOper { gsoSearch = 0, gsoNext, gsoPrev };
    //-----------------------------------------------------------------------
    __fastcall TDBSearchForm(TComponent* Owner);
    //-----------------------------------------------------------------------
    __fastcall ~TDBSearchForm();
    //-----------------------------------------------------------------------
    void __fastcall ShowSearch(TXDBGrid* ADBGrid);
    //-----------------------------------------------------------------------
    void __fastcall SetDBGrid(TXDBGrid* ADBGrid);
    //-----------------------------------------------------------------------
    void __fastcall SetDBGridEvents(TXDBGrid* ADBGrid);
    //-----------------------------------------------------------------------
    void __fastcall RestoreDBGrid(TXDBGrid* ADBGrid);
    //-----------------------------------------------------------------------
    void __fastcall RestoreDBGridEvents(TXDBGrid* ADBGrid, bool RestoreFilter = true);
    //-----------------------------------------------------------------------
    void __fastcall UpdateFilter();
    //-----------------------------------------------------------------------
    void __fastcall DataSourceDataChange(TObject *Sender, TField *Field);
    //-----------------------------------------------------------------------
    void __fastcall DataSetFilterRecord(TDataSet *DataSet, bool &Accept);
    //-----------------------------------------------------------------------
    void __fastcall DBGridDrawColumnCell(TObject *Sender,
          const TRect &Rect, int DataCol, TXColumn *Column, TGridDrawState State);
    //-----------------------------------------------------------------------
    void __fastcall DBGridTitleClick(TXColumn *Column);
    //-----------------------------------------------------------------------
    void __fastcall SetTableSort(TTable* Table, String FieldName, bool Descending);
    //-----------------------------------------------------------------------
    void __fastcall SetDBGridSort(TXDBGrid* Grid, TXColumn* Column);
    //-----------------------------------------------------------------------
    TField* __fastcall FindDBGridColumn(TXDBGrid* DBGrid, String FieldName);
    //-----------------------------------------------------------------------
    Variant __fastcall GetDBGridColumnValue(TXDBGrid* DBGrid, String FieldName);
    //-----------------------------------------------------------------------
    String __fastcall GetDBGridColumnString(TXDBGrid* DBGrid, String FieldName);
    //-----------------------------------------------------------------------
    String __fastcall FormatDateTimeField(TDateTime DT, String Format = "");
    //-----------------------------------------------------------------------
    String __fastcall FormatField(TField* Field, String Format = "");
    //-----------------------------------------------------------------------
    void __fastcall DrawColumnText(TDBGridColumnDrawData& Data);
    //-----------------------------------------------------------------------
    bool __fastcall LoadComboDBGridColumns(TElComboBox* ComboBox, TXDBGrid* DBGrid, bool All);
    //-----------------------------------------------------------------------
    String __fastcall GetFieldNEx(TDataSet* DSet, String FieldName);
    //-----------------------------------------------------------------------
    String __fastcall GetDBGridFieldValue(TXDBGrid* DBGrid, int DataCol, TXColumn* Column, String FieldName);
    //-----------------------------------------------------------------------
    bool __fastcall MatchRecord(TDataSet* DataSet, TXDBGrid* DBGrid, String Field, String Text, TDBGridSearchType SearchType, bool CaseSens);
    //-----------------------------------------------------------------------
    bool __fastcall MatchRecordDBGrid(TDataSet* DataSet, TXDBGrid* DBGrid, String Text, TDBGridSearchType SearchType, bool CaseSens, String& FoundField);
    //-----------------------------------------------------------------------
    bool __fastcall FilterRecord(TDataSet* DataSet, TXDBGrid* DBGrid);
    //-----------------------------------------------------------------------
    bool __fastcall DataSetNext(TDataSet* DataSet, bool Dir, bool Wrap, int& EndCount);
    //-----------------------------------------------------------------------
    bool __fastcall DoSearch(TDataSet* DataSet, TXDBGrid* DBGrid, String Field, String Text, TDBGridSearchOper Oper = gsoSearch);
    //-----------------------------------------------------------------------
    bool __fastcall Search(TDataSet* DataSet, TXDBGrid* DBGrid, String Field, String Text, TDBGridSearchOper Oper = gsoSearch);
    //-----------------------------------------------------------------------

    TDataSet*               DataSet;
    TDataSource*            DataSource;
    TXDBGrid*               DBGrid;
    
    // User Events
    TNotifyEvent            DBGridDrawColumnCellGetText;

    // Options
    bool                    RestoreOnClose;
    bool                    Filter;

    // Save Events
    TDataChangeEvent        SaveOnDataChange;
    TFilterRecordEvent      SaveOnFilterRecord;
    Xdbgrids::TDrawColumnCellEvent    SaveOnDrawColumnCell;
    Xdbgrids::TDBGridClickEvent       SaveOnTitleClick;
    bool                    SaveFiltered;

    // Search Parameters
    String                  SearchField;
    String                  SearchText;
    bool                    SearchCase;
    bool                    ShowHourGlass;

    // Search Result
    bool                    SearchFound;
    String                  SearchFoundField;

    // Filter Parameters
    bool                    FilterActive;
    bool                    FilterCase;
    String                  FilterField;
    String                  FilterText;
};

//---------------------------------------------------------------------------
void PACKAGE __fastcall ShowDBSearch(TForm* AForm, TXDBGrid* ADBGrid, bool ARestoreOnClose, TNotifyEvent AOnGetText = NULL);
//---------------------------------------------------------------------------
void PACKAGE __fastcall ShowDBSearch(TForm* AForm, TXDBGrid* ADBGrid);
//---------------------------------------------------------------------------
void PACKAGE __fastcall CloseDBSearch(TForm* AForm, TXDBGrid* ADBGrid);
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
extern PACKAGE TDBSearchForm *DBSearchForm;
//---------------------------------------------------------------------------
#endif


