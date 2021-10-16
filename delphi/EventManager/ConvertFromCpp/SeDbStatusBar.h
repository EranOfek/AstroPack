//---------------------------------------------------------------------------
#ifndef SeDbStatusBarH
#define SeDbStatusBarH

#include <comctrls.hpp>
#include <db.hpp>

#include <ElStatBar.hpp>
#include <XDBGrids.hpp>

//===========================================================================
//                              TDBStatusBar
//===========================================================================
class PACKAGE TDBStatusBar : public TComponent {
public:
    //-----------------------------------------------------------------------
    __fastcall TDBStatusBar(TForm* AForm, TStatusBar* AStatusBar, TDataSet* ADataSet);
    //-----------------------------------------------------------------------
    __fastcall ~TDBStatusBar();
    //-----------------------------------------------------------------------
    void __fastcall Create();
    //-----------------------------------------------------------------------
    void __fastcall UpdatePanel(int Panel, String AText, TColor AColor = clBlack);
    //-----------------------------------------------------------------------
    void __fastcall Update();
    //-----------------------------------------------------------------------
    void __fastcall DBUpdate();
    //-----------------------------------------------------------------------
    void __fastcall StatusBarDrawPanel(TStatusBar *StatusBar, TStatusPanel *Panel, const TRect &Rect);
    //-----------------------------------------------------------------------
    void __fastcall DataSetAfterOpen(TDataSet *DataSet);
    //-----------------------------------------------------------------------
    void __fastcall DataSetAfterScroll(TDataSet *DataSet);
    //-----------------------------------------------------------------------
    TForm*          FForm;
    TStatusBar*     FStatusBar;
    TDataSet*       FDataSet;
    TColor          FPanelColor[256];
    int             RecordCount;

    TDataSetNotifyEvent FDataSetAfterOpen;
    TDataSetNotifyEvent FDataSetAfterScroll;
};

TDBStatusBar* PACKAGE __fastcall GetDBStatusBar(TForm* AForm);
//===========================================================================
//                              TElDBStatusBar
//===========================================================================
class PACKAGE TElDBStatusBar : public TComponent {
public:
	//-----------------------------------------------------------------------
	__fastcall TElDBStatusBar(TForm* AForm, TElStatusBar* AStatusBar, TDataSet* ADataSet, TXDBGrid* AXDBGrid);
	//-----------------------------------------------------------------------
	__fastcall ~TElDBStatusBar();
	//-----------------------------------------------------------------------
	void __fastcall Create();
	//-----------------------------------------------------------------------
	void __fastcall UpdatePanel(int Panel, String AText, TColor AColor = clBlack);
	//-----------------------------------------------------------------------
	void __fastcall Update();
	//-----------------------------------------------------------------------
	void __fastcall DBUpdate();
	//-----------------------------------------------------------------------
	void __fastcall StatusBarDrawPanel(TElStatusBar *StatusBar, TElStatusPanel *Panel, const TRect &Rect);
	//-----------------------------------------------------------------------
	void __fastcall DataSetAfterOpen(TDataSet *DataSet);
	//-----------------------------------------------------------------------
	void __fastcall DataSetAfterScroll(TDataSet *DataSet);
	//-----------------------------------------------------------------------
	TForm*          FForm;
	TElStatusBar*   FStatusBar;
	TDataSet*       FDataSet;
	TXDBGrid*       FXDBGrid;
	TColor          FPanelColor[256];
	int             RecordCount;

	TDataSetNotifyEvent FDataSetAfterOpen;
	TDataSetNotifyEvent FDataSetAfterScroll;
};

TElDBStatusBar* PACKAGE __fastcall GetElDBStatusBar(TForm* AForm);
//---------------------------------------------------------------------------

#endif

