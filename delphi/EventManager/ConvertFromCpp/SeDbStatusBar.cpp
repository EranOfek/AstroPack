//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SeDbStatusBar.h"

#pragma package(smart_init)
//===========================================================================
//                              TDBStatusBar
//===========================================================================
TDBStatusBar* PACKAGE __fastcall GetDBStatusBar(TForm* AForm)
{
    for (int i=0;  i < AForm->ControlCount;  i++) {
        TStatusBar* SB = dynamic_cast<TStatusBar*>(AForm->Controls[i]);
        if (SB && SB->Tag) {
            TDBStatusBar* DS = dynamic_cast<TDBStatusBar*>((TDBStatusBar*)SB->Tag);
            if (DS)
                return DS;
        }
    }

    return NULL;
}
//---------------------------------------------------------------------------
__fastcall TDBStatusBar::TDBStatusBar(TForm* AForm, TStatusBar* AStatusBar, TDataSet* ADataSet) :
    TComponent(AForm)
{
    FForm       = AForm;
    FDataSet    = ADataSet;
    FStatusBar  = AStatusBar;
    RecordCount = 0;
    
    if (FDataSet) {
        FDataSetAfterOpen   = FDataSet->AfterOpen;
        FDataSet->AfterOpen = DataSetAfterOpen;

        FDataSetAfterScroll   = FDataSet->AfterScroll;
        FDataSet->AfterScroll = DataSetAfterScroll;
    }

    for (DWORD i=0;  i < sizeof(FPanelColor)/sizeof(FPanelColor[0]);  i++)
        FPanelColor[i] = clBlack;

    Create();
    Update();
}
//---------------------------------------------------------------------------
__fastcall TDBStatusBar::~TDBStatusBar()
{
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::Create()
{
    if (!FStatusBar) {
        FStatusBar = new TStatusBar(FForm);
        FStatusBar->Align = alBottom;
        FForm->InsertControl(FStatusBar);
    }

    FStatusBar->Tag = (int)this;
    FStatusBar->OnDrawPanel = StatusBarDrawPanel;

    UpdatePanel(0, "");
    UpdatePanel(1, "");
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::UpdatePanel(int Panel, String AText, TColor AColor)
{
    int PanelIndex;
    if (Panel == -1 || Panel >= FStatusBar->Panels->Count) {
        TStatusPanel* P = FStatusBar->Panels->Add();
        PanelIndex = P->Index;
        P->Style = psOwnerDraw;
    }
    else PanelIndex = Panel;

    // Adjust panel width
    int W = FStatusBar->Canvas->TextWidth(AText) + 16;
    if (W > FStatusBar->Panels->Items[PanelIndex]->Width)
        FStatusBar->Panels->Items[PanelIndex]->Width = W;

    FPanelColor[PanelIndex] = AColor;
    FStatusBar->Panels->Items[PanelIndex]->Text = AText;


    FStatusBar->Update();
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::Update()
{
    if (FDataSet)
        DBUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::DBUpdate()
{
    if (FDataSet && FDataSet->Active) {
        try {
            String S;

            if (FDataSet->Filtered)
                S += String(FDataSet->RecordCount) + " records (Filtered)";
            else
                S += "Record " + String(FDataSet->RecNo) + " of " + String(FDataSet->RecordCount);

            UpdatePanel(0, S, clBlack);
        }
        catch (Exception& E) {
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::StatusBarDrawPanel(TStatusBar *StatusBar, TStatusPanel *Panel, const TRect &Rect)
{
    TCanvas* pCanvas = StatusBar->Canvas;

    pCanvas->Font->Color = FPanelColor[Panel->Index];

    pCanvas->FillRect(Rect);
    pCanvas->TextOut(Rect.Left + 2, Rect.Top + 2, Panel->Text);
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::DataSetAfterOpen(TDataSet *DataSet)
{
    if (FDataSetAfterOpen)
        FDataSetAfterOpen(DataSet);

    Update();
}
//---------------------------------------------------------------------------
void __fastcall TDBStatusBar::DataSetAfterScroll(TDataSet *DataSet)
{
    if (FDataSetAfterScroll)
        FDataSetAfterScroll(DataSet);

    Update();
}
//===========================================================================
//                           TElDBStatusBar
//===========================================================================
TElDBStatusBar* PACKAGE __fastcall GetElDBStatusBar(TForm* AForm)
{
	for (int i=0;  i < AForm->ControlCount;  i++) {
		TElStatusBar* SB = dynamic_cast<TElStatusBar*>(AForm->Controls[i]);
		if (SB && SB->Tag) {
			TElDBStatusBar* DS = dynamic_cast<TElDBStatusBar*>((TElDBStatusBar*)SB->Tag);
			if (DS)
				return DS;
		}
	}

	return NULL;
}
//---------------------------------------------------------------------------
__fastcall TElDBStatusBar::TElDBStatusBar(TForm* AForm, TElStatusBar* AStatusBar,
	TDataSet* ADataSet, TXDBGrid* AXDBGrid) :
	TComponent(AForm)
{
	FForm       = AForm;
	FDataSet    = ADataSet;
	FStatusBar  = AStatusBar;
	FXDBGrid    = AXDBGrid;
	RecordCount = 0;

	if (FDataSet) {
		FDataSetAfterOpen   = FDataSet->AfterOpen;
		FDataSet->AfterOpen = DataSetAfterOpen;

		FDataSetAfterScroll   = FDataSet->AfterScroll;
		FDataSet->AfterScroll = DataSetAfterScroll;
	}

	for (DWORD i=0;  i < sizeof(FPanelColor)/sizeof(FPanelColor[0]);  i++)
		FPanelColor[i] = clBlack;


	Create();

	// Status bar simple panel can be set to false only if there are
	// panels exist! (shira 04/02/2007)
	FStatusBar->SimplePanel = false;

	Update();
}
//---------------------------------------------------------------------------
__fastcall TElDBStatusBar::~TElDBStatusBar()
{
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::Create()
{
	if (!FStatusBar) {
		FStatusBar = new TElStatusBar(FForm);
		FStatusBar->Align = alBottom;
		FForm->InsertControl(FStatusBar);
	}

	FStatusBar->Tag = (int)this;
///    FStatusBar->OnPanelDraw = StatusBarDrawPanel;

	UpdatePanel(0, "");
	UpdatePanel(1, "");
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::UpdatePanel(int Panel, String AText, TColor AColor)
{
	int PanelIndex;
	if ((Panel == -1) || (Panel >= FStatusBar->Panels->Count)) {
		TElStatusPanel* P = FStatusBar->Panels->Add();
		PanelIndex = P->Index;
		//P->Style = epsOwnerDraw;
	}
	else PanelIndex = Panel;

	// Adjust panel width
	int W = FStatusBar->Canvas->TextWidth(AText) + 16;
	if (W > FStatusBar->Panels->Items[PanelIndex]->Width)
		FStatusBar->Panels->Items[PanelIndex]->Width = W;

	FPanelColor[PanelIndex] = AColor;
	FStatusBar->Panels->Items[PanelIndex]->Text = AText;


	FStatusBar->Update();
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::Update()
{
	if (FDataSet)
		DBUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::DBUpdate()
{
	if (FDataSet && FDataSet->Active) {
		try {
			String S;

            // Shira 19/09/10
			//if (FXDBGrid) {
				// DataRowCount property holds the number of visible rows of the grid
				// This is not the correct property for us to use - Shira 06/02/2007
				//S += "Record " + String(FXDBGrid->Position+1) + " of " + String(FXDBGrid->DataRowCount);
			//	S += String(FXDBGrid->RecNumber) + " / " + String(FXDBGrid->RecCount);
			//}
			//else {
				if (FDataSet->Filtered)
					S += String(FDataSet->RecordCount) + " records (Filtered)";
				else
					S += String(FDataSet->RecNo) + " / " + String(FDataSet->RecordCount);
					//S += "Record " + String(FDataSet->RecNo) + " of " + String(FDataSet->RecordCount);
			//}

			UpdatePanel(0, S, clBlack);
		}
		catch (Exception& E) {
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::StatusBarDrawPanel(TElStatusBar *StatusBar, TElStatusPanel *Panel, const TRect &Rect)
{
#ifdef never
	TCanvas* pCanvas = StatusBar->Canvas;

	pCanvas->Font->Color = FPanelColor[Panel->Index];

	pCanvas->FillRect(Rect);
	pCanvas->TextOut(Rect.Left + 2, Rect.Top + 2, Panel->Text);
#endif
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::DataSetAfterOpen(TDataSet *DataSet)
{
	if (FDataSetAfterOpen)
		FDataSetAfterOpen(DataSet);

	Update();
}
//---------------------------------------------------------------------------
void __fastcall TElDBStatusBar::DataSetAfterScroll(TDataSet *DataSet)
{
	if (FDataSetAfterScroll)
		FDataSetAfterScroll(DataSet);

	Update();
}
//===========================================================================


