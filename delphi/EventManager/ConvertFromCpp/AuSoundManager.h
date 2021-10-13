//---------------------------------------------------------------------------
#ifndef AuSoundManagerH
#define AuSoundManagerH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <MPlayer.hpp>
#include <ExtCtrls.hpp>

//===========================================================================
struct TSoundRecord {
   typedef System::AnsiString String;
   //------------------------------------------------------------------------
   enum TSoundType { stNone, stWaveFile, stTTS };
   //------------------------------------------------------------------------
   TSoundRecord(TSoundType type = stNone);
   //------------------------------------------------------------------------
   void SetWaveFile(const char* szFilename);
   void SetText(const char* szText);
   void SetDelay(int StartDelay, int EndDelay);
   void SetRepeat(int RepeatCount, int RepeatDelay);
   //------------------------------------------------------------------------
   bool IsEmpty();
   //------------------------------------------------------------------------
   TSoundType  Type;
   String      sFilename;
   String      sText;
   int         nPriority;        // Priority, 1 is highest
   int         nRepeatCount;     // Repeat count
   int         nRepeatDelay;     // Delay between repeats in miliseconds
   int         nStartDelay;
   int         nEndDelay;
   DWORD       nStartTime;       //
};
//---------------------------------------------------------------------------
class TSoundManager : public TForm
{
__published:	// IDE-managed Components
	TButton *Button1;
	TLabel *Label1;
	TEdit *EditWave;
	TButton *Button2;
	TButton *Button3;
	TEdit *EditBack;
	TLabel *Label2;
	TEdit *EditText;
	TLabel *Label3;
	TEdit *EditRepCount;
	TMediaPlayer *MediaPlayer;
	TTimer *MngTimer;
	TLabel *Label4;
	TButton *Button4;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall MngTimerTimer(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
   //------------------------------------------------------------------------
   enum TState {
      smNone,           // Nothing is being played
      smStartBackground,
      smBackground,     // Background sound is being played
      smStartDelay,     // Delay before starting sound
      smPlay,           // Forground sound is begin played
      smRepeatDelay,	// Delay between repeats
      smEndDelay        // Delay after ending sound
   };
   	//------------------------------------------------------------------------
	__fastcall TSoundManager(TComponent* Owner);
   	//------------------------------------------------------------------------
	virtual __fastcall ~TSoundManager();
   	//------------------------------------------------------------------------
   	void __fastcall Manage();
   	//------------------------------------------------------------------------
   	void __fastcall PutSound(TSoundRecord* Sound);
   	//------------------------------------------------------------------------
   	bool __fastcall GetNextSound();
   	//------------------------------------------------------------------------
   	// Set background sound
   	bool __fastcall SetBackground(TSoundRecord* Sound);
   	//------------------------------------------------------------------------
   	void __fastcall PlayBackground();
   	//------------------------------------------------------------------------
   	void __fastcall StopBackground();
   	//------------------------------------------------------------------------
   	// Play message
   	bool __fastcall PlaySound(TSoundRecord* Sound);
   	//---------------------------------------------------------------------------
   	// Stop current sound
   	void __fastcall StopSound();
   	//---------------------------------------------------------------------------
   	void __fastcall PlayWaveFile(const char* szFilename, bool bLoop = false);
   	//---------------------------------------------------------------------------
   	void __fastcall StopWaveSound();
   	//---------------------------------------------------------------------------
   	bool __fastcall EnableSound(bool Enable = true);
   	//---------------------------------------------------------------------------
   	TState			State;
   	bool           	SoundEnabled;
   	TSoundRecord*  	CurrentSound;
   	TSoundRecord*  	BackSound;
   	TList*			Queue;
   	DWORD          	StartTime;
   	DWORD           EndTime;
    DWORD			RepeatTime;
   	DWORD           Length;
	bool			HaveTTS;
   //------------------------------------------------------------------------
   bool __fastcall InitTTS();
   //------------------------------------------------------------------------
   void __fastcall CloseTTS();
   //------------------------------------------------------------------------
   void __fastcall Speak(const char* szSpeak);
   //------------------------------------------------------------------------
   void __fastcall SetPitch(int Pitch);
   void __fastcall SetSpeed(int Speed);
   void __fastcall SetVolume(int Volume);
   //------------------------------------------------------------------------
private:
};
//---------------------------------------------------------------------------
extern TSoundManager *SoundManager;
//---------------------------------------------------------------------------

//==============================================================================
inline TSoundRecord::TSoundRecord(TSoundType type)
{
   Type = type;
   nPriority = 0;
   nRepeatCount = 1;
   nRepeatDelay = 500;
   nStartDelay = 600;
   nEndDelay = 600;
}
//------------------------------------------------------------------------------
inline bool TSoundRecord::IsEmpty()
{
   return (Type == stNone);
}
//------------------------------------------------------------------------------
inline void TSoundRecord::SetWaveFile(const char *szFilename)
{
   Type = stWaveFile;
   sFilename = String(szFilename);
}
//------------------------------------------------------------------------------
inline void TSoundRecord::SetText(const char* szText)
{
   Type = stTTS;
   sText = String(szText);
}
//------------------------------------------------------------------------------
inline void TSoundRecord::SetDelay(int StartDelay, int EndDelay)
{
   nStartDelay = StartDelay;
   nEndDelay = EndDelay;
}
//------------------------------------------------------------------------------
inline void TSoundRecord::SetRepeat(int RepeatCount, int RepeatDelay)
{
   nRepeatCount = RepeatCount;
   nRepeatDelay = RepeatDelay;
}
//---------------------------------------------------------------------------
#endif
