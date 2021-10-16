//---------------------------------------------------------------------------
#include <vcl.h>
#include <windows.h>
#include <string.h>
#include <stdio.h>
#include <mmsystem.h>
#pragma hdrstop

#ifdef TTS
#include <initguid.h>
#include <objbase.h>
//#include <objerror.h>
#include <ole2ver.h>
#include "speech.h"
#endif

#include "SesamCoreAPI.h"
#include "SesamSound.h"
#include "AuSoundManager.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TSoundManager *SoundManager = NULL;
//******************************************************************************
//								Text-To-Speech Stuff
//******************************************************************************
#ifdef TTS
static BOOL BeginOLE();
static BOOL EndOLE();
static PITTSCENTRALW FindAndSelect(PTTSMODEINFOW pTTSInfo);

//static HINSTANCE         ghInstance			= NULL;
static PITTSCENTRALW     	m_pITTSCentral		= NULL;
static PITTSATTRIBUTES   	m_pITTSAttributes	= NULL;
static bool 				TTS_Finished = false;

//---------------------------------------------------------------------------
// Notification objects

class CTestNotify : public ITTSNotifySink {
   private:

   public:
      CTestNotify ();
      ~CTestNotify (void);

      // IUnkown members that delegate to m_punkOuter
      // Non-delegating object IUnknown
      STDMETHODIMP         QueryInterface (REFIID, LPVOID FAR *);
      STDMETHODIMP_(ULONG) AddRef(void);
      STDMETHODIMP_(ULONG) Release(void);

      // ITTSNotifySink
		STDMETHOD (AttribChanged)  (DWORD);
		STDMETHOD (AudioStart)     (QWORD);
		STDMETHOD (AudioStop)      (QWORD);
		STDMETHOD (Visual)         (QWORD, CHAR, CHAR, DWORD, PTTSMOUTH);
   };
typedef CTestNotify * PCTestNotify;
//------------------------------------------------------------------------------
class CTestBufNotify : public ITTSBufNotifySink {
   private:

   public:
      CTestBufNotify (void);
      ~CTestBufNotify (void);

      // IUnkown members that delegate to m_punkOuter
      // Non-delegating object IUnknown
      STDMETHODIMP         QueryInterface (REFIID, LPVOID FAR *);
      STDMETHODIMP_(ULONG) AddRef(void);
      STDMETHODIMP_(ULONG) Release(void);

      // ITTSNotifySink
	   STDMETHOD (BookMark)		   	(QWORD, DWORD);
	   STDMETHOD (TextDataDone)   	(QWORD, DWORD);
	   STDMETHOD (TextDataStarted)   (QWORD);
	   STDMETHOD (WordPosition)      (QWORD, DWORD);
   };
typedef CTestBufNotify * PCTestBufNotify;


static PCTestNotify     m_pTestNotify		= NULL;
static PCTestBufNotify  m_pTestBufNotify	= NULL;
static DWORD            m_dwRegKey     		= 0xFFFFFFFF;;

#endif  // TTS
//******************************************************************************
__fastcall TSoundManager::TSoundManager(TComponent* Owner)
	: TForm(Owner)
{
	Queue = new TList;

#ifdef TTS
	HaveTTS = GetPrivateProfileIntA("Audio", "TTS", 0, SESAM_INIFILE);

	if (HaveTTS)
		InitTTS();
#endif

	SoundEnabled = GetPrivateProfileIntA("Events", "Sound", 1, SESAM_INIFILE);
    State = smStartBackground;
}
//---------------------------------------------------------------------------
__fastcall TSoundManager::~TSoundManager()
{
	MngTimer->Enabled = false;

#ifdef TTS
	CloseTTS();
#endif

    StopWaveSound();
    StopBackground();

	delete Queue;
    SoundManager = NULL;

   	if (CurrentSound) delete CurrentSound;
   	if (BackSound) delete BackSound;
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::PutSound(TSoundRecord* Sound)
{
	// Ignore TTS if no engeine
	if (Sound->Type == TSoundRecord::stTTS && !HaveTTS) {
    	delete Sound;
        return;
    }

	Queue->Add(Sound);
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::Manage()
{
   SoundEnabled = GetPrivateProfileIntA("Events", "Sound", 1, SESAM_INIFILE);
   if (!SoundEnabled)
      return;

   DWORD CurTime = GetTickCount();

   switch (State) {

      // Nothing happens
      case smNone:
      	 State = smStartBackground;
         break;

      // Start the background sound
      case smStartBackground:
         PlayBackground();
         State = smBackground;
         break;

      // Background sound is being played
      case smBackground:
      	 // Check for forground sound
         if (GetNextSound()) {
         	StartTime = CurTime + CurrentSound->nStartDelay;
         	State = smStartDelay;
         }
         break;

      // Delay before sound
      case smStartDelay:
         if (CurTime >= StartTime) {
            if (PlaySound(CurrentSound))
               State = smPlay;
            else
               State = smStartBackground;
         }
         break;

      // Forground sound is being played
      case smPlay: {
      	 bool Finished = false;

      	 if (CurrentSound->Type == TSoundRecord::stWaveFile) {
            try {
         	    Finished = (MediaPlayer->Position == MediaPlayer->Length);
            }
            catch (Exception& E) {
                Finished = true;
            }
         }
#ifdef TTS
      	 else if (CurrentSound->Type == TSoundRecord::stTTS)
         	Finished = TTS_Finished;
#endif

         if (Finished) {
	       	 if (--CurrentSound->nRepeatCount > 0) {
			   	EndTime = CurTime + CurrentSound->nRepeatDelay;
		       	State = smRepeatDelay;
	         }
	         else {
			   	EndTime = CurTime + CurrentSound->nEndDelay;
		      	State = smEndDelay;
	         }
         }
         break;
	  }

      case smRepeatDelay:
      	 if (CurTime >= RepeatTime)
         	State = smStartDelay;
      	 break;

      // Delay after sound
      case smEndDelay:
         if (CurTime >= EndTime) {
            if (CurrentSound) {
               delete CurrentSound;
               CurrentSound = NULL;
            }
            State = smStartBackground;
         }
         break;
   };
}
//------------------------------------------------------------------------------
bool __fastcall TSoundManager::GetNextSound()
{
	if (Queue->Count > 0) {
		CurrentSound = (TSoundRecord*) Queue->Items[0];
        Queue->Delete(0);
        return true;
    }
    return false;
}
//------------------------------------------------------------------------------
// Set background sound
bool __fastcall TSoundManager::SetBackground(TSoundRecord* Sound)
{
   // Do nothing if current background has higher priority
   if (Sound->Type == TSoundRecord::stNone) {
   	  delete Sound;
      Sound = BackSound;
      BackSound = NULL;
      delete Sound;
   	  State = smStartBackground;
   	  StopBackground();
      return true;
   }

   if (BackSound) {
	   if (BackSound->nPriority > Sound->nPriority)
	      return false;

      delete BackSound;
   }

   BackSound = Sound;

   if (State == smBackground)
   	  State = smStartBackground;

   return true;
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::PlayBackground()
{
   // Nothing if no sound
   if (!BackSound || BackSound->IsEmpty() || (BackSound->nPriority == 0))
      return;

   // Play background sound in loop
	if (SoundEnabled)
		sndPlaySoundA(BackSound->sFilename.c_str(), SND_ASYNC | SND_LOOP);
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::StopBackground()
{
   // Mark background sound as null
   // BackSound->nPriority = 0;

   // Stop current sound
   if (SoundEnabled)
   	  sndPlaySound(NULL, 0);
}
//------------------------------------------------------------------------------
bool __fastcall TSoundManager::PlaySound(TSoundRecord* Sound)
{
   if (Sound->IsEmpty())
      return true;

   StopBackground();

   // Wave file
   if (Sound->Type == TSoundRecord::stWaveFile) {
      PlayWaveFile(Sound->sFilename.c_str());
      return true;
   }

#ifdef TTS
   // Text-to-speech
   if (Sound->Type == TSoundRecord::stTTS) {
      Speak(Sound->sText.c_str());
      return true;
   }
#endif

   return false;
}
//==============================================================================
//                       LOW LEVEL SOUND FUNCTIONS
//==============================================================================
// Play file
void __fastcall TSoundManager::PlayWaveFile(const char* szFilename, bool bLoop)
{
   int nFlags = SND_ASYNC | (bLoop ? SND_LOOP : 0);

	if (SoundEnabled)
    	if (bLoop)
			sndPlaySoundA(szFilename, nFlags);
        else {
			try {
	        	MediaPlayer->FileName = szFilename;
	        	MediaPlayer->Open();
    	        MediaPlayer->Play();
            }
            catch (Exception& E) {
            }
        }
}
//------------------------------------------------------------------------------
// Stop
void __fastcall TSoundManager::StopWaveSound()
{
	if (SoundEnabled) {
		sndPlaySound(NULL, 0);
        try {
	        MediaPlayer->Close();
        }
        catch (Exception& E) {
        }
    }
}
//------------------------------------------------------------------------------
// Enable/disable sound
bool __fastcall TSoundManager::EnableSound(bool Enable)
{
   bool Old = SoundEnabled;
   SoundEnabled = Enable;

   if (SoundEnabled) {
   }
   else
      StopWaveSound();

   return Old;
}
//------------------------------------------------------------------------------
bool __fastcall TSoundManager::InitTTS()
{
#ifdef TTS
	if( (m_pTestNotify = new CTestNotify()) == NULL )
		MessageBox(NULL, "Error creating notify pointer.", "Warning", MB_OK );

	if( (m_pTestBufNotify = new CTestBufNotify()) == NULL )
		MessageBox(NULL, "Error creating buf notify pointer.", "Warning", MB_OK );

   m_pITTSCentral = NULL;
   m_pITTSAttributes = NULL;


   TTSMODEINFOW   ModeInfo;

   // try to begin ole
   if (!BeginOLE()) {
      MessageBox(NULL, "Can't create OLE.", "TTS Error", MB_OK);
      return false;
   }

   // find the right object
   memset (&ModeInfo, 0, sizeof(ModeInfo));
   m_pITTSCentral = FindAndSelect (&ModeInfo);
   if (!m_pITTSCentral) {
      MessageBox(NULL, "Can't create TTS engine.", "TTS Error", MB_OK);
      return false;
   };

    m_pITTSCentral->QueryInterface (IID_ITTSAttributes, (void**)&m_pITTSAttributes);

   SetPitch(120);
   SetSpeed(120);

	m_pITTSCentral->Register((void*)m_pTestNotify, IID_ITTSNotifySink,
                                 &m_dwRegKey);
#endif
   return true;
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::CloseTTS()
{
#ifdef TTS
	if ( m_pITTSCentral ) {
	    m_pITTSCentral->UnRegister(m_dwRegKey);
        m_pITTSCentral->Release();
        m_pITTSCentral = NULL;
    }

   if (!EndOLE())
      MessageBox(NULL, "Can't shut down OLE.", "TTS Error", MB_OK);
#endif
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::Speak(const char* szSpeak)
{
#ifdef TTS
   WCHAR wszSpeak[1024];
   SDATA data;

   // Speak
   data.dwSize = (DWORD)
                  MultiByteToWideChar(CP_ACP, 0, szSpeak, -1, wszSpeak,
                  sizeof(wszSpeak) / sizeof(WCHAR)) * sizeof(WCHAR);

   data.pData = wszSpeak;
   m_pITTSCentral->TextData (CHARSET_TEXT, 0, data, NULL, IID_ITTSBufNotifySinkW);
#endif
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::SetPitch(int Pitch)
{
#ifdef TTS
   m_pITTSAttributes->PitchSet((WORD)Pitch);
#endif
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::SetSpeed(int Speed)
{
#ifdef TTS
   m_pITTSAttributes->SpeedSet(Speed);
#endif
}
//------------------------------------------------------------------------------
void __fastcall TSoundManager::SetVolume(int Volume)
{
#ifdef TTS
   DWORD dwVolume = 0xffff * Volume / 100;
   dwVolume |= dwVolume << 16;
   m_pITTSAttributes->VolumeSet(dwVolume);
#endif
}
//******************************************************************************
#ifdef TTS
// CTestNotify - Notification object.

CTestNotify::CTestNotify()
{
}

CTestNotify::~CTestNotify (void)
{
	// this space intentionally left blank
}

STDMETHODIMP CTestNotify::QueryInterface (REFIID riid, LPVOID *ppv)
{
	*ppv = NULL;

	/* always return our IUnknown for IID_IUnknown */
	if (IsEqualIID (riid, IID_IUnknown) || IsEqualIID(riid,IID_ITTSNotifySink))
	{
		*ppv = (LPVOID) this;
		return S_OK;
	}

	// otherwise, cant find
	return ResultFromScode (E_NOINTERFACE);
}

STDMETHODIMP_ (ULONG) CTestNotify::AddRef (void)
{
	// normally this increases a reference count, but this object
	// is going to be freed as soon as the app is freed, so it doesn't
	// matter
	return 1;
}

STDMETHODIMP_(ULONG) CTestNotify::Release (void)
{
	// normally this releases a reference count, but this object
	// is going to be freed when the application is freed so it doesnt
	// matter
	return 1;
}

STDMETHODIMP CTestNotify::AttribChanged (DWORD dwAttribID)
{
   // m_pCTTSAPPDlg->UpdateSliders();
   return NOERROR;
}

STDMETHODIMP CTestNotify::AudioStart (QWORD qTimeStamp)
{
   TTS_Finished = false;
   return NOERROR;
}

STDMETHODIMP CTestNotify::AudioStop (QWORD qTimeStamp)
{
   TTS_Finished = true;
   return NOERROR;
}

STDMETHODIMP CTestNotify::Visual (QWORD qTimeStamp, CHAR cIPAPhoneme,
				CHAR cEnginePhoneme, DWORD dwHints, PTTSMOUTH pTTSMouth)
{
   return NOERROR;
}
//------------------------------------------------------------------------------
// CTestBufNotify - Notification object.

CTestBufNotify::CTestBufNotify (void)
{
// this space intentionally left blank
}

CTestBufNotify::~CTestBufNotify (void)
{
// this space intentionally left blank
}

STDMETHODIMP CTestBufNotify::QueryInterface (REFIID riid, LPVOID *ppv)
{
	*ppv = NULL;

	/* always return our IUnknown for IID_IUnknown */
	if (IsEqualIID (riid, IID_IUnknown) || IsEqualIID(riid,IID_ITTSBufNotifySink))
	{
		*ppv = (LPVOID) this;
		return S_OK;
	}

	// otherwise, cant find
	return ResultFromScode (E_NOINTERFACE);
}

STDMETHODIMP_ (ULONG) CTestBufNotify::AddRef (void)
{
	// normally this increases a reference count, but this object
	// is going to be freed as soon as the app is freed, so it doesn't
	// matter
	return 1;
}

STDMETHODIMP_(ULONG) CTestBufNotify::Release (void)
{
	// normally this releases a reference count, but this object
	// is going to be freed when the application is freed so it doesnt
	// matter
	return 1;
}

STDMETHODIMP CTestBufNotify::BookMark (QWORD qTimeStamp, DWORD dwMarkNum)
{
   return NOERROR;
}

STDMETHODIMP CTestBufNotify::TextDataDone (QWORD qTimeStamp, DWORD dwFlags)
{
   return NOERROR;
}

STDMETHODIMP CTestBufNotify::TextDataStarted (QWORD qTimeStamp)
{
   return NOERROR;
}

STDMETHODIMP CTestBufNotify::WordPosition (QWORD qTimeStamp, DWORD dwByteOffset)
{
   return NOERROR;
}
//------------------------------------------------------------------------------
// FindAndSelect - This finds and selects according to the specific TTSMODEINFOW.

static PITTSCENTRALW FindAndSelect(PTTSMODEINFOW pTTSInfo)
{
   HRESULT        hRes;
   TTSMODEINFOW        ttsResult;        // final result
//   WCHAR          Zero = 0;
   PITTSFINDW      pITTSFind;             // find interface
   PIAUDIOMULTIMEDIADEVICE    pIAMM;      // multimedia device interface for audio-dest
   PITTSCENTRALW  pITTSCentral;           // central interface


   hRes = CoCreateInstance(CLSID_TTSEnumerator, NULL, CLSCTX_ALL, IID_ITTSFindW,
                                                   (void**)&pITTSFind);
   if (FAILED(hRes)) return NULL;

   hRes = pITTSFind->Find(pTTSInfo, NULL, &ttsResult);

   if (hRes)
      {
      pITTSFind->Release();
      return NULL;     // error
      }


   // Get the audio dest
   hRes = CoCreateInstance(CLSID_MMAudioDest, NULL, CLSCTX_ALL, IID_IAudioMultiMediaDevice,
                                                   (void**)&pIAMM);
   if (hRes)
      {
      pITTSFind->Release();
      return NULL;     // error
      }
	pIAMM->DeviceNumSet (WAVE_MAPPER);

	// Pass off the multi-media-device interface as an IUnknown (since it is one)
   // Should do select now

   hRes = pITTSFind->Select(ttsResult.gModeID, &pITTSCentral, (LPUNKNOWN) pIAMM);

   if (hRes) {
      pITTSFind->Release();
      return NULL;
      };

   // free random stuff up

   pITTSFind->Release();

   return pITTSCentral;
}
//------------------------------------------------------------------------------
// BeginOLE - This begins the OLE.

static BOOL BeginOLE()
{
   DWORD    dwVer;

   // Initialize OLE

   SetMessageQueue(96);
   dwVer = CoBuildVersion();

   if (rmm != HIWORD(dwVer)) return FALSE;         // error

   if (FAILED(CoInitialize(NULL))) return FALSE;

   return TRUE;
}
//------------------------------------------------------------------------------
// EndOLE - This closes up the OLE.

static BOOL EndOLE()
{
   // Free up all of OLE
   CoUninitialize ();

   return TRUE;
}

#endif  // ifdef TTS
//******************************************************************************
//extern "C" {

void __export __pascal SoundMngPlayBackground(const char* szFilename, int nPriority)
{
	if (!SoundManager)
    	SoundManager = new TSoundManager(Application);

	TSoundRecord* Sr = new TSoundRecord;
	if (szFilename) {
		if (!SoundManager->SoundEnabled)
			return;

		Sr->SetWaveFile(szFilename);
		Sr->nPriority = nPriority;
    }
    else
		Sr->nPriority = 0;

	SoundManager->SetBackground(Sr);
}
//------------------------------------------------------------------------------
void __export __pascal SoundMngPlayWave(const char* szFilename, int nRepCount)
{
	if (!SoundManager)
    	SoundManager = new TSoundManager(Application);

    if (!SoundManager->SoundEnabled)
        return;

	TSoundRecord* Sr = new TSoundRecord;
    Sr->SetWaveFile(szFilename);
	Sr->SetRepeat(nRepCount, 1000);

	SoundManager->PutSound(Sr);
}
//------------------------------------------------------------------------------
void __export __pascal SoundMngSpeak(const char* szText, int nRepCount)
{
	if (!SoundManager)
		SoundManager = new TSoundManager(Application);

	if (!SoundManager->SoundEnabled)
		return;

	TSoundRecord* Sr = new TSoundRecord;
	Sr->SetText(szText);
	Sr->SetRepeat(nRepCount, 1000);

	SoundManager->PutSound(Sr);
}

// } // extern "C"
//------------------------------------------------------------------------------
void __fastcall TSoundManager::Button1Click(TObject *Sender)
{
	SoundMngPlayBackground(AnsiString(EditBack->Text).c_str());
}
//---------------------------------------------------------------------------
void __fastcall TSoundManager::Button2Click(TObject *Sender)
{
	int i = 1;
	swscanf(EditRepCount->Text.c_str(), L"%d", &i);

	SoundMngPlayWave(AnsiString(EditWave->Text).c_str(), i);
}
//---------------------------------------------------------------------------
void __fastcall TSoundManager::Button3Click(TObject *Sender)
{
	int i = 1;
	sscanf(AnsiString(EditRepCount->Text).c_str(), "%d", &i);
	SoundMngSpeak(AnsiString(EditText->Text).c_str(), i);
}

void __fastcall TSoundManager::MngTimerTimer(TObject *Sender)
{
    MngTimer->Enabled = false;
	Manage();
    MngTimer->Enabled = true;     
}
//---------------------------------------------------------------------------
void __fastcall TSoundManager::Button4Click(TObject *Sender)
{
	SoundMngPlayBackground(NULL);
}
//---------------------------------------------------------------------------
