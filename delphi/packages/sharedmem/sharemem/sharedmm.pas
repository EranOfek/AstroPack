unit sharedmm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;
  {

   ---------------------------------------------------------------------
   TSharedmemory 1.1 for 32-bit Delphi.

   Written 1996 by Arthur Hoornweg (hoornweg@hannover.sgh-net.de)

   This little component allows applications to share a block of memory.
   It is the preferred method to exchange data between 32-bit Applications.
   It is freeware; If it proves useful to you, drop me a mail.
   --------------------------------------------------------------------

   Platforms: WIN95 and NT.

   Properties:
                Blockname
                   -This is a unique name that identifies the memory block.
                    No backslashes, colons etc. allowed;
                    it is NOT a "file" name.

                Blocksize:
                    -The size of the block (should be kept
                     constant for all applications accessing a given
                     shared memory block)

                Memory:
                      -gives you a pointer to the shared memory block!


                BlockExists
                      -FALSE if you have just created the first instance
                       of the block, TRUE otherwise. This property can
                       be used to determine if a data server resides in
                       memory....

  }

  




type
  TSharedMemory = class(TComponent)
  private
    { Private-Deklarationen }
    FMappingHandle :THandle;
    FMemory        :Pointer;
    Fsize          :Longint;
    fblockexists   :boolean;
    Fname          :String;
  protected
    { Protected-Deklarationen }
    Procedure   Alloc;
    Procedure   Dealloc;
    Procedure   setfSize  (size:Longint);
    Procedure   Setfname  (const name:String);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;

    property    Memory    :Pointer read FMemory;

  published
    { Published-Deklarationen }

    Property    BlockSize :Longint read fsize write setfSize default 0;
    Property    BlockName :String read fname write setfname;
    Property    BlockExists : Boolean read fblockexists;
  end;


procedure Register;

implementation

{$R sharedmm.DCR}

    Procedure tsharedmemory.setfSize(size:Longint);
    begin
       Dealloc;
       Fsize:=size;
       Alloc;
    end;


    Procedure   Tsharedmemory.Setfname  (const name:String);
    begin
     Dealloc;
     fname:=name;
     Alloc;
    end;


    Procedure   TSharedmemory.Dealloc;
    begin
      if csdesigning in componentstate then exit;

      fblockexists:=False;


      if ( FMemory <> Nil ) then
      begin
         if not UnmapViewOfFile( FMemory )
           then raise Exception.Create( 'TFilemapping unmapview error');
         FMemory := Nil;
       end;

     if ( FMappingHandle <> 0 ) then
       if not CloseHandle( FMappingHandle )
         then raise Exception.Create('TFileMapping close handle error' );
    end;



constructor Tsharedmemory.Create(AOwner: TComponent);
begin
  inherited create(aowner);
  fmemory:=NIL;
  fmappingHandle:=0;
  fsize:=0;
  fblockexists:=False;
  fname:='';
end;








procedure   TSharedMemory.alloc;
VAR i:integer;
begin
   if csdesigning in componentstate then exit;
   if (fsize=0) or (fname='') then exit;

  FMappingHandle :=
     CreateFileMapping(
       $FFFFFFFF, {to virtual memory}
       nil,
       page_readwrite,
       0,
       fsize,
       pchar(fName));


  i:=getlasterror;
  fblockExists:=(i=error_already_exists);

  if ( FMappingHandle = 0 )
    then raise Exception.Create( 'TSharedmemory.create handle error' );

  FMemory := MapViewOfFile( FMappingHandle, FILE_MAP_write, 0, 0, 0 );



  if ( FMemory = Nil )
    then raise Exception.Create('Tfilemapping map view error' );
end;


destructor TSharedMemory.Destroy;
begin
  {Dealloc;}
  inherited Destroy;
end;


procedure Register;
begin
  RegisterComponents('MULTI', [TSharedMemory]);
end;

end.
