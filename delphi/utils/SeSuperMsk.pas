(*

Component name...................: SuperMask (supermsk.pas)
Classes implemented..............: none
Version..........................: 1.1
Status...........................: Beta
Last update......................: 2001-09-04
Author...........................: Marcello 'Panda' Tavares
Homepage.........................: none
Comments, bugs, suggestions to...: mycelo@yahoo.com
Language.........................: English
Platform (tested)................: Windows 95/98/98SE/2000
Requires.........................: Borland Delphi 5 Standard or better


Features
--------

1. Matching of wildcard strings, similar to Unix regular expressions;
2. Plenty of wildcards, variable-length sets, OR sequences;
3. No classes, no pointers nor special types, just one function!

How to use
----------

SuperMatch(source-text, wildcard-string) -> Boolean

Supported wildcards are:

  1. Simple
  ---------

    ? - 1 any character
    _ - 0 or 1 any character
    * - 0 or more characters
    % - 1 or more characters
    # - 1 or more digits ('0'...'9')

  2. Sets
  -------

    [<occurrences>:][!]<starting_char>[<ending_char>][;[!]<starting_char>[<ending_char>]]...

    Examples:

    [a;b]       - one mandatory occurrence of 'a' or 'b'
    [az]        - characters 'a','b','c'...'z'
    [az;09]     - characters 'a'...'z' or '0'...'9'
    [az;!mo]    - characters 'a'...'z' except 'm','n','o'
    [!ac]       - any character except 'a','b','c'
    [6:az;09]   - 6 occurrences (e.g. '9c3ax7')
    [2-5:az;09] - 2 to 5 occurrences
    [:az]       - 0 or 1 occurrence
    [*:az;09]   - 0 or more occurrences
    [%:az;09]   - 1 or more occurrences

  3. OR sequences
  ---------------

    <mask_string>;<mask_string>[;<mask_string>]...

    Examples:

    {xyz;abcd;123}   - word 'xyz' or 'abcd' or '123'
    {[3:09];-[2:09]} - 3 digits or '-' and 2 digits
    {a*;*0}          - something starting with 'a' or ending with '0'

  4. Escapes
  ----------

    bypass next character (next character will not be seen as a wildcard)

    Examples:

    \*      - character '*'
    \\      - character '\'
    \s      - character space (#$20)
    \t      - character tab (#$9)
    \$41    - character 'A' (#$41) - hex must have 2 digits
    [\[;\]] - character '[' or ']'


License stuff
-------------

SuperMask Copyleft 2001

This software is provided as-is, without any express or implied
warranty. In no event will the author be held liable for any damages
arising from the use of this software.

As a freeware, the author reserve your rights to not provide support,
requested changes in the code, specific versions, improvements of any
kind and bug fixes. The main purpose is to help a little the programmers
community over the world as a whole, not just one person or organization.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented, you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated.

2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being an original software.

3. If you make changes to this software, you must send me the modified
   integral version.

Please, consider my hard work.


What's new in 1.1 version
-------------------------

1.  Better documentation;
2.  Fixed unpredicable behavior on OR sequences.


Author data
-----------

Marcello Roberto Tavares Pereira
mycelo@yahoo.com
http://mpanda.8m.com
ICQ 5831833
Sorocaba/SP - BRAZIL
Spoken languages: Portuguese, English, Spanish

*)

unit SeSuperMsk;

interface

function SuperMatch(Text, Mask: String): Boolean;

implementation

uses Sysutils;

type
  WildcardKind = (tmFixed, tmOne, tmSeveral, tmSet, tmGroup);

  PWildcardRec = ^WildcardRec;
  aWildcard = array of PWildcardRec;

  WildcardRec = record
    Kind: WildcardKind;
    Valids: set of Char;
    Invalids: set of Char;
    Counter: Integer;
    Minimum: Integer;
    Solid: Boolean;
    Sequence: String;
    SubMasks: array of aWildcard;
  end;

function SuperMatch;
var
  Root: aWildcard;
  QtdUsed: Integer;

  // Normalize all escapes in hexa format

  function NormEscape(const Text: String): String;
  var
    Loop: Integer;
    Escape: Boolean;

  begin

    Loop := 1;
    Escape := False;
    Result := '';

    while Loop <= Length(Text) do
    begin

      if (not Escape) and (Text[Loop] = '\') then
      begin

        Escape := True;
        Result := Result + '\';
      end
      else
      begin

        if Escape then
        begin

          case Text[Loop] of

            '$': Result := Result + '$';

            's': Result := Result + '$20';

            't': Result := Result + '$09';

            else
              Result := Result + '$' + Format('%.2x', [Ord(Text[Loop])]);
          end;

          Escape := False;
        end
        else
        begin

          Result := Result + Text[Loop];
        end;
      end;

      Inc(Loop);
    end;
  end;

  // Replace escape with actual character

  function ConvEscape(const Text: String): String;
  var
    Loop: Integer;
    Escape: Boolean;

  begin

    Loop := 1;
    Escape := False;
    Result := '';

    while Loop <= Length(Text) do
    begin

      if (not Escape) and (Text[Loop] = '\') then
      begin

        Escape := True;
      end
      else
      begin

        if Escape and (Text[Loop] = '$')
        and (StrToIntDef(Copy(Text, Loop, 3), -1) >= 0) then
        begin

          Result := Result + Chr(StrToInt(Copy(Text, Loop, 3)));
          Inc(Loop, 2);
        end
        else
        begin

          Result := Result + Text[Loop];
        end;

        Escape := False;
      end;

      Inc(Loop);
    end;
  end;

  // Returns an item from a list

  function Item(Txt: String; Cont: Integer): String;
  var
    Quote: Integer;
    QChar: array [1..100] of Char;
    Loop: Integer;
    ContItem: Integer;
    Jump: Boolean;

  begin

    Result := '';
    ContItem := 1;
    Quote := 0;
    Jump := False;

    for Loop := 1 to Length(Txt) do
    begin

      if Txt[Loop] in ['{', '['] then
      begin

        Inc(Quote);

        case Txt[Loop] of

          '{': QChar[Quote] := '}';
          '[': QChar[Quote] := ']';
        end;
      end
      else
      begin

        if (Quote > 0) and (Txt[Loop] = QChar[Quote]) then
        begin

          Dec(Quote);
        end
        else
        begin

          if (Quote = 0) and (Txt[Loop] = ';') then
          begin

            Inc(ContItem);
            Jump := True;
          end;
        end;
      end;

      if (not Jump) and (ContItem = Cont) then
      begin

        Result := Result + Txt[Loop];
      end;

      Jump := False;
    end;
  end;

  // Create a new wildcard structure

  procedure NewWildcard(var Wildcards: aWildcard);
  begin

    SetLength(Wildcards, High(Wildcards)+2);
    New(Wildcards[High(Wildcards)]);
    Wildcards[High(Wildcards)]^.Valids := [];
    Wildcards[High(Wildcards)]^.Invalids := [];
    Wildcards[High(Wildcards)]^.Counter := 0;
    Wildcards[High(Wildcards)]^.Solid := False;
    Wildcards[High(Wildcards)]^.Sequence := '';
    Wildcards[High(Wildcards)]^.SubMasks := nil;
  end;

  // Identifies a new set []

  procedure NewSet(var Wildcards: aWildcard; Txt: String);
  var
    Loop, nPos: Integer;
    Charac: Byte;
    StrCont: String;
    Range: String;
    Neg: Boolean;

  begin

    if Txt = '' then
      Exit;

    NewWildcard(Wildcards);
    Wildcards[High(Wildcards)]^.Kind := tmSet;
    Wildcards[High(Wildcards)]^.Solid := True;
    Wildcards[High(Wildcards)]^.Counter := 1;
    Wildcards[High(Wildcards)]^.Minimum := 1;

    if Pos(':', Txt) > 0 then
    begin

      Loop := 1;
      StrCont := '';

      while (Txt[Loop] <> ':') and (Loop <= Length(Txt)) do
      begin

        StrCont := StrCont + Txt[Loop];
        Inc(Loop);
      end;

      if StrCont = '' then
      begin

        Wildcards[High(Wildcards)]^.Counter := 1;
        Wildcards[High(Wildcards)]^.Minimum := 0;
        Wildcards[High(Wildcards)]^.Solid := False;
      end
      else
      begin

        case StrCont[1] of

          '%':
          begin

            Wildcards[High(Wildcards)]^.Counter := -1;
            Wildcards[High(Wildcards)]^.Minimum := -1;
            Wildcards[High(Wildcards)]^.Solid := True;
          end;

          '*':
          begin

            Wildcards[High(Wildcards)]^.Counter := -1;
            Wildcards[High(Wildcards)]^.Minimum := -1;
            Wildcards[High(Wildcards)]^.Solid := False;
          end;

          else
          begin

            nPos := Pos('-', StrCont);

            if nPos > 0 then
            begin

              Wildcards[High(Wildcards)]^.Minimum := StrToIntDef(Copy(StrCont, 1, nPos-1), 1);
              Wildcards[High(Wildcards)]^.Counter := StrToIntDef(Copy(StrCont, nPos+1, Length(StrCont)), 1);
            end
            else
            begin

              Wildcards[High(Wildcards)]^.Minimum := StrToIntDef(StrCont, 1);
              Wildcards[High(Wildcards)]^.Counter := StrToIntDef(StrCont, 1);
            end;

            Wildcards[High(Wildcards)]^.Solid := Wildcards[High(Wildcards)]^.Minimum > 0;
          end;
        end;
      end;

      Txt := Copy(Txt, Loop+1, Length(Txt));
    end;

    Loop := 0;

    repeat

      Inc(Loop);
      Range := Item(Txt, Loop);

      if Length(Range) > 0 then
      begin

        Neg := Range[1] = '!';

        if Neg then
          Range := ConvEscape(Copy(Range, 2, Length(Range)-1))
        else
          Range := ConvEscape(Range);

        if Length(Range) = 1 then
        begin

          if Neg then
            Wildcards[High(Wildcards)]^.Invalids := Wildcards[High(Wildcards)]^.Invalids + [Range[1]]
          else
            Wildcards[High(Wildcards)]^.Valids := Wildcards[High(Wildcards)]^.Valids + [Range[1]];
        end;

        if (Length(Range) = 2) and (Ord(Range[1]) < Ord(Range[2])) then
        begin

          for Charac := Ord(Range[1]) to Ord(Range[2]) do
          begin

            if Neg then
              Wildcards[High(Wildcards)]^.Invalids := Wildcards[High(Wildcards)]^.Invalids + [Chr(Charac)]
            else
              Wildcards[High(Wildcards)]^.Valids := Wildcards[High(Wildcards)]^.Valids + [Chr(Charac)];
          end;
        end;
      end;
    until Length(Range) = 0;
  end;

  // Divides the mask in wildcards and sets

  procedure Divide(var Wildcards: aWildcard; Txt: String);
  var
    Loop, Loop2: Integer;
    Wildset: Boolean;
    Group: Integer;
    Str: String;
    SubTxt: String;

  begin

    SetLength(Wildcards, 0);
    Wildset := False;
    Group := 0;
    Str := '';

    for Loop := 1 to Length(Txt)+1 do
    begin

      if Group = 0 then
      begin

        if not Wildset then
        begin

          if ((Loop > Length(Txt))
          or (Txt[Loop] in ['*', '?', '_', '%', '#', '[', '{'])) then
          begin

            if Str <> '' then
            begin

              NewWildcard(Wildcards);

              Wildcards[High(Wildcards)]^.Kind := tmFixed;
              Wildcards[High(Wildcards)]^.Sequence := ConvEscape(Str);
              Wildcards[High(Wildcards)]^.Solid := True;
              Wildcards[High(Wildcards)]^.Counter := Length(Wildcards[High(Wildcards)]^.Sequence);

              Str := '';
            end;

            if Loop <= Length(Txt) then
            begin

              case Txt[Loop] of

                '*':
                begin

                  NewWildcard(Wildcards);

                  Wildcards[High(Wildcards)]^.Kind := tmSeveral;
                  Wildcards[High(Wildcards)]^.Solid := False;
                end;

                '%':
                begin

                  NewWildcard(Wildcards);

                  Wildcards[High(Wildcards)]^.Kind := tmSeveral;
                  Wildcards[High(Wildcards)]^.Solid := True;
                end;

                '#':
                begin

                  NewWildcard(Wildcards);

                  Wildcards[High(Wildcards)]^.Kind := tmSet;
                  Wildcards[High(Wildcards)]^.Solid := True;
                  Wildcards[High(Wildcards)]^.Valids := ['0'..'9'];
                  Wildcards[High(Wildcards)]^.Counter := 1;
                end;

                '?':
                begin

                  NewWildcard(Wildcards);

                  Wildcards[High(Wildcards)]^.Kind := tmOne;
                  Wildcards[High(Wildcards)]^.Solid := True;
                end;

                '_':
                begin

                  NewWildcard(Wildcards);

                  Wildcards[High(Wildcards)]^.Kind := tmOne;
                  Wildcards[High(Wildcards)]^.Solid := False;
                end;

                '[':
                begin

                  Wildset := True;
                end;

                '{':
                begin

                  Inc(Group);
                end;
              end;
            end;
          end  // wildcard
          else
          begin

            Str := Str + Txt[Loop];
          end;
        end  // Wildset
        else
        begin

          if Txt[Loop] = ']' then
          begin

            Wildset := False;
            NewSet(Wildcards, Str);
            Str := '';
          end
          else
          begin

            Str := Str + Txt[Loop];
          end;
        end;
      end  // Group
      else
      begin

        if Txt[Loop] = '}' then
          Dec(Group);

        if Group = 0 then
        begin

          NewWildcard(Wildcards);
          Wildcards[High(Wildcards)]^.Kind := tmGroup;
          Wildcards[High(Wildcards)]^.Solid := True;

          Loop2 := 1;
          SubTxt := Item(Str, Loop2);

          while SubTxt <> '' do
          begin

            SetLength(Wildcards[High(Wildcards)]^.SubMasks, Loop2);
            Divide(Wildcards[High(Wildcards)]^.SubMasks[Loop2-1], SubTxt);
            Inc(Loop2);
            SubTxt := Item(Str, Loop2);
          end;

          Str := '';
        end
        else
        begin

          Str := Str + Txt[Loop];
        end;
      end;
    end; // loop
  end;

  // Recursive wildcard matching

  function Matching(var Wildcards: aWildcard; WCIndex: Integer; Txt: String; var Qtd: Integer; SubMask: Boolean): Boolean;
  var
    Loop: Integer;
    QtdUsed: Integer;

  begin

    if WCIndex > High(Wildcards) then
    begin

      Result := SubMask or (Txt = '');
      Exit;
    end;

    if Txt = '' then
    begin

      Result := (not Wildcards[WCIndex]^.Solid) and Matching(Wildcards, WCIndex+1, '', Qtd, SubMask);
      Exit;
    end;

    Result := False;

    with Wildcards[WCIndex]^ do
    begin

      case Kind of

        tmFixed:
        begin

          Result := (Copy(Txt, 1, Counter) = Sequence) and Matching(Wildcards, WCIndex+1, Copy(Txt, Counter+1, Length(Txt)), Qtd, SubMask);

          if Result then
            Inc(Qtd, Counter);
        end;

        tmOne:
        begin

          Result := Matching(Wildcards, WCIndex+1, Copy(Txt, 2, Length(Txt)), Qtd, SubMask);

          if Result then
            Inc(Qtd);

          if (not Result) and (not Solid) then
          begin

            Result := Matching(Wildcards, WCIndex+1, Txt, Qtd, SubMask);
          end;
        end;

        tmSeveral:
        begin

          if Solid then
            Loop := 2
          else
            Loop := 1;

          Result := False;

          while (not Result) and (Loop <= Length(Txt)+1) do
          begin

            Result := Result or Matching(Wildcards, WCIndex+1, Copy(Txt, Loop, Length(Txt)), Qtd, SubMask);
            Inc(Loop);
          end;

          if Result then
            Inc(Qtd, Loop-2);
        end;

        tmSet:
        begin

          if Solid and (Minimum > Length(Txt)) then
          begin

            Result := False;
          end
          else
          begin

            if Counter > 0 then
            begin

              Loop := 1;
              Result := True;

              while Result and (Loop <= Counter) do
              begin

                if Valids <> [] then
                  Result := Result and (Txt[Loop] in Valids) and (not (Txt[Loop] in Invalids))
                else
                  Result := Result and (not (Txt[Loop] in Invalids));

                if Result then
                  Inc(Loop);
              end;

              if not Result then
                Result := (Loop - 1) >= Minimum;

              Result := Result and Matching(Wildcards, WCIndex+1, Copy(Txt, Loop, Length(Txt)), Qtd, SubMask);
            end
            else
            begin

              Loop := 0;
              Result := True;

              while Result and (Loop <= Length(Txt)) do
              begin

                Inc(Loop);

                if Valids <> [] then
                  Result := Result and (Txt[Loop] in Valids) and (not (Txt[Loop] in Invalids))
                else
                  Result := Result and (not (Txt[Loop] in Invalids));
              end;

              if Solid and (Loop = 1) then
                Result := False
              else
                Result := Matching(Wildcards, WCIndex+1, Copy(Txt, Loop, Length(Txt)), Qtd, SubMask);
            end;

            if Result then
              Inc(Qtd, Loop-1);

            if (not Solid) and (not Result) then
              Result := Matching(Wildcards, WCIndex+1, Copy(Txt, 1, Length(Txt)), Qtd, SubMask);
          end;
        end;

        tmGroup:
        begin

          Result := False;

          for Loop := Low(SubMasks) to High(SubMasks) do
          begin

            QtdUsed := 0;

            if Matching(SubMasks[Loop], 0, Txt, QtdUsed, WCIndex < High(Wildcards)) then
              Result := Matching(Wildcards, WCIndex+1, Copy(Txt, QtdUsed+1, Length(Txt)), Qtd, SubMask);

            if Result then
              Break;
          end;

          if Result then
            Inc(Qtd, QtdUsed);
        end;
      end;
    end;
  end;

  // Release structure memory

  procedure RelMemory(var Wildcards: aWildcard);
  var
    Loop1, Loop2: Integer;

  begin

    for Loop1 := Low(Wildcards) to High(Wildcards) do
    begin

      for Loop2 := Low(Wildcards[Loop1]^.SubMasks) to High(Wildcards[Loop1]^.SubMasks) do
        RelMemory(Wildcards[Loop1]^.SubMasks[Loop2]);

      SetLength(Wildcards[Loop1]^.SubMasks, 0);
      Dispose(Wildcards[Loop1]);
    end;

    SetLength(Wildcards, 0);
  end;

begin

  SetLength(Root, 0);
  QtdUsed := 0;

  // Search for wildcards
  Divide(Root, NormEscape(Mask));

  // Match text with wildcards
  Result := Matching(Root, 0, Text, QtdUsed, False);

  // Release memory
  RelMemory(Root);
end;

end.


