{ utility functions for comparing files.

  Copyright (C) 2021 Domingo Galmés dgalmesp@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit lazdiffutils;

{$mode objfpc}{$H+} {$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, IntegerList, SynEdit, Diff2;

type

  TTextDiffFlag2 = (
    tdfIgnoreCase,            // ignore case of letters
    tdfIgnoreEmptyLineChanges,// ignore if empty lines were added or removed
    tdfIgnoreHeadingSpaces,   // ignore spaces at start of line
    tdfIgnoreLineEnds,        // ignore if line end chars differ (e.g. #10 = #13#10)
    tdfIgnoreSpaceCharAmount, // ignore if space chars were added or removed
    // except if all spaces were removed
    tdfIgnoreSpaceChars,      // ignore spaces (newline chars not included)
    tdfIgnoreTrailingSpaces   // ignore spaces at end of line
    );
  TTextDiffFlags2 = set of TTextDiffFlag2;

  IncrementalCRC32 = record
    crc: cardinal;
    bytescount: cardinal;
    procedure Start;
    procedure NextByte(AByte: byte);
    procedure Finalize;
  end;


const
  EMPTY_LINE_HASH = 0;

function HashLine(const ALine: string; AOptions: TTextDiffFlags2 = []): cardinal;

//Line number 1 based as TSynEdit.
procedure FillHashList(AList: TCardinalList; ALines: TStrings; AOptions: TTextDiffFlags2 = [];
  AFirstLine: integer = 1; ALastLine: integer = maxint);

function EditorGetLastColumn(AEditor: TSynEdit; ALineNumber: integer): integer; overload;
procedure EditorDeleteLine(AEditor: TSynEdit; ALineNumber: integer); overload;
procedure EditorInsertLine(AEditor: TSynEdit; ALineNumber: integer; AText: string); overload;
procedure EditorReplaceLine(AEditor: TSynEdit; ALineNumber: integer; ANewText: string); overload;
procedure Compare(ADiff: TDiff; AText0: TStrings; AText1: TStrings; AList0TextHashes, AList1TextHashes: TCardinalList;
  AEditor0: TSynEdit; AEditor1: TSynEdit);

procedure MakeUnifiedPatch(ADiff: TDiff; ALeftLines: TStrings; ARightLines: TStrings;
  APatchLines: TStrings; const ALeftFileName: string; const ARightFilename: string;
  ALeftFileDate: TDateTime = 0; ARigthFileDate: TDateTime = 0; ALeftFirstLineNumber: integer = 1;
  ARightFirstLineNumber: integer = 1);

// Tries to apply the first patch to the editor.
// Only tries the first.
function ApplyUnifiedPatch(AEditor: TSynEdit; APatch: TStrings; AOnlyTest: boolean; out AErrorMsg: string): boolean;

procedure CopyStringList(AFrom: TStrings; ATo: TStrings); overload;
procedure CopyStringList(AFrom: TStrings; ATo: TStrings; AFirstLine: integer; ALastLine: integer); overload;

implementation

uses
  dateutils, crc, strutils;

var
  crc32_Table: PCardinal;

procedure IncrementalCRC32.Start;
begin
  if crc32_Table = nil then
    crc32_Table := get_crc32_table;
  crc := 0 xor $FFFFFFFF;
  bytescount := 0;
end;

procedure IncrementalCRC32.NextByte(AByte: byte);
begin
  crc := crc32_table[(crc xor AByte) and $ff] xor (crc shr 8);
  Inc(bytescount);
end;

procedure IncrementalCRC32.Finalize;
begin
  if bytescount > 0 then
    crc := crc xor $FFFFFFFF
  else
    crc := EMPTY_LINE_HASH;
end;

function HashLine(const ALine: string; AOptions: TTextDiffFlags2 = []): cardinal;
var
  lLen: integer;
  lstart: integer;
  lLine: string;
  lCrc: IncrementalCRC32;
begin
  lLine := ALine;
  lLen := Length(ALine);
  lstart := 1;

  if tdfIgnoreCase in AOptions then
    lLine := AnsiLowerCase(ALine);

  //tdfIgnoreEmptyLineChanges  Not affect hash.

  if tdfIgnoreLineEnds in AOptions then
  begin
    if (llen > 0) and (lLine[llen] = #10) then
      Dec(lLen);
    if (llen > 0) and (lLine[llen] = #13) then
      Dec(lLen);
  end;

  if tdfIgnoreSpaceChars in AOptions then
  begin
    lCrc.Start;
    while (lstart <= llen) do
    begin
      if not (lLine[lstart] in [' ', #9]) then
        lCrc.NextByte(byte(lLine[lstart]));
      Inc(lstart);
    end;
    lCrc.Finalize;
    Result := lCrc.crc;
    exit;
  end;

  if tdfIgnoreHeadingSpaces in AOptions then
  begin
    while (lstart <= llen) and (lLine[lstart] in [' ', #9]) do
    begin
      Inc(lstart);
    end;
  end;
  if tdfIgnoreTrailingSpaces in AOptions then
  begin
    while (lLen > 0) and (lLine[lLen] in [' ', #9]) do
    begin
      Dec(lLen);
    end;
  end;
  if tdfIgnoreSpaceCharAmount in AOptions then
  begin
    lCrc.Start;
    while (lstart <= llen) do
    begin
      if lLine[lstart] in [' ', #9] then
      begin
        lCrc.NextByte(Ord(' ')); //first space
        Inc(lStart);
        //skip next spaces.
        while (lstart <= llen) and (lLine[lstart] in [' ', #9]) do
          Inc(lStart);
      end
      else
        lCrc.NextByte(byte(lLine[lstart]));
      Inc(lstart);
    end;
    lCrc.Finalize;
    Result := lCrc.crc;
    exit;
  end;
  if (lLen - lstart + 1) <= 0 then
    Result := CRC32(0, nil, 0)
  else
    Result := CRC32(0, pbyte(@lLine[lstart]), lLen - lstart + 1);
end;

//Line number 1 based as TSynEdit.
procedure FillHashList(AList: TCardinalList; ALines: TStrings; AOptions: TTextDiffFlags2 = [];
  AFirstLine: integer = 1; ALastLine: integer = maxint);
var
  i: integer;
begin
  AList.Clear;
  Dec(AFirstLine);
  Dec(ALastLine);
  if AFirstLine < 0 then
    AFirstLine := 0;
  if ALastLine > ALines.Count - 1 then
    ALastLine := ALines.Count - 1;
  for i := AFirstLine to ALastLine do
  begin
    AList.Add(HashLine(ALines[i], AOptions));
  end;
end;

function EditorGetLastColumn(AEditor: TSynEdit; ALineNumber: integer): integer; overload;
var
  lPoint: TPoint;
begin
  lPoint.X := length(AEditor.Lines[ALineNumber - 1]) + 1;
  lPoint.Y := ALineNumber;
  lPoint := AEditor.PhysicalToLogicalPos(lPoint);
  Result := lPoint.X;
end;

procedure EditorDeleteLine(AEditor: TSynEdit; ALineNumber: integer); overload;
var
  lStartPoint, lEndPoint: Tpoint;
begin
  if (ALineNumber < 0) or (ALineNumber > AEditor.Lines.Count) then
    Exit;
  lStartPoint.X := 1;
  lStartPoint.Y := ALineNumber;
  lEndPoint.X := 1;
  lEndPoint.Y := ALineNumber + 1;
  if lEndPoint.Y > AEditor.Lines.Count then
  begin
    lEndPoint.Y := ALineNumber;
    lEndPoint.X := EditorGetLastColumn(AEditor, ALineNumber);
    if ALineNumber > 1 then
    begin
      lStartPoint.X := EditorGetLastColumn(AEditor, ALineNumber - 1);
      lStartPoint.Y := ALineNumber - 1;
    end;
  end;
  AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, '');
end;

procedure EditorInsertLine(AEditor: TSynEdit; ALineNumber: integer; AText: string); overload;
var
  lStartPoint, lEndPoint: Tpoint;
  lColumn: integer;
begin
  if (ALineNumber < 0) or (ALineNumber > AEditor.Lines.Count + 1) then
    Exit;
  if ALineNumber = (AEditor.Lines.Count + 1) then   //append at the end.
  begin
    Dec(ALineNumber);
    lColumn := EditorGetLastColumn(AEditor, ALineNumber);
    lStartPoint.X := lColumn;
    lStartPoint.Y := ALineNumber;
    lEndPoint.X := lColumn;
    lEndPoint.Y := ALineNumber;
    AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, #10 + AText);
  end
  else
  begin                                         //insert
    lStartPoint.X := 1;
    lStartPoint.Y := ALineNumber;
    lEndPoint.X := 1;
    lEndPoint.Y := ALineNumber;
    AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, AText + #10);
  end;
end;

procedure EditorReplaceLine(AEditor: TSynEdit; ALineNumber: integer; ANewText: string); overload;
var
  lStartPoint, lEndPoint: Tpoint;
begin
  if (ALineNumber < 0) or (ALineNumber > AEditor.Lines.Count) then
    Exit;
  lStartPoint.X := 1;
  lStartPoint.Y := ALineNumber;
  lEndPoint.X := 1;
  lEndPoint.Y := ALineNumber;
  lEndPoint.X := EditorGetLastColumn(AEditor, ALineNumber);
  AEditor.SetTextBetweenPoints(lStartPoint, lEndPoint, ANewText);
end;

procedure Compare(ADiff: TDiff; AText0: TStrings; AText1: TStrings; AList0TextHashes, AList1TextHashes: TCardinalList;
  AEditor0: TSynEdit; AEditor1: TSynEdit);
var
  lI: integer;
begin
  try
    ADiff.Execute(AList0TextHashes, AList1TextHashes);
    AEditor0.BeginUpdate;
    AEditor1.BeginUpdate;
    AEditor0.Lines.Clear;
    AEditor1.Lines.Clear;
    for lI := 0 to ADiff.Count - 1 do
    begin
      with ADiff.Compares[lI] do
      begin
        case Kind of
          ckNone:
          begin
            AEditor0.Lines.Add(aText0[oldIndex1]);
            AEditor1.Lines.Add(aText1[oldIndex2]);
          end;
          ckAdd:
          begin
            AEditor0.Lines.Add('');
            AEditor1.Lines.Add(aText1[oldIndex2]);
          end;
          ckDelete:
          begin
            AEditor0.Lines.Add(aText0[oldIndex1]);
            AEditor1.Lines.Add('');
          end;
          ckModify:
          begin
            AEditor0.Lines.Add(aText0[oldIndex1]);
            AEditor1.Lines.Add(aText1[oldIndex2]);
          end;
        end;
      end;
    end;
  finally
    AEditor0.EndUpdate;
    AEditor1.EndUpdate;
  end;
end;

// modified from dateutil.inc
const
  FmtUTC = 'yyyy"-"mm"-"dd" "hh":"nn":"ss"."zzz000000 ';

function DateToUnifiedPatchFormat(const ADate: TDateTime; AInputIsUTC: boolean = True): string;
const
  FmtOffset: string = '%.02d%.02d';
  Sign: array[boolean] of char = ('+', '-');
var
  Offset: integer;
begin
  Result := FormatDateTime(FmtUTC, ADate);
  Offset := GetLocalTimeOffset;//(ADate, AInputIsUTC);
  if AInputIsUTC or (Offset = 0) then
    Result := Result + 'Z'
  else
  begin
    Result := Result + Sign[Offset > 0];
    Offset := Abs(Offset);
    Result := Result + Format(FmtOffset, [Offset div MinsPerHour, Offset mod MinsPerHour]);
  end;
end;

procedure MakeUnifiedPatch(ADiff: TDiff; ALeftLines: TStrings; ARightLines: TStrings;
  APatchLines: TStrings; const ALeftFileName: string; const ARightFilename: string;
  ALeftFileDate: TDateTime = 0; ARigthFileDate: TDateTime = 0; ALeftFirstLineNumber: integer = 1;
  ARightFirstLineNumber: integer = 1);
const
  CONTEXT_LINES = 2;
  NEW_HUNK_LINES = 8;
var
  LineIndex, wJ: integer;
  DiffFound: boolean;
  StartLine: integer;
  CountContext: integer;
  HunkHeaderIndex: integer;
  PushLineIndex: integer;
  PushedLineIndexModify: integer;
  LeftLineStart, RightLineStart, LeftLinesCount, RightLinesCount: integer;
  NewHunk: boolean;
  LeftDateStr, RightDateStr: string;
  IgnoreEmptyLine: boolean;

  procedure SetInitialLines(ALeftLine: integer; ARightLine: integer);
  begin
    if (ALeftLine >= 0) and (LeftLineStart < 0) then
      LeftLineStart := ALeftLine + 1;
    if (ARightLine >= 0) and (RightLineStart < 0) then
      RightLineStart := ARightLine + 1;
  end;

  procedure TestNoNewLineAtEndOfFile(AStrings: TStrings; AIndex: integer);
  begin
    if AIndex = AStrings.Count - 1 then
    begin
      if AStrings[AIndex] <> '' then
        APatchLines.Add('\ no new line at end of file');    //don't translate
    end;
  end;

  procedure AddPatchLine(const APrefix: string; const Astrings: TStrings; AIndex: integer);
  begin
    APatchLines.Add(APrefix + AStrings[AIndex]);
    TestNoNewLineAtEndOfFile(AStrings, AIndex);
  end;

begin
  Dec(ALeftFirstLineNumber); // 0 offset
  Dec(ARightFirstLineNumber);
  APatchLines.Clear;
  if (ADiff.DiffStats.adds = 0) and (ADiff.DiffStats.deletes = 0) and (ADiff.DiffStats.modifies = 0) then
    Exit;
  LeftDateStr := '';
  RightDateStr := '';
  if ALeftFileDate <> 0 then
    LeftDateStr := DateToUnifiedPatchFormat(ALeftFileDate, False);
  if ARigthFileDate <> 0 then
    RightDateStr := DateToUnifiedPatchFormat(ARigthFileDate, False);
  APatchLines.Add('--- ' + ALeftFileName + ' ' + LeftDateStr);
  APatchLines.Add('+++ ' + ARightFilename + ' ' + RightDateStr);
  // find blocks with differences
  LineIndex := 0;
  StartLine := 0;
  while (LineIndex < ADiff.Count) do
  begin
    DiffFound := False;
    while (LineIndex < ADiff.Count) and (DiffFound = False) do
    begin
      if ADiff.Compares[LineIndex].Kind <> ckNone then
      begin
        DiffFound := True;
        break;
      end;
      Inc(LineIndex);
    end;
    if DiffFound = False then
      break;
    // move back context lines
    wJ := LineIndex - 1;
    CountContext := 0;
    while (wJ >= StartLine) and (ADiff.Compares[wJ].Kind = ckNone) and (CountContext < CONTEXT_LINES) do
    begin
      Inc(CountContext);
      Dec(wJ);
    end;
    LineIndex := wJ + 1;
    HunkHeaderIndex := APatchLines.Count;
    APatchLines.Add('@@ -ll,rl +lr,rr @@'); // < updated later at end of hunk.
    LeftLineStart := -1;
    LeftLinesCount := 0;
    RightLineStart := -1;
    RightLinesCount := 0;
    // Add context lines
    for wJ := 1 to CountContext do
    begin
      AddPatchLine(' ', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
      SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
        ADiff.Compares[LineIndex].oldIndex2);
      Inc(LineIndex);
      Inc(LeftLinesCount);
      Inc(RightLinesCount);
    end;
    while (LineIndex < ADiff.Count) do
    begin
      //Add changed lines
      while (LineIndex < ADiff.Count) and (ADiff.Compares[LineIndex].Kind <> ckNone) do
      begin
        case ADiff.Compares[LineIndex].Kind of
          //ckNone: ;
          ckAdd:
          begin
            AddPatchLine('+', ARightLines, ADiff.Compares[LineIndex].oldIndex2);
            SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
              ADiff.Compares[LineIndex].oldIndex2);
            Inc(RightLinesCount);
          end;
          ckDelete:
          begin
            AddPatchLine('-', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
            SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
              ADiff.Compares[LineIndex].oldIndex2);
            Inc(LeftLinesCount);
          end;
          ckModify:
          begin
          {$DEFINE FIRSTDELETEALLMODIFIEDLINES}
          {$IFDEF FIRSTDELETEALLMODIFIEDLINES}
            PushedLineIndexModify := LineIndex;
            while (LineIndex < ADiff.Count) and (ADiff.Compares[LineIndex].Kind = ckModify) do
            begin
              AddPatchLine('-', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
              SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
                ADiff.Compares[LineIndex].oldIndex2);
              Inc(LineIndex);
            end;
            LineIndex := PushedLineIndexModify;
            while (LineIndex < ADiff.Count) and (ADiff.Compares[LineIndex].Kind = ckModify) do
            begin
              AddPatchLine('+', ARightLines, ADiff.Compares[LineIndex].oldIndex2);
              SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
                ADiff.Compares[LineIndex].oldIndex2);
              Inc(LeftLinesCount);
              Inc(RightLinesCount);
              Inc(LineIndex);
            end;
            if LineIndex > PushedLineIndexModify then
              Dec(LineIndex);
          {$ELSE}
            AddPatchLine('-', LeftLines, Diff.Compares[LineIndex].oldIndex1);
            AddPatchLine('+', RightLines, Diff.Compares[LineIndex].oldIndex2);
            SetInitialLines(Diff.Compares[LineIndex].oldIndex1, Diff.Compares[LineIndex].oldIndex2);
            Inc(LeftLinesCount);
            Inc(RightLinesCount);
          {$ENDIF}
          end;
        end;
        Inc(LineIndex);
      end;
      // Add context lines
      // check if there are less than NEW_HUNK_LINES lines without modifying and add them
      // if there are more, just add the first CONTEXT_LINES and jump to next HUNK.
      PushLineIndex := LineIndex;
      CountContext := 0;
      NewHunk := False;
      while (LineIndex < ADiff.Count) do
      begin
        if ADiff.Compares[LineIndex].Kind = ckNone then
        begin
          Inc(CountContext);
          if CountContext >= NEW_HUNK_LINES then
          begin
            CountContext := CONTEXT_LINES;
            NewHunk := True;
            break;
          end;
        end
        else
        begin
          break;
        end;
        Inc(LineIndex);
      end;
      if (LineIndex >= ADiff.Count) and (ADiff.Compares[ADiff.Count - 1].Kind = ckNone) then
      begin
        CountContext := CONTEXT_LINES;
        NewHunk := True;
      end;
      LineIndex := PushLineIndex;
      for PushLineIndex := 1 to CountContext do
      begin
        if LineIndex >= ADiff.Count then
          break;
        AddPatchLine(' ', ALeftLines, ADiff.Compares[LineIndex].oldIndex1);
        SetInitialLines(ADiff.Compares[LineIndex].oldIndex1,
          ADiff.Compares[LineIndex].oldIndex2);
        Inc(LineIndex);
        Inc(LeftLinesCount);
        Inc(RightLinesCount);
      end;
      if NewHunk then
        break;
    end;
    // Update hunk data.
    if LeftLineStart < 0 then LeftLineStart := 0;
    if RightLineStart < 0 then RightLineStart := 0;
    if LeftLinesCount < 0 then LeftLinesCount := 0;
    if RightLinesCount < 0 then RightLinesCount := 0;
    APatchLines[HunkHeaderIndex] :=
      '@@ -' + IntToStr(LeftLineStart + ALeftFirstLineNumber) + ',' + IntToStr(LeftLinesCount) +
      ' +' + IntToStr(RightLineStart + ARightFirstLineNumber) + ',' + IntToStr(RightLinesCount) + ' @@';
    StartLine := LineIndex;
  end;
end;

// Tries to apply the first patch to the editor.
// Only tries the first.
function ApplyUnifiedPatch(AEditor: TSynEdit; APatch: TStrings; AOnlyTest: boolean; out AErrorMsg: string): boolean;
const
  lisDiffMsgOk = 'Ok';
  lisDiffMsgInvalidFileFormat = 'Invalid patch file format';
  lisDiffMsgLineNotExists = 'Error, line: %d not exists in editor';
  lisDiffMsgLinesDontMatch = 'Error, editor line: %d don''t match: [%s] editor [%s]';
var
  PatchLine: string;
  Index: integer;
  FirstChar: char;
  LineNumber: integer;
  EditorLineCount: integer;
  DeltaLines: integer;

  function GetLineNumber: integer;
  var
    I: integer;
  begin
    Result := -9999;
    I := 4;
    if (I <= length(PatchLine)) and (PatchLine[I] in ['0'..'9']) then
    begin
      Result := Ord(PatchLine[I]) - Ord('0');
      Inc(I);
    end;
    while (I <= length(PatchLine)) and (PatchLine[I] in ['0'..'9']) do
    begin
      Result := 10 * Result + (Ord(PatchLine[I]) - Ord('0'));
      Inc(I);
    end;
  end;

begin
  // first test if we can apply the patch.
  if AOnlyTest = False then
  begin
    if not ApplyUnifiedPatch(AEditor, APatch, True, AErrorMsg) then
    begin
      exit(False);
    end;
  end;
  try
    if not AOnlyTest then
    begin
      AEditor.BeginUpdate;
      AEditor.BeginUndoBlock;
    end;
    AErrorMsg := lisDiffMsgInvalidFileFormat;
    if aPatch.Count < 4 then
      exit(False);
    Index := 0;
    PatchLine := APatch[Index];
    // Skip git header if present.
    if AnsiStartsStr('From ', PatchLine) then
    begin
      while Index < aPatch.Count do
      begin
        Inc(Index);
        PatchLine := APatch[Index];
        if AnsiStartsStr('index', PatchLine) then
        begin
          Inc(Index);
          PatchLine := APatch[Index];
          break;
        end;
      end;
    end;
    // Skip subversion header if present.
    if AnsiStartsStr('Index', PatchLine) then
    begin
      while Index < aPatch.Count do
      begin
        Inc(Index);
        PatchLine := APatch[Index];
        if AnsiStartsStr('=====', PatchLine) then
        begin
          Inc(Index);
          PatchLine := APatch[Index];
          break;
        end;
      end;
    end;
    if aPatch.Count < (Index + 4) then
      exit(False);
    if not AnsiStartsStr('---', PatchLine) then
      exit(False);
    Inc(Index);
    PatchLine := APatch[Index];
    if not AnsiStartsStr('+++', PatchLine) then
      exit(False);
    Inc(Index);
    PatchLine := APatch[Index];
    if Copy(PatchLine, 1, 4) <> '@@ -' then
      exit(False);
    EditorLineCount := AEditor.Lines.Count;
    DeltaLines := 0;
    while Index < APatch.Count do
    begin
      PatchLine := APatch[Index];
      if length(PatchLine) < 1 then   // empty line.
      begin
        FirstChar := ' ';
        PatchLine := '';
      end
      else
      begin
        FirstChar := PatchLine[1];
        PatchLine := Copy(PatchLine, 2, Length(PatchLine) - 1);
      end;
      case FirstChar of
        '@':
        begin
          LineNumber := GetLineNumber;
          if LineNumber = -9999 then
            exit(False);
        end;
        ' ':
        begin  //compare if lines are equal.
          if LineNumber > EditorLineCount then
          begin
            AErrorMsg := Format(lisDiffMsgLineNotExists, [LineNumber]);
            exit(False);
          end;
          if PatchLine <> AEditor.Lines[LineNumber + DeltaLines - 1] then
          begin
            AErrorMsg := Format(lisDiffMsgLinesDontMatch, [LineNumber, PatchLine,
              AEditor.Lines[LineNumber + DeltaLines - 1]]);
            exit(False);
          end;
          Inc(LineNumber);
        end;
        '+':
        begin
          if not AOnlyTest then
          begin
            EditorInsertLine(aEditor, LineNumber + DeltaLines, PatchLine);
            Inc(DeltaLines);
          end;
        end;
        '-':
        begin
          if not AOnlyTest then
          begin
            EditorDeleteLine(aEditor, LineNumber + DeltaLines);
            Dec(DeltaLines);
          end;
          Inc(LineNumber);
        end;
        '\':
        begin
        end;
        'd':  // 'diff' next patch in the file git
        begin
          break;
        end;
        'I', '=':  // 'Index' next patch in the file subversion
        begin
          break;
        end;
        else
          exit(False);
      end;
      Inc(Index);
    end;
  finally
    if not AOnlyTest then
    begin
      AEditor.EndUndoBlock;
      AEditor.EndUpdate;
    end;
  end;
  AErrorMsg := lisDiffMsgOk;
  exit(True);
end;

// faster than aTo.Text:=aFrom.Text
procedure CopyStringList(AFrom: TStrings; ATo: TStrings); overload;
var
  lI: integer;
begin
  ATo.Clear;
  ATo.Capacity := AFrom.Count;
  for lI := 0 to AFrom.Count - 1 do
  begin
    ATo.Add(AFrom[lI]);
  end;
end;

procedure CopyStringList(AFrom: TStrings; ATo: TStrings; AFirstLine: integer; ALastLine: integer); overload;
var
  lI: integer;
begin
  ATo.Clear;
  ATo.Capacity := ALastLine - AFirstLine + 1;
  for lI := AFirstLine to ALastLine do
    ATo.Add(AFrom[lI]);
end;

end.
