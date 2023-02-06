{
 /***************************************************************************
                              lazdiffform.pas
                              --------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Domingo Galmés
  some code borrowed from diffdialog.pas, author   Mattias Gaertner (www.lazarus-ide.org)

  Abstract:
    The TDiffForm is a form for showing differences between two files.

}
unit lazdiffform;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Buttons, StdCtrls, ExtCtrls, Dialogs, ComCtrls, LCLType,
  // LazUtils
  FileUtil, IntegerList,
  // SynEdit
  SynEdit, SynHighlighterDiff, SynEditTypes, SynHighlighterPas,
  Graphics, SynGutterLineNumber,
  // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEImagesIntf,
  SrcEditorIntf,
  diff2, lazdiffutils,
  Types, SynEditMiscClasses, SynEditMarkupSpecialLine;

const
  DiffFormWindowName = 'IDEDiffCompareFilesWindow';

type

  { TAvailableDiffFile }

  TAvailableDiffFile = class
  private
    Name: string;
    Editor: TSourceEditorInterface;
    SelectionAvailable: boolean;
  public
    constructor Create(const NewName: string; NewEditor: TSourceEditorInterface; NewSelectionAvailable: boolean);
  end;

  { TAvailableDiffFiles }

  TAvailableDiffFiles = class(TList)
  private
    function GetItems(Index: integer): TAvailableDiffFile;
    procedure SetItems(Index: integer; const AValue: TAvailableDiffFile);
  public
    procedure Clear; override;
    function Add(DiffFile: TAvailableDiffFile): integer;
    function IndexOfName(const Name: string): integer;
  public
    property Items[Index: integer]: TAvailableDiffFile read GetItems write SetItems; default;
  end;


  TDiffForm = class;

  { TSelectedDiffFile }

  TSelectedDiffFile = class
  private
    fOwner: TDiffForm;
    fFile: TAvailableDiffFile;        // Selected File
    fCombobox: TComboBox;             // Reference for the user selection GUI.
    fOnlySelCheckBox: TCheckBox;
    procedure GetLines(ALines: TStrings; var aFirstLine: integer; var aFileName: string);
    procedure SetIndex(NewIndex: integer);
    procedure SetFileName(aFileName: string);
    procedure UpdateIndex;
  public
    constructor Create(aOwner: TDiffForm; aCombobox: TComboBox; aOnlySelCheckBox: TCheckBox);
  end;

  { TDiffForm }

  TDiffForm = class(TForm)
    btnFindL: TBitBtn;
    btnFindNextR: TBitBtn;
    btnFindPreviousR: TBitBtn;
    btnFindR: TBitBtn;
    btnFindPreviousL: TBitBtn;
    btnFindNextL: TBitBtn;
    btnFirst: TButton;
    btnCompare: TButton;
    btnLast: TButton;
    btnNext: TButton;
    btnPrevious: TButton;
    CancelScanningButton: TBitBtn;
    DiffSynEdit: TSynEdit;
    edLeft: TSynEdit;
    edRight: TSynEdit;
    HelpButton: TBitBtn;
    CloseButton: TBitBtn;
    lbLeftEdit: TLabel;
    lbRightEdit: TLabel;
    OpenInEditorButton: TBitBtn;
    OptionsGroupBox: TCheckGroup;
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    ProgressBar1: TProgressBar;
    SynDiffSyn1: TSynDiffSyn;
    dlgOpen: TOpenDialog;
    SynFreePascalSyn1: TSynFreePascalSyn;
    tbsOptions: TTabSheet;
    tbsCompare: TTabSheet;
    tbsPatch: TTabSheet;
    Text1Combobox: TComboBox;
    Text1FileOpenButton: TButton;
    Text1GroupBox: TGroupBox;
    Text1OnlySelectionCheckBox: TCheckBox;
    Text2Combobox: TComboBox;
    Text2FileOpenButton: TButton;
    Text2GroupBox: TGroupBox;
    Text2OnlySelectionCheckBox: TCheckBox;

    procedure btnCompareClick(Sender: TObject);
    procedure btnFindLClick(Sender: TObject);
    procedure btnFindNextLClick(Sender: TObject);
    procedure btnFindNextRClick(Sender: TObject);
    procedure btnFindPreviousLClick(Sender: TObject);
    procedure btnFindPreviousRClick(Sender: TObject);
    procedure btnFindRClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnpreviousClick(Sender: TObject);
    procedure CancelScanningButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure OnSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure edLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure edRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FileOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OnChangeFlag(Sender: TObject);
    procedure LeftSynGutterLineNumber1FormatLineNumber(Sender: TSynGutterLineNumber;
      ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
    procedure OpenInEditorButtonClick(Sender: TObject);
    procedure RightSynGutterLineNumber1FormatLineNumber(Sender: TSynGutterLineNumber;
      ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
    procedure tbsCompareResize(Sender: TObject);
    procedure Text1ComboboxChange(Sender: TObject);
    procedure Text2ComboboxChange(Sender: TObject);
  private
    fUpdating: boolean;
    fAvailableFiles: TAvailableDiffFiles;
    fSelectedFile1: TSelectedDiffFile;
    fSelectedFile2: TSelectedDiffFile;
    fDiff: TDiff;
    fLeftLines: TStringList;
    fRightLines: TStringList;
    fLeftFirstLineNumber: integer;
    fRightFirstLineNumber: integer;
    fLeftTextHashes: TCardinalList;
    fRightTextHashes: TCardinalList;
    fCurrentOptions: TTextDiffFlags2;

    fRightSearchOptions: TSynSearchOptions;
    fRightSearchText: string;
    fLeftSearchOptions: TSynSearchOptions;
    fLeftSearchText: string;

    fFile1Index: integer;
    procedure SetupComponents;
    procedure UpdateDiff;
    function GetNomalizedKindType(aLineIndex: integer): TChangeKind;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure FillTextComboBoxes;
    procedure SaveSettings;
    procedure LoadSettings;
    function GetDiffOptions: TTextDiffFlags2;
  end;

procedure ShowDiffDialog;
procedure CreateIDEDiffForm(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);

const
  IgnoreCaseCheckBox = 0;
  IgnoreEmptyLineChangesCheckBox = 1;
  IgnoreHeadingSpacesCheckBox = 2;
  IgnoreSpaceCharAmountCheckBox = 3;
  IgnoreSpaceCharsCheckBox = 4;
  IgnoreTrailingSpacesCheckBox = 5;
//IgnoreLineEndsCheckBox = 6;          //we use stringlist to read files and end of lines are lost.

implementation

{$R *.lfm}

uses
  LCLIntf, dateutils
  , FindReplaceDialog2
  , lazdiffUIConsts, LazConfigStorage, BaseIDEIntf, IDEMsgIntf, IDEExternToolIntf, lazideintf, projectintf;

var
  gWindowCount: integer = 0;

procedure CreateIDEDiffForm(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
var
  lForm: TDiffForm;
begin
  if CompareText(aFormName, DiffFormWindowName) <> 0 then exit;
  lForm := nil;
  IDEWindowCreators.CreateForm(lForm, TDiffForm, DoDisableAutoSizing, LazarusIDE.OwningComponent);
  Inc(gWindowCount);
  //Name required for docking
  lForm.Name := DiffFormWindowName + IntToStr(gWindowCount);   // to allow multiple compare windows name must be diferent.
  AForm := lForm;
end;


procedure ShowDiffDialog;
begin
  IDEWindowCreators.ShowForm(DiffFormWindowName, True);
end;

{ TSelectedDiffFile }

constructor TSelectedDiffFile.Create(aOwner: TDiffForm; aCombobox: TComboBox; aOnlySelCheckBox: TCheckBox);
begin
  inherited Create;
  fOwner := aOwner;
  fCombobox := aCombobox;
  fOnlySelCheckBox := aOnlySelCheckBox;
end;

procedure TSelectedDiffFile.GetLines(ALines: TStrings; var aFirstLine: integer; var aFileName: string);
begin
  aFirstLine := 1;
  aFileName := '';
  if fFile = nil then Exit;
  if fFile.Editor = nil then
  begin
    ALines.LoadFromFile(fFile.Name);
    AFileName := fFile.Name;
  end
  else
  begin
    AFileName := fFile.Editor.FileName;
    if (fFile.SelectionAvailable and fOnlySelCheckBox.Checked) then
    begin
      aFirstLine := fFile.Editor.BlockBegin.Y;
      //ALines.Text := fFile.Editor.EditorComponent.SelText;
      CopyStringList(fFile.Editor.Lines, ALines, aFirstLine - 1,
        fFile.Editor.BlockEnd.Y - 1);
    end
    else
      //ALines.Text := fFile.Editor.EditorComponent.Lines.Text;
      CopyStringList(fFile.Editor.Lines, ALines);
  end;
end;

procedure TSelectedDiffFile.SetIndex(NewIndex: integer);
var
  OldFile: TAvailableDiffFile;
begin
  OldFile := fFile;
  if (NewIndex >= 0) and (NewIndex < fOwner.fAvailableFiles.Count) then
  begin
    fFile := fOwner.fAvailableFiles[NewIndex];
    fCombobox.Text := fFile.Name;
    fOnlySelCheckBox.Enabled := fFile.SelectionAvailable;
  end
  else
  begin
    fFile := nil;
    fCombobox.Text := '';
    fOnlySelCheckBox.Enabled := False;
  end;
  if fFile <> OldFile then fOwner.UpdateDiff;
end;

procedure TSelectedDiffFile.SetFileName(aFileName: string);
// Assumes that aFileName is already in fCombobox.Items.
begin
  fCombobox.ItemIndex := fCombobox.Items.IndexOf(aFileName);
  SetIndex(fCombobox.Items.IndexOf(aFileName));
end;

procedure TSelectedDiffFile.UpdateIndex;
begin
  SetIndex(fCombobox.Items.IndexOf(fCombobox.Text));
end;

{ TDiffForm }

constructor TDiffForm.Create(TheOwner: TComponent);
var
  i: integer;
  SrcEdit: TSourceEditorInterface;
  ActiveEdit: TSourceEditorInterface;
begin
  inherited Create(TheOwner);
  fUpdating := False;
  fSelectedFile1 := TSelectedDiffFile.Create(Self, Text1Combobox, Text1OnlySelectionCheckBox);
  fSelectedFile2 := TSelectedDiffFile.Create(Self, Text2Combobox, Text2OnlySelectionCheckBox);
  if gWindowCount>0 then
    Caption:=lisCaptionCompareFiles2+' '+IntToStr(gWindowCount+1)
  else
    Caption := lisCaptionCompareFiles2;
  //  IDEDialogLayoutList.ApplyLayout(Self, 600, 500);
  SetupComponents;
  fDiff := TDiff.Create(nil);
  fLeftLines := TStringList.Create;
  fRightLines := TStringList.Create;
  fLeftTextHashes := TCardinalList.Create;
  fRightTextHashes := TCardinalList.Create;
  fAvailableFiles := TAvailableDiffFiles.Create;
  ActiveEdit := SourceEditorManagerIntf.ActiveEditor;
  fFile1Index := 0;
  // Get available files
  for i := 0 to SourceEditorManagerIntf.SourceEditorCount - 1 do
  begin
    SrcEdit := SourceEditorManagerIntf.SourceEditors[i];
    fAvailableFiles.Add(TAvailableDiffFile.Create(SrcEdit.PageName, SrcEdit, SrcEdit.SelectionAvailable));
    if ActiveEdit = SrcEdit then
      fFile1Index := i;
  end;
  Init;
end;

destructor TDiffForm.Destroy;
begin
  SaveSettings;
  fSelectedFile2.Free;
  fSelectedFile1.Free;
  fDiff.Free;
  fLeftLines.Free;
  fRightLines.Free;
  fLeftTextHashes.Free;
  fRightTextHashes.Free;
  fAvailableFiles.Free;
  inherited Destroy;
end;

procedure TDiffForm.OnChangeFlag(Sender: TObject);
begin
  UpdateDiff;
end;

function MyShowDot(Sender: TSynGutterLineNumber; AEditor: TSynEdit; ALine: integer): boolean;
begin
  Result := ((ALine mod Sender.ShowOnlyLineNumbersMultiplesOf) <> 0) and (ALine <> AEditor.CaretY) and
    (ALine <> 1) and (ALine <> AEditor.Lines.Count);
end;

procedure TDiffForm.LeftSynGutterLineNumber1FormatLineNumber(Sender: TSynGutterLineNumber;
  ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
begin
  if (FDiff.Count > 0) and (aLine > 0) and (aLine <= FDiff.Count) then
  begin
    if FDiff.Compares[aLine - 1].Kind = ckAdd then
    begin
      aText := '+';
      Exit;
    end
    else
      aLine := FDiff.Compares[aLine - 1].oldIndex1 + fLeftFirstLineNumber;
    aText := Sender.FormatLineNumber(aLine, MyShowDot(Sender, edLeft, ALine));
  end;
end;

procedure TDiffForm.OpenInEditorButtonClick(Sender: TObject);
begin
  //NewDiffFilename:=CreateSrcEditPageName('','FileDifference.diff', nil);
  LazarusIDE.DoNewEditorFile(FileDescriptorText, 'FileDifference.diff', DiffSynEdit.Text,
    [nfOpenInEditor, nfIsNotPartOfProject]);
end;

procedure TDiffForm.RightSynGutterLineNumber1FormatLineNumber(Sender: TSynGutterLineNumber;
  ALine: integer; out AText: string; const ALineInfo: TSynEditGutterLineInfo);
begin
  if (FDiff.Count > 0) and (aLine > 0) and (aLine <= FDiff.Count) then
  begin
    if FDiff.Compares[aLine - 1].Kind = ckDelete then
    begin
      aText := '-';
      Exit;
    end
    else
      aLine := FDiff.Compares[aLine - 1].oldIndex2 + fRightFirstLineNumber;
    aText := Sender.FormatLineNumber(aLine, MyShowDot(Sender, edRight, ALine));
  end;
end;

procedure TDiffForm.tbsCompareResize(Sender: TObject);
const
  MARGIN = 5;
  SEPARATION = 10;
var
  lWidth: integer;
begin
  lWidth := (TTabSheet(Sender).Width - MARGIN - MARGIN - SEPARATION) div 2;
  edLeft.Width := lWidth;
  edRight.Left := edLeft.Left + lWidth + SEPARATION;
  edRight.Width := lWidth;
  //center buttons.
  PanelButtons.Left := (TTabSheet(Sender).Width div 2) - (PanelButtons.Width div 2);
  lbLeftEdit.Width:=lWidth;
  lbRightEdit.Width:=lWidth;
  lbRightEdit.Left:=edRight.Left;
end;

procedure TDiffForm.FileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    //only add new files
    if Text1ComboBox.Items.IndexOf(dlgOpen.FileName) = -1 then
    begin
      fAvailableFiles.Add(TAvailableDiffFile.Create(dlgOpen.FileName, nil, False));
      Text1ComboBox.Items.Add(dlgOpen.FileName);
      Text2ComboBox.Items.Add(dlgOpen.FileName);
    end;
    //set the combobox and make the diff
    if TButton(Sender) = Text1FileOpenButton then
      fSelectedFile1.SetFileName(dlgOpen.FileName)
    else
      fSelectedFile2.SetFileName(dlgOpen.FileName);
  end;
end;

procedure TDiffForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDiffForm.CancelScanningButtonClick(Sender: TObject);
begin
  fDiff.Cancel;
end;

procedure TDiffForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TDiffForm.btnCompareClick(Sender: TObject);
var
  FileNameLeft, FileNameRight: string;
  Temp:string;
  Digits: integer;
const
  TAB_COMPARE_INDEX = 1;
begin
  if fUpdating then Exit;
  fUpdating := True;
  DiffSynEdit.Lines.Clear;
  fSelectedFile1.GetLines(fLeftLines, fLeftFirstLineNumber, FileNameLeft);
  fSelectedFile2.GetLines(fRightLines, fRightFirstLineNumber, FileNameRight);
  if fSelectedFile1.fFile.Editor<>nil then
    Temp:='[Editor] '
  else
    Temp:='[File] ';
  lbLeftEdit.Caption:=Temp+FileNameLeft;
  if fSelectedFile2.fFile.Editor<>nil then
    Temp:='[Editor] '
  else
    Temp:='[File] ';
  lbRightEdit.Caption:=Temp+FileNameRight;
  lbLeftEdit.Hint:=FileNameLeft;
  lbRightEdit.Hint:=FileNameRight;
  fCurrentOptions := GetDiffOptions;
  if (fLeftLines.Count > 0) and (fRightLines.Count > 0) then
  begin
    Text1GroupBox.Enabled := False;
    Text2GroupBox.Enabled := False;
    OpenInEditorButton.Enabled := False;
    CancelScanningButton.Enabled := True;
    CancelScanningButton.Visible := True;
    ProgressBar1.Style := pbstMarquee;
    FillHashList(fLeftTextHashes, fLeftLines, fCurrentOptions);
    FillHashList(fRightTextHashes, fRightLines, fCurrentOptions);
    Compare(fDiff, fLeftLines, fRightLines, fLeftTextHashes, fRightTextHashes,
      edLeft, edRight);
    if not fDiff.Cancelled then
    begin
      MakeUnifiedPatch(fDiff, fLeftLines, fRightLines, DiffSynEdit.Lines,
        ExtractFileName(FileNameLeft), ExtractFileName(FileNameRight), 0,
        0, fLeftFirstLineNumber, fRightFirstLineNumber);
      //adjust gutter sizes.
      Digits := fDiff.Count + fLeftFirstLineNumber;
      TSynGutterLineNumber(edLeft.Gutter.Parts[1]).DigitCount :=
        length(IntToStr(Digits));
      Digits := fDiff.Count + fRightFirstLineNumber;
      TSynGutterLineNumber(edRight.Gutter.Parts[1]).DigitCount :=
        length(IntToStr(Digits));
      ProgressBar1.Style := pbstNormal;
      PageControl1.TabIndex := TAB_COMPARE_INDEX;  //compare.
    end;
    CancelScanningButton.Visible := False;
    CancelScanningButton.Enabled := False;
    OpenInEditorButton.Enabled := True;
    Text2GroupBox.Enabled := True;
    Text1GroupBox.Enabled := True;
  end;
  fUpdating := False;
end;


//  *****************    search        ***********************
procedure DoFind(aEditor: TSynEdit; const aSearchText: string; aOptions: TSynSearchOptions; aForward: boolean);
var
  StartPoint: TPoint;
begin
  if length(aSearchText) <> 0 then
  begin
    //Include(aOptions, ssoFindContinue);
    if aForward then
      Exclude(aOptions, ssoBackwards)
    else
      Include(aOptions, ssoBackwards);
    if not (ssoFindContinue in aOptions) then
      StartPoint := Point(1, 1)
    else
    begin
      StartPoint := aEditor.LogicalCaretXY;
      if ssoBackwards in aOptions then
      begin
        if aEditor.SelText = aSearchText then
          Dec(StartPoint.X, length(aSearchText));
      end
      else
      begin
        if aEditor.SelText = aSearchText then
          Inc(StartPoint.X, length(aSearchText));
      end;
    end;
    if aEditor.SearchReplaceEx(aSearchText, '', aOptions, StartPoint) = 0 then
      ShowMessage(Format(lisUESearchStringNotFound2, [aSearchText]));
  end;
end;

procedure DoFindDialog(aEditor: TSynEdit; var aSearchText: string; var aSearchOptions: TSynSearchOptions);
var
  LazFindReplaceDialog2: TLazFindReplaceDialog2;
begin
  LazFindReplaceDialog2 := TLazFindReplaceDialog2.Create(nil);
  try
    LazFindReplaceDialog2.FindText := aSearchText;
    LazFindReplaceDialog2.Options := aSearchOptions;

    LazFindReplaceDialog2.ReplaceWithCheckbox.Checked := False;
    LazFindReplaceDialog2.ReplaceWithCheckboxChange(nil);
    LazFindReplaceDialog2.ReplaceWithCheckbox.Enabled := False;

    if LazFindReplaceDialog2.ShowModal = mrOk then
    begin
      aSearchText := LazFindReplaceDialog2.FindText;
      aSearchOptions := LazFindReplaceDialog2.Options;
      DoFind(aEditor, aSearchText, aSearchOptions, not (ssoBackwards in aSearchOptions));
    end;
    LazFindReplaceDialog2.ReplaceWithCheckbox.Enabled := True;
  finally
    LazFindReplaceDialog2.Free;

  end;
end;


procedure TDiffForm.btnFindRClick(Sender: TObject);
begin
  DoFindDialog(edRight, fRightSearchText, fRightSearchOptions);
end;

procedure TDiffForm.btnFindNextRClick(Sender: TObject);
begin
  DoFind(edRight, fRightSearchText, fRightSearchOptions + [ssoFindContinue], True);
end;

procedure TDiffForm.btnFindPreviousRClick(Sender: TObject);
begin
  DoFind(edRight, fRightSearchText, fRightSearchOptions + [ssoFindContinue], False);
end;

procedure TDiffForm.btnFindLClick(Sender: TObject);
begin
  DoFindDialog(edLeft, fLeftSearchText, fLeftSearchOptions);
end;

procedure TDiffForm.btnFindNextLClick(Sender: TObject);
begin
  DoFind(edLeft, fLeftSearchText, fLeftSearchOptions + [ssoFindContinue], True);
end;

procedure TDiffForm.btnFindPreviousLClick(Sender: TObject);
begin
  DoFind(edLeft, fLeftSearchText, fLeftSearchOptions + [ssoFindContinue], False);
end;
//  **********************************************************

procedure TDiffForm.btnFirstClick(Sender: TObject);
begin
  edLeft.CaretY := 1;
  if fDiff.Count > 0 then
  begin
    if fDiff.Compares[0].Kind = ckNone then
      btnNextClick(Self);
  end;
end;

procedure TDiffForm.btnLastClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := edLeft.Lines.Count - 1;
  edLeft.CaretY := Idx + 1;
  if fDiff.Count >= Idx then
  begin
    if fDiff.Compares[Idx].Kind = ckNone then
      btnpreviousClick(Self);
  end;
end;

// normalize kind type if we ignore blank line aditions or deletions
function TDiffForm.GetNomalizedKindType(aLineIndex: integer): TChangeKind;
begin
  Result := fDiff.Compares[aLineIndex - 1].Kind;
  if tdfIgnoreEmptyLineChanges in fCurrentOptions then
  begin
    if Result = ckAdd then
    begin
      if fDiff.Compares[aLineIndex - 1].int2 = EMPTY_LINE_HASH then
        Result := ckNone;
    end
    else if Result = ckDelete then
    begin
      if fDiff.Compares[aLineIndex - 1].int1 = EMPTY_LINE_HASH then
        Result := ckNone;
    end;
  end;
end;

procedure TDiffForm.btnNextClick(Sender: TObject);
var
  LineIndex: integer;
  Kind, KindTest: TChangeKind;
begin
  LineIndex := edLeft.CaretY;
  if (LineIndex > fDiff.Count) or (LineIndex < 1) then
    Exit;
  Kind := GetNomalizedKindType(LineIndex);
  Inc(LineIndex);
  if LineIndex > fDiff.Count then
    Exit;
  while LineIndex <= fDiff.Count do
  begin
    KindTest := GetNomalizedKindType(LineIndex);
    if KindTest <> Kind then
    begin
      if KindTest <> ckNone then
      begin
        edLeft.CaretY := LineIndex;
        break;
      end;
      Kind := ckNone;
    end;
    Inc(LineIndex);
  end;
end;

procedure TDiffForm.btnpreviousClick(Sender: TObject);
var
  LineIndex: integer;
  Kind, KindTest: TChangeKind;
begin
  LineIndex := edLeft.CaretY;
  if (LineIndex > fDiff.Count) or (LineIndex < 1) then
    Exit;
  Kind := GetNomalizedKindType(LineIndex);
  Dec(LineIndex);
  if LineIndex < 1 then
    Exit;
  while LineIndex >= 1 do
  begin
    KindTest := GetNomalizedKindType(LineIndex);
    if KindTest <> Kind then
    begin
      if KindTest <> ckNone then
      begin
        edLeft.CaretY := LineIndex;
        break;
      end;
      Kind := ckNone;
    end;
    Dec(LineIndex);
  end;
end;

procedure TDiffForm.OnSpecialLineMarkup(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
var
  r, g, b: byte;
const
  COLOR_ADDED_LINE = $ffdddd;
  COLOR_ADDED_EMPTY_LINE = $fff5f5;
  COLOR_DELETED_LINE = $ddddff;
  COLOR_DELETED_EMPTY_LINE = $f5f5ff;
  COLOR_MODIFIED_LINE = $ddffdd;
  DARKNESS = $a;
begin

  if (fDiff.Count > 0) and (Line > 0) and (Line <= fDiff.Count) then
  begin
    Markup.Foreground := clNone;
    case fDiff.Compares[Line - 1].Kind of
      ckNone:
        Markup.Background := clWhite;
      ckAdd:
      begin
        Markup.Background := COLOR_ADDED_LINE;
        if (tdfIgnoreEmptyLineChanges in fCurrentOptions) and (fDiff.Compares[Line - 1].int2 =
          EMPTY_LINE_HASH) then
        begin
          Markup.Background := COLOR_ADDED_EMPTY_LINE;
        end;
      end;
      ckDelete:
      begin
        Markup.Background := COLOR_DELETED_LINE;
        // if (tdfIgnoreEmptyLineChanges in fCurrentOptions) and (edLeft.Lines[fDiff.Compares[Line-1].oldIndex1]='') then
        if (tdfIgnoreEmptyLineChanges in fCurrentOptions) and (fDiff.Compares[Line - 1].int1 =
          EMPTY_LINE_HASH) then
        begin
          Markup.Background := COLOR_DELETED_EMPTY_LINE;
        end;
      end;
      ckModify:
        Markup.Background := COLOR_MODIFIED_LINE;
    end;
  end;
  if Line = TSynEdit(Sender).CaretY then  //current line.
  begin
    r := Red(Markup.Background) - DARKNESS;
    g := Green(Markup.Background) - DARKNESS;
    b := Blue(Markup.Background) - DARKNESS;
    Markup.Background := RGB(r, g, b);
  end;
  Special := True;
end;

procedure Syncronize(Changes: TSynStatusChanges; aMaster: TSynEdit; aSlave: TSynEdit);
begin
  if scTopLine in Changes then
    aSlave.TopLine := aMaster.TopLine;
  if scLeftChar in Changes then
    aSlave.LeftChar := aMaster.LeftChar;
  if scCaretY in Changes then
  begin
    if aSlave.CaretY <> aMaster.CaretY then    // if then to allow text selection.
      aSlave.CaretY := aMaster.CaretY;
  end;
end;

procedure TDiffForm.edLeftStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  Syncronize(Changes, edLeft, edRight);
end;

procedure TDiffForm.edRightStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  Syncronize(Changes, edRight, edLeft);
end;

procedure TDiffForm.HelpButtonClick(Sender: TObject);
begin
  //  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TDiffForm.Text1ComboboxChange(Sender: TObject);
begin
  fSelectedFile1.UpdateIndex;
end;

procedure TDiffForm.Text2ComboboxChange(Sender: TObject);
begin
  fSelectedFile2.UpdateIndex;
end;

procedure TDiffForm.SetupComponents;
const
  GetAllFilesMask2 = '*.*';
begin
  // text 1
  Text1GroupBox.Caption := lisDiffDlgFile12;
  Text1OnlySelectionCheckBox.Caption := lisDiffDlgOnlySelection2;
  Text1FileOpenButton.Caption := '...';

  // text 2
  Text2GroupBox.Caption := lisDiffDlgFile22;
  Text2OnlySelectionCheckBox.Caption := lisDiffDlgOnlySelection2;
  Text2FileOpenButton.Caption := '...';

  // options
  with OptionsGroupBox do
  begin
    Caption := lisOptions2;
    Items.Add(lisDiffDlgCaseInsensitive2);
    Items.Add(lisDiffDlgIgnoreIfEmptyLinesWereAdd2);
    Items.Add(lisDiffDlgIgnoreSpacesAtStartOfLine2);
    Items.Add(lisDiffDlgIgnoreSpacesAtEndOfLine2);
    Items.Add(lisDiffDlgIgnoreIfSpaceCharsWereAdd2);
    Items.Add(lisDiffDlgIgnoreSpaces2);
    //Items.Add(lisDiffDlgIgnoreIfLineEndCharsDiffe2);
  end;

  // buttons
  IDEImages.AssignImage(CancelScanningButton, 'btn_cancel');
  CloseButton.Caption := lisClose2;
  OpenInEditorButton.Caption := lisDiffDlgOpenDiffInEditor2;
  HelpButton.Caption := lisMenuHelp2;

  OpenInEditorButton.LoadGlyphFromStock(idButtonOpen);
  if OpenInEditorButton.Glyph.Empty then
    IDEImages.AssignImage(OpenInEditorButton, 'laz_open');
  // dialogs
  dlgOpen.Title := lisOpenExistingFile2;
  dlgOpen.Filter := dlgFilterAll2 + ' (' + GetAllFilesMask2 + ')|' + GetAllFilesMask2 +
    '|' + dlgFilterLazarusUnit2 + ' (*.pas;*.pp)|*.pas;*.pp' + '|' + dlgFilterLazarusProject2 +
    ' (*.lpi)|*.lpi' + '|' + dlgFilterLazarusForm2 + ' (*.lfm;*.dfm)|*.lfm;*.dfm'
    + '|' + dlgFilterLazarusPackage2 + ' (*.lpk)|*.lpk' + '|' +
    dlgFilterLazarusProjectSource2 + ' (*.lpr)|*.lpr';
  //tabsheets
  tbsOptions.Caption := lisDiffTabOptions2;
  tbsCompare.Caption := lisDiffTabCompare2;
  tbsPatch.Caption := lisDiffTabPatch2;

  //buttons
  btnCompare.Caption := lisDiffBtnCompare2;
  btnFirst.Caption := lisDiffBtnFirst2;
  btnLast.Caption := lisDiffBtnLast2;
  btnNext.Caption := lisDiffBtnNext2;
  btnprevious.Caption := lisDiffBtnPrevious2;
  // diff
  SourceEditorManagerIntf.GetEditorControlSettings(DiffSynEdit);
  SourceEditorManagerIntf.GetEditorControlSettings(edLeft);
  SourceEditorManagerIntf.GetEditorControlSettings(edRight);
  // code folding not allowed.
  edLeft.Gutter.Parts[4].Visible := False;
  edRight.Gutter.Parts[4].Visible := False;
  SourceEditorManagerIntf.GetHighlighterSettings(SynFreePascalSyn1);
  IDEImages.AssignImage(btnFindL, 'menu_search_find');
  IDEImages.AssignImage(btnFindR, 'menu_search_find');
  IDEImages.AssignImage(btnFindPreviousL, 'menu_search_find_previous');
  IDEImages.AssignImage(btnFindPreviousR, 'menu_search_find_previous');
  IDEImages.AssignImage(btnFindNextL, 'menu_search_find_next');
  IDEImages.AssignImage(btnFindNextR, 'menu_search_find_next');
end;

procedure TDiffForm.UpdateDiff;
begin
end;

procedure TDiffForm.Init;
var
  //LastText2Name: string;
  i: integer;
begin
  // fill all diff file names
  FillTextComboBoxes;

  fSelectedFile1.SetIndex(fFile1Index);
  // get recent Text 2
  i := 0;
  //  LastText2Name:=InputHistories.DiffText2;
  //if LastText2Name <> '' then
   // i := fAvailableFiles.IndexOfName(LastText2Name);
  //if i < 0 then i := 0;
  if i = fAvailableFiles.IndexOf(fSelectedFile2.fFile) then Inc(i);
  fSelectedFile2.SetIndex(i);
  // set recent options
  LoadSettings;
end;

procedure TDiffForm.FillTextComboBoxes;
var
  i: integer;
begin
  // Text 1
  Text1Combobox.Items.BeginUpdate;
  Text1Combobox.Items.Clear;
  for i := 0 to fAvailableFiles.Count - 1 do
    Text1Combobox.Items.Add(fAvailableFiles[i].Name);
  Text1Combobox.Items.EndUpdate;

  // Text 2
  Text2Combobox.Items.BeginUpdate;
  Text2Combobox.Items.Clear;
  for i := 0 to fAvailableFiles.Count - 1 do
    Text2Combobox.Items.Add(fAvailableFiles[i].Name);
  Text2Combobox.Items.EndUpdate;
end;

procedure TDiffForm.FormCreate(Sender: TObject);
begin
  //Text1Combobox.DropDownCount:=EnvironmentOptions.DropDownCount;
  //Text2Combobox.DropDownCount:=EnvironmentOptions.DropDownCount;
  fCurrentOptions := GetDiffOptions;
  PageControl1.TabIndex := 0;
end;

procedure TDiffForm.SaveSettings;
var
  DiffFlags: TTextDiffFlags2;
  Config: TConfigStorage;
const
  Version = 1;
begin
  DiffFlags := GetDiffOptions;
  try
    Config := GetIDEConfigStorage('lazdiffcomparefiles.xml', False);
    try
      // store the version number so future extensions can handle old config files
      Config.SetDeleteValue('Version', Version, 0);
      // store string variable "SomeValue"
      // if SomeValue has the default value the entry is not stored,
      // so only the differences to the default are stored.
      // This way the xml is kept short and defaults may change in future.
      Config.SetDeleteValue('Options/IgnoreCase', tdfIgnoreCase in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreEmptyLineChanges',
        tdfIgnoreEmptyLineChanges in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreHeadingSpaces', tdfIgnoreHeadingSpaces in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreLineEnds', tdfIgnoreLineEnds in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreSpaceCharAmount', tdfIgnoreSpaceCharAmount in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreSpaceChars', tdfIgnoreSpaceChars in DiffFlags, False);
      Config.SetDeleteValue('Options/IgnoreTrailingSpaces', tdfIgnoreTrailingSpaces in DiffFlags, False);
      if (fSelectedFile1 <> nil) and (fSelectedFile1.fFile <> nil) then
      begin
        Config.SetDeleteValue('Options/DiffText1', fSelectedFile1.fFile.Name, '');
        Config.SetDeleteValue('Options/Text1OnlySelection',
          Text1OnlySelectionCheckBox.Checked, False);
      end
      else
      begin
        Config.SetDeleteValue('Options/DiffText1', '', '');
        Config.SetDeleteValue('Options/Text1OnlySelection', False, False);
      end;
      if (fSelectedFile2 <> nil) and (fSelectedFile2.fFile <> nil) then
      begin
        Config.SetDeleteValue('Options/DiffText2', fSelectedFile2.fFile.Name, '');
        Config.SetDeleteValue('Options/Text2OnlySelection',
          Text2OnlySelectionCheckBox.Checked, False);
      end
      else
      begin
        Config.SetDeleteValue('Options/DiffText2', '', '');
        Config.SetDeleteValue('Options/Text2OnlySelection', False, False);
      end;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      AddIDEMessage(mluWarning, 'Saving lazdiffcomparefiles.xml failed: ' + E.Message);
    end;
  end;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TDiffForm.LoadSettings;
var
  Config: TConfigStorage;
  Version: integer;
  i: integer;
  LastText1Name,LastText2Name: string;
begin
  try
    Config := GetIDEConfigStorage('lazdiffcomparefiles.xml', True);
    try
      Version := Config.GetValue('Version', 1);
      OptionsGroupBox.Checked[IgnoreCaseCheckBox] := Config.GetValue('Options/IgnoreCase', False);
      OptionsGroupBox.Checked[IgnoreEmptyLineChangesCheckBox] :=
        Config.GetValue('Options/IgnoreEmptyLineChanges', False);
      OptionsGroupBox.Checked[IgnoreHeadingSpacesCheckBox] := Config.GetValue('Options/IgnoreHeadingSpaces', False);
      //OptionsGroupBox.Checked[IgnoreLineEndsCheckBox] := Config.GetValue('Options/IgnoreLineEnds', False);
      OptionsGroupBox.Checked[IgnoreSpaceCharAmountCheckBox] :=
        Config.GetValue('Options/IgnoreSpaceCharAmount', False);
      OptionsGroupBox.Checked[IgnoreSpaceCharsCheckBox] := Config.GetValue('Options/IgnoreSpaceChars', False);
      OptionsGroupBox.Checked[IgnoreTrailingSpacesCheckBox] := Config.GetValue('Options/IgnoreTrailingSpaces', False);
      // get recent Text 1
      i := 0;
      LastText1Name := Config.GetValue('Options/DiffText1', '');
      Text1OnlySelectionCheckBox.Checked := Config.GetValue('Options/Text1OnlySelection', False);
      if LastText1Name <> '' then
      begin
        i := fAvailableFiles.IndexOfName(LastText1Name);
        if i = -1 then
        begin
          fAvailableFiles.Add(TAvailableDiffFile.Create(LastText1Name, nil, False));
          Text1ComboBox.Items.Add(LastText1Name);
          Text2ComboBox.Items.Add(LastText1Name);
          i:=fAvailableFiles.Count-1;
        end;
        fSelectedFile1.SetFileName(LastText1Name);
        fSelectedFile1.SetIndex(i);
      end
      else
      begin
        fSelectedFile1.SetIndex(-1);
      end;
      fSelectedFile1.UpdateIndex;
      // get recent Text 2
      i := 0;
      LastText2Name := Config.GetValue('Options/DiffText2', '');
      Text2OnlySelectionCheckBox.Checked := Config.GetValue('Options/Text2OnlySelection', False);
      if LastText2Name <> '' then
      begin
        i := fAvailableFiles.IndexOfName(LastText2Name);
        if i = -1 then
        begin
          fAvailableFiles.Add(TAvailableDiffFile.Create(LastText2Name, nil, False));
          Text1ComboBox.Items.Add(LastText2Name);
          Text2ComboBox.Items.Add(LastText2Name);
          i:=fAvailableFiles.Count-1;
        end;
        fSelectedFile2.SetFileName(LastText2Name);
        fSelectedFile2.SetIndex(i);
      end
      else
      begin
        fSelectedFile2.SetIndex(-1);
      end;
      fSelectedFile2.UpdateIndex;
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      AddIDEMessage(mluWarning, 'Loading lazdiffcomparefiles.xml failed: ' + E.Message);
    end;
  end;
end;

function TDiffForm.GetDiffOptions: TTextDiffFlags2;
begin
  Result := [];
  if OptionsGroupBox.Checked[IgnoreCaseCheckBox] then
    Include(Result, tdfIgnoreCase);
  if OptionsGroupBox.Checked[IgnoreEmptyLineChangesCheckBox] then
    Include(Result, tdfIgnoreEmptyLineChanges);
  if OptionsGroupBox.Checked[IgnoreHeadingSpacesCheckBox] then
    Include(Result, tdfIgnoreHeadingSpaces);
  //if OptionsGroupBox.Checked[IgnoreLineEndsCheckBox] then
  //  Include(Result, tdfIgnoreLineEnds);
  if OptionsGroupBox.Checked[IgnoreSpaceCharAmountCheckBox] then
    Include(Result, tdfIgnoreSpaceCharAmount);
  if OptionsGroupBox.Checked[IgnoreSpaceCharsCheckBox] then
    Include(Result, tdfIgnoreSpaceChars);
  if OptionsGroupBox.Checked[IgnoreTrailingSpacesCheckBox] then
    Include(Result, tdfIgnoreTrailingSpaces);
end;

{ TAvailableDiffFile }

constructor TAvailableDiffFile.Create(const NewName: string; NewEditor: TSourceEditorInterface;
  NewSelectionAvailable: boolean);
begin
  Name := NewName;
  Editor := NewEditor;
  SelectionAvailable := NewSelectionAvailable;
end;

{ TAvailableDiffFiles }

function TAvailableDiffFiles.GetItems(Index: integer): TAvailableDiffFile;
begin
  Result := TAvailableDiffFile(inherited Items[Index]);
end;

procedure TAvailableDiffFiles.SetItems(Index: integer; const AValue: TAvailableDiffFile);
begin
  inherited Items[Index] := AValue;
end;

procedure TAvailableDiffFiles.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;

function TAvailableDiffFiles.Add(DiffFile: TAvailableDiffFile): integer;
begin
  Result := inherited Add(DiffFile);
end;

function TAvailableDiffFiles.IndexOfName(const Name: string): integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].Name <> Name) do Dec(Result);
end;

end.
