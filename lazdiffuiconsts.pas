{
  Author: Domingo Galmés
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 constant strings for the package lazdiffcomparefiles
}
unit lazdiffUIConsts;

{$mode objfpc}{$H+}

interface

resourcestring
  FORMAT_CURRENT_MENU2 = 'Diff compare files...';
  MENU_CMD_DESC2 = 'Compares files side by side';
  dlgTextToFind2 = 'Text to find';
  dlgReplaceWith2 = 'Replace with';
  lisOptions2 = 'Options';
  lisReplace2 = 'Replace';
  lisMenuFind2 = 'Find';
  lisBtnFind2 = 'Find';
  lisBtnReplace2 = 'Replace';
  lisUEErrorInRegularExpression2 = 'Error in regular expression';
  lisAutoCompletionOn2 = 'Enable autocomplete on';
  lisAutoCompletionOff2 = 'Enable autocomplete off';
  // Search dialog
  dlgSearchCaption2 = 'Searching ...';
  dlgSearchAbort2 = 'Search terminated by user.';
  dlgSeachDirectoryNotFound2 = 'Search directory "%s" not found.';
  lissMatches2 = 'Matches';
  lissSearching2 = 'Searching';
  lissSearchText2 = 'Search text';
  dlgCaseSensitive2 = '&Case sensitive';
  lisDistinguishBigAndSmallLettersEGAAndA2 =
    'Distinguish big and small letters e.g. A and a';
  dlgWholeWordsOnly2 = '&Whole words only';
  lisOnlySearchForWholeWords2 = 'Only search for whole words';
  dlgRegularExpressions2 = 'Regular e&xpressions';
  lisActivateRegularExpressionSyntaxForTextAndReplaceme2 =
    'Activate regular ' + 'expression syntax for text and replacement (pretty much like perl)';
  lisAllowSearchingForMultipleLines2 = 'Allow searching for multiple lines';
  dlgPromptOnReplace2 = '&Prompt on replace';
  lisAskBeforeReplacingEachFoundText2 = 'Ask before replacing each found text';
  dlgSROrigin2 = 'Origin';
  dlgFromCursor2 = '&From cursor';
  dlgFromBeginning2 = 'From b&eginning';
  dlgSearchScope2 = 'Search scope';

  lisFindFileMultiLinePattern2 = '&Multiline pattern';
  dlgGlobal2 = '&Global';
  dlgSelectedText2 = '&Selected text';
  dlgDirection2 = 'Direction';
  lisFRForwardSearch2 = 'Forwar&d search';
  lisFRBackwardSearch2 = '&Backward search';
  dlgReplaceAll2 = 'Replace &All';

  lisClose2 = 'Close';
  lisMenuHelp2 = 'Help';
  lisOpenExistingFile2 = 'Open existing file';
  lisCaptionCompareFiles2 = 'Compare files';
  lisDiffDlgFile12 = 'File1';
  lisDiffDlgOnlySelection2 = 'Only selection';
  lisDiffDlgFile22 = 'File2';
  lisDiffDlgCaseInsensitive2 = 'Case Insensitive';
  lisDiffDlgIgnoreIfEmptyLinesWereAdd2 = 'Ignore if empty lines were added or removed';
  lisDiffDlgIgnoreSpacesAtStartOfLine2 = 'Ignore spaces at start of line';
  lisDiffDlgIgnoreSpacesAtEndOfLine2 = 'Ignore spaces at end of line';
  lisDiffDlgIgnoreIfLineEndCharsDiffe2 = 'Ignore difference in line ends (e.' + 'g. #10 = #13#10)';
  lisDiffDlgIgnoreIfSpaceCharsWereAdd2 = 'Ignore amount of space chars';
  lisDiffDlgIgnoreSpaces2 = 'Ignore spaces (newline chars not included)';
  lisDiffDlgOpenDiffInEditor2 = 'Open difference in editor';

  lisDiffTabOptions2 = 'Options';
  lisDiffTabCompare2 = 'Compare';
  lisDiffTabPatch2 = 'Patch';
  lisDiffBtnCompare2 = 'Compare';
  lisDiffBtnFirst2 = 'First';
  lisDiffBtnLast2 = 'Last';
  lisDiffBtnNext2 = 'Next';
  lisDiffBtnPrevious2 = 'Previous';
  lisUESearchStringNotFound2 = 'Search string ''%s'' not found!';

  dlgFilterAll2 = 'All files';
  dlgFilterLazarusUnit2 = 'Lazarus unit';
  dlgFilterLazarusProject2 = 'Lazarus project';
  dlgFilterLazarusForm2 = 'Lazarus form';
  dlgFilterLazarusPackage2 = 'Lazarus package';
  dlgFilterLazarusProjectSource2 = 'Lazarus project source';

implementation

end.
