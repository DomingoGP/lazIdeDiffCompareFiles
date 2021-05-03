{
  Author: Domingo Galmés
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Register the package in the lazarus IDE
}
unit lazdiffregisteride;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType,
  // IdeIntf
  LazIDEIntf, MenuIntf, IdeCommands;

procedure Register;

implementation

uses
  lazdiffform, lazdiffUIConsts,IDEWindowIntf,controls;

procedure DoCompareFilesMenu(Sender: TObject);
begin
  ShowDiffDialog;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  CmdMyTool: TIDECommand;
begin
  // register IDE shortcut and menu item
  Key := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
  Cat := IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdMyTool := RegisterIDECommand(Cat, FORMAT_CURRENT_MENU2, MENU_CMD_DESC2,
    Key, nil, @DoCompareFilesMenu);
  RegisterIDEMenuCommand(itmSecondaryTools, 'LazDiffCompareFiles',
    FORMAT_CURRENT_MENU2, nil, nil, CmdMyTool);
  // register window creator
  IDEWindowCreators.Add(DiffFormWindowName,@CreateIDEDiffForm,nil,'1097','667','','','',alNone,true);
end;

end.
