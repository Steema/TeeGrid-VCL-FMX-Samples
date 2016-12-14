unit Unit_Sheet;

interface

{
  This example uses TeeGrid to simulate a spreadsheet (Excel)
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Menus, FMX.TabControl,

  FMXTee.Sheet.Tools, FMXTee.Sheet.Grid, FMXTee.Sheet.Expression, Tee.Sheet;

type
  TFormSheet = class(TForm)
    LayoutTools: TLayout;
    TabSheets: TTabControl;
    MainMenu1: TMainMenu;
    LayoutExpression: TLayout;
    PopupTabs: TPopupMenu;
    InsertTab: TMenuItem;
    DeleteTab: TMenuItem;
    RenameTab: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure InsertTabClick(Sender: TObject);
    procedure PopupTabsPopup(Sender: TObject);
    procedure DeleteTabClick(Sender: TObject);
    procedure TabSheetsChange(Sender: TObject);
    procedure RenameTabClick(Sender: TObject);
  private
    { Private declarations }

    Expression : TSheetExpression;
    Tools : TSheetTools;

    function AddSheet(const ATab:TTabControl):TGridSheet;
    procedure ChangedCurrent(Sender: TObject);
    function Current:TSheet;
    function CurrentGrid: TGridSheet;
    function NewName:String;
    procedure Prepare(const AMenu:TMainMenu);
    procedure SelectedChanged(Sender: TObject);
    procedure SelectLastTab;
    procedure TypingCell(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormSheet: TFormSheet;

implementation

{$R *.fmx}

function TFormSheet.NewName:String;

  function Find(const AName:String):TGridSheet;
  var t : Integer;
  begin
    for t:=0 to TabSheets.TabCount-1 do
        if SameText(TabSheets.Tabs[t].Text,AName) then
           Exit(TGridSheet(TabSheets.Tabs[t]));

    result:=nil;
  end;

var tmp : Integer;
begin
  tmp:=1;

  repeat
    result:='Sheet'+tmp.ToString;

    if Find(result)=nil then
       break
    else
       Inc(tmp);

  until False;
end;

function TFormSheet.AddSheet(const ATab: TTabControl): TGridSheet;
var tmp : TTabItem;
begin
  tmp:=ATab.Add;
  tmp.Text:=NewName;

  result:=TGridSheet.Create(Self);
  result.Align:=TAlignLayout.Client;
  result.Parent:=tmp;

  tmp.TagObject:=result;

  result.Grid.OnSelect:=SelectedChanged;
  result.Grid.OnTyping:=TypingCell;
end;

procedure TFormSheet.TypingCell(Sender: TObject);
begin
  Expression.RefreshFormula;
end;

procedure TFormSheet.SelectedChanged(Sender: TObject);
begin
  Current.Grid.Header.SelectedColumn:=Current.Grid.Selected.Column;

  Expression.RefreshSelected;
  Expression.RefreshFormula;
end;

procedure TFormSheet.TabSheetsChange(Sender: TObject);
var tmp : TSheet;
begin
  tmp:=Current;

  if tmp<>nil then
  begin
    Tools.Refresh(tmp);
    Expression.Refresh(tmp);
  end;
end;

function TFormSheet.CurrentGrid: TGridSheet;
var tmp : TObject;
begin
  tmp:=TabSheets.ActiveTab.TagObject;

  if tmp=nil then
     result:=nil
  else
     result:=TGridSheet(tmp);
end;

function TFormSheet.Current: TSheet;
var tmp : TGridSheet;
begin
  tmp:=CurrentGrid;

  if tmp=nil then
     result:=nil
  else
     result:=tmp.Sheet;
end;

procedure TFormSheet.DeleteTabClick(Sender: TObject);
var tmp,
    tmpCount : Integer;
begin
  tmp:=TabSheets.ActiveTab.Index;

  TabSheets.ActiveTab.Free;

  tmpCount:=TabSheets.TabCount;

  if tmp<tmpCount-1 then
     TabSheets.ActiveTab:=TabSheets.Tabs[tmp]
  else
     TabSheets.ActiveTab:=TabSheets.Tabs[tmpCount-1];
end;

procedure TFormSheet.FormCreate(Sender: TObject);
begin
  TabSheets.TabPosition:=TTabPosition.Bottom;

  Tools:=TSheetTools.Embedd(Self,LayoutTools);

  Expression:=TSheetExpression.Embedd(Self,LayoutExpression);
  Expression.OnChangeCurrent:=ChangedCurrent;

  Prepare(MainMenu1);

  InsertTabClick(Self);
end;

// Switch to last tab
procedure TFormSheet.SelectLastTab;
begin
  TabSheets.ActiveTab:=TabSheets.Tabs[TabSheets.TabCount-1];
  TabSheetsChange(Self);
end;

procedure TFormSheet.ChangedCurrent(Sender: TObject);
begin
  CurrentGrid.SetFocus;
end;

procedure TFormSheet.InsertTabClick(Sender: TObject);
begin
  AddSheet(TabSheets);
  SelectLastTab;
end;

procedure TFormSheet.PopupTabsPopup(Sender: TObject);
begin
  DeleteTab.Enabled:=TabSheets.TabCount>1;
end;

procedure TFormSheet.Prepare(const AMenu: TMainMenu);
begin
end;

procedure TFormSheet.RenameTabClick(Sender: TObject);
var tmp : String;
begin
  tmp:=TabSheets.ActiveTab.Text;

  tmp:=InputBox('Rename Sheet','Name',tmp);
  TabSheets.ActiveTab.Text:=tmp;
end;

end.
