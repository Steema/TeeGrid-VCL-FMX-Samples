unit Unit_Sheet;

interface

{
  This example uses TeeGrid to simulate a spreadsheet (Excel)
}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Menus, FMX.TabControl,

  FMXTee.Sheet.Tools, FMXTee.Sheet.Grid, FMXTee.Sheet.Expression, Tee.Sheet,
  Tee.Grid.Columns;

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
    PopupColumns: TPopupMenu;
    MenuInsertColumn: TMenuItem;
    MenuDeleteColumn: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure InsertTabClick(Sender: TObject);
    procedure PopupTabsPopup(Sender: TObject);
    procedure DeleteTabClick(Sender: TObject);
    procedure TabSheetsChange(Sender: TObject);
    procedure RenameTabClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuDeleteColumnClick(Sender: TObject);
  private
    { Private declarations }

    Expression : TSheetExpression;
    Tools : TSheetTools;

    Sheets : TSheets;

    function AddSheet(const ATab:TTabControl):TGridSheet;
    procedure ChangedCurrent(Sender: TObject);
    function Current:TSheet;
    function CurrentColumn:TColumn;
    function CurrentGrid: TGridSheet;
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

function TFormSheet.AddSheet(const ATab: TTabControl): TGridSheet;
var tmp : TTabItem;
begin
  tmp:=ATab.Add;

  result:=Sheets.Add.Sheet;

  result.Align:=TAlignLayout.Client;
  result.Parent:=tmp;

  tmp.Text:=result.Sheet.Name;
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

  Tools.Refresh(Current);
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

function TFormSheet.CurrentColumn: TColumn;
begin
  result:=Current.Grid.Selected.Column;
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
  Sheets:=TSheets.Create(Self,TSheetItem);

  TabSheets.TabPosition:=TTabPosition.Bottom;

  Tools:=TSheetTools.Embedd(Self,LayoutTools);

  Expression:=TSheetExpression.Embedd(Self,LayoutExpression);
  Expression.OnChangeCurrent:=ChangedCurrent;

  Prepare(MainMenu1);

  InsertTabClick(Self);
end;

procedure TFormSheet.FormDestroy(Sender: TObject);
begin
  Sheets.Free;
end;

procedure TFormSheet.FormShow(Sender: TObject);
begin
  CurrentGrid.SetFocus;
  CurrentGrid.Selected.Column:=CurrentGrid.Columns[1];
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

procedure TFormSheet.MenuDeleteColumnClick(Sender: TObject);
begin
  CurrentColumn.Free;
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
