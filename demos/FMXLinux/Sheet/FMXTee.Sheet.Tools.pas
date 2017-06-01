unit FMXTee.Sheet.Tools;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMXTee.Sheet.Editor.Font,

  Tee.Sheet, Tee.Format, Tee.Painter, Tee.Renders, Tee.Grid.Columns;

type
  TSheetTools = class(TForm)
    Tabs: TTabControl;
    TabFile: TTabItem;
    TabHome: TTabItem;
    TabInsert: TTabItem;
    LayoutClipboard: TLayout;
    BPaste: TButton;
    BCut: TButton;
    BCopy: TButton;
    LayoutFont: TLayout;
    Text1: TText;
    Text2: TText;
    LayoutAlign: TLayout;
    Text3: TText;
    SBTop: TSpeedButton;
    SBVCenter: TSpeedButton;
    SBBottom: TSpeedButton;
    SBLeft: TSpeedButton;
    SBCenter: TSpeedButton;
    SBRight: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure BCutClick(Sender: TObject);
    procedure BPasteClick(Sender: TObject);
    procedure BCopyClick(Sender: TObject);
    procedure SBTopClick(Sender: TObject);
    procedure SBVCenterClick(Sender: TObject);
    procedure SBBottomClick(Sender: TObject);
    procedure SBLeftClick(Sender: TObject);
    procedure SBCenterClick(Sender: TObject);
    procedure SBRightClick(Sender: TObject);
  private
    { Private declarations }

    Sheet : TSheet;

    IFont : TSheetFontEditor;

    procedure ClearSelected;
    function CurrentAlign:TTextAlign;
    function GetCell(out AColumn:TColumn; out ARow:Integer):Boolean;
    procedure RefreshAlign(const Align:TTextAlign);
    function SelectedText:String;
    procedure SetSelected(const Value:String);
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TControl):TSheetTools; static;

    procedure Refresh(const ASheet:TSheet);
  end;

implementation

{$R *.fmx}

uses
  System.Rtti,

  FMXTee.Editor.Painter.Stroke, FMX.Platform, Tee.Sheet.Data;

{ TSheetTools }

procedure TSheetTools.BCopyClick(Sender: TObject);
{$IF CompilerVersion>23}
var ClipService : IFMXClipboardService;
{$IFEND}
var tmp : String;
begin
  tmp:=SelectedText;

  {$IF CompilerVersion>23}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(ClipService)) then
     ClipService.SetClipboard(tmp);
  {$ELSE}
     Platform.SetClipboard(tmp);
  {$IFEND}
end;

procedure TSheetTools.BCutClick(Sender: TObject);
begin
  BCopyClick(Self);
  ClearSelected;
end;

procedure TSheetTools.BPasteClick(Sender: TObject);
{$IF CompilerVersion>23}
var ClipService : IFMXClipboardService;
{$IFEND}
var tmp : TValue;
begin
  tmp:=SelectedText;

  {$IF CompilerVersion>23}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(ClipService)) then
     tmp:=ClipService.GetClipboard;
  {$ELSE}
     tmp:=Platform.GetClipboard;
  {$IFEND}

  SetSelected(tmp.AsString);
end;

procedure TSheetTools.SetSelected(const Value:String);
var tmpCol : TColumn;
    tmpRow : Integer;
begin
  if GetCell(tmpCol,tmpRow) then
     Sheet.Data.Cells[tmpCol.Index,tmpRow]:=Value;
end;

procedure TSheetTools.ClearSelected;
begin
  SetSelected('');
end;

class function TSheetTools.Embedd(const AOwner: TComponent;
  const AParent: TControl): TSheetTools;
begin
  result:=TSheetTools.Create(AOwner);
  TStrokeEditor.AddForm(result,AParent);
end;

procedure TSheetTools.FormCreate(Sender: TObject);
begin
  Tabs.ActiveTab:=TabHome;
end;

function TSheetTools.GetCell(out AColumn: TColumn; out ARow: Integer): Boolean;
begin
  AColumn:=Sheet.Grid.Selected.Column;
  ARow:=Sheet.Grid.Selected.Row;

  result:=(AColumn<>nil) and (ARow<>-1);
end;

function TSheetTools.CurrentAlign:TTextAlign;
begin
  result:=Sheet.CurrentRender.TextAlign;
end;

procedure TSheetTools.RefreshAlign(const Align:TTextAlign);
begin
  SBTop.IsPressed:=Align.Vertical=TVerticalAlign.Top;
  SBVCenter.IsPressed:=Align.Vertical=TVerticalAlign.Center;
  SBBottom.IsPressed:=Align.Vertical=TVerticalAlign.Bottom;

  SBLeft.IsPressed:=Align.Horizontal=THorizontalAlign.Left;
  SBCenter.IsPressed:=Align.Horizontal=THorizontalAlign.Center;
  SBRight.IsPressed:=Align.Horizontal=THorizontalAlign.Right;
end;

procedure TSheetTools.Refresh(const ASheet: TSheet);
var tmp : TSheetCell;
    tmpRender : TRender;
    tmpFormat : TTextFormat;
begin
  Sheet:=ASheet;

  tmp:=Sheet.CurrentCell;

  if tmp=nil then
     tmpRender:=nil
  else
     tmpRender:=tmp.Render;

  if tmpRender=nil then
     tmpRender:=Sheet.Grid.Cells;

  if tmpRender is TTextRender then
  begin
    LayoutAlign.Enabled:=True;
    RefreshAlign(TTextRender(tmpRender).TextAlign);
  end
  else
    LayoutAlign.Enabled:=False;

  if tmpRender is TFormatRender then
  begin
    LayoutFont.Enabled:=True;

    tmpFormat:=TFormatRender(tmpRender).Format;

    if IFont=nil then
       IFont:=TSheetFontEditor.Embedd(Self,LayoutFont,tmpFormat,Sheet)
    else
       IFont.Refresh(tmpFormat,Sheet);
  end
  else
    LayoutFont.Enabled:=False;
end;

procedure TSheetTools.SBBottomClick(Sender: TObject);
begin
  CurrentAlign.Vertical:=TVerticalAlign.Bottom;
end;

procedure TSheetTools.SBCenterClick(Sender: TObject);
begin
  CurrentAlign.Horizontal:=THorizontalAlign.Center;
end;

procedure TSheetTools.SBLeftClick(Sender: TObject);
begin
  CurrentAlign.Horizontal:=THorizontalAlign.Left;
end;

procedure TSheetTools.SBRightClick(Sender: TObject);
begin
  CurrentAlign.Horizontal:=THorizontalAlign.Right;
end;

procedure TSheetTools.SBTopClick(Sender: TObject);
begin
  CurrentAlign.Vertical:=TVerticalAlign.Top;
end;

procedure TSheetTools.SBVCenterClick(Sender: TObject);
begin
  CurrentAlign.Vertical:=TVerticalAlign.Center;
end;

function TSheetTools.SelectedText: String;
var tmpCol : TColumn;
    tmpRow : Integer;
begin
  if GetCell(tmpCol,tmpRow) then
     result:=Sheet.Data[tmpCol.Index,tmpRow]
  else
     result:='';
end;

end.
