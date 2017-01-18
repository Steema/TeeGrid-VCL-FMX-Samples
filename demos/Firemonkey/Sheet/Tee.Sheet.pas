unit Tee.Sheet;

interface

{
   Common unit (VCL, Firemonkey and Lazarus LCL)
}

uses
  {System.}Classes,

  Tee.Grid, Tee.Grid.Columns, Tee.Format, Tee.Grid.Header,
  Tee.Renders, Tee.Painter, Tee.Sheet.Data, BI.Expression;

type
  TSheet=class
  private
    FData : TSheetData;
    FName : String;

    IGrid : TCustomTeeGrid;

    function CreateData:TSheetData;
    procedure FillRows;
    procedure InitHeaderFormat(const AFormat:TTextFormat);
    procedure InitHeaderSelected(const ASelected:TSelectedRender);
    procedure InitRowNumbers(const AColumn:TColumn);
    procedure InitSelectedFormat(const AFormat:TTextFormat);
    procedure InitSheetBorder(const ABorder:TBorder);
    function NewExpression(const S:String):TExpression;
    procedure PaintCell(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
    procedure PaintRowNumber(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
    function ParseError(const APos:Integer; const AMessage:String):Boolean;
    procedure Prepare(const AColumns:TColumns);
    function Resolve(const S:String; IsFunction:Boolean):TExpression;
  public
    Constructor Create(const AGrid:TCustomTeeGrid);

    function CurrentCell:TSheetCell;
    function CurrentRender:TTextRender;

    property Data:TSheetData read FData;
    property Grid:TCustomTeeGrid read IGrid;

    property Name:String read FName write FName;
  end;

implementation

uses
  {System.}SysUtils,

  System.UITypes,

  Tee.Control, Tee.Cell.Expression;

{ TSheet }

Constructor TSheet.Create(const AGrid:TCustomTeeGrid);
begin
  inherited Create;

  IGrid:=AGrid;

  FData:=CreateData;
  IGrid.Data:=FData;

  FillRows;

  Prepare(IGrid.Columns);

  InitHeaderFormat(IGrid.Header.Format);

  InitHeaderSelected(IGrid.Header.Selected);

  IGrid.Rows.Back.Stroke.Show;

  InitSelectedFormat(IGrid.Selected.Format);

  // Advance to next row when pressing the enter key in editing mode
  IGrid.Editing.EnterKey:=TEditingEnter.NextRow;

  // Automatically start the cell editing mode when pressing a letter or number key
  IGrid.Editing.AutoEdit:=True;
end;

procedure TSheet.InitSheetBorder(const ABorder:TBorder);
begin
  ABorder.Show;
  ABorder.Size:=2;
  ABorder.Brush.InitColor(TColors.Darkgreen);
end;

procedure TSheet.InitHeaderSelected(const ASelected:TSelectedRender);
begin
  ASelected.Format.Font.InitColor(TColors.Darkgreen);

  InitSheetBorder(ASelected.Borders.Bottom);
end;

procedure TSheet.InitHeaderFormat(const AFormat:TTextFormat);
begin
  AFormat.Font.Style:=[];
  AFormat.Brush.Gradient.Hide;
  AFormat.Brush.Color:=TColors.White;
end;

procedure TSheet.InitSelectedFormat(const AFormat:TTextFormat);
begin
  AFormat.Brush.Hide;
  AFormat.Stroke.Style:=TStrokeStyle.Solid;
  AFormat.Stroke.Color:=TColors.Darkgreen;
  AFormat.Stroke.Size:=2;
end;

procedure TSheet.PaintCell(const Sender: TColumn; var AData: TRenderData;
  var DefaultPaint: Boolean);
var tmp : TRender;
begin
  tmp:=Data.HasRender(Sender,AData.Row);

  DefaultPaint:=tmp=nil;

  if DefaultPaint then
  begin
    if AData.Text<>'' then
       AData.Painter.SetFont(IGrid.Cells.Format.Font);
  end
  else
  begin
    if tmp is TFormatRender then
       AData.Painter.SetFont(TFormatRender(tmp).Format.Font);

    tmp.Paint(AData);
  end;
end;

procedure TSheet.PaintRowNumber(const Sender:TColumn; var AData:TRenderData; var DefaultPaint:Boolean);
begin
  DefaultPaint:=True;

  (Sender.Render as TFormatRender).Borders.Right.InitVisible(AData.Row=IGrid.Selected.Row);

  if AData.Row=IGrid.Selected.Row then
     AData.Painter.Fill(AData.Rect,TColors.Darkgray);
end;

procedure TSheet.InitRowNumbers(const AColumn:TColumn);
begin
  AColumn.Locked:=TColumnLocked.Left;
  AColumn.Width.Value:=30;
  AColumn.Selectable:=False;

  AColumn.TextAlignment:=TColumnTextAlign.Custom;
  AColumn.TextAlign.Horizontal:=THorizontalAlign.Center;

  InitSheetBorder((AColumn.Render as TFormatRender).Borders.Right);

  AColumn.ParentFormat:=False;

  AColumn.OnPaint:=PaintRowNumber;
end;

function TSheet.CreateData: TSheetData;

  function HeaderText(ACol:Integer):String;
  const
    A=Ord('A');
  begin
    result:=Chr(A+(ACol mod 26));

    while ACol>25 do
    begin
      result:=Chr(A+(ACol div 26)-1)+result;
      ACol:=ACol div 26;
    end;
  end;

var col : Integer;
begin
  result:=TSheetData.Create(100,10000,60);

  for col:=1 to result.Columns-1 do
      result.Headers[col]:=HeaderText(col-1);

  result.NewExpression:=NewExpression;
  result.GridColumns:=IGrid.Columns;
end;

type
  TCustomTeeControlAccess=class(TCustomTeeControl);

function TSheet.CurrentCell: TSheetCell;
var tmpCol : TColumn;
    tmpRow : Integer;
begin
  tmpCol:=IGrid.Selected.Column;
  tmpRow:=IGrid.Selected.Row;

  if (tmpCol<>nil) and (tmpRow<>-1) then
     result:=FData.SheetCells.Cells[tmpCol.Index,tmpRow]
  else
     result:=nil;
end;

function TSheet.CurrentRender: TTextRender;
var tmpCol : TColumn;
    tmpRow : Integer;
    tmpRender : TRender;
begin
  tmpCol:=IGrid.Selected.Column;
  tmpRow:=IGrid.Selected.Row;

  if (tmpCol=nil) or (tmpRow=-1) then
     result:=nil
  else
  begin
    tmpRender:=FData.ForceRender(tmpCol,tmpRow,TCustomTeeControlAccess(IGrid).DoChanged);

    if tmpRender is TTextRender then
       result:=TTextRender(tmpRender)
    else
       result:=nil;
  end;
end;

procedure TSheet.FillRows;
var row : Integer;
begin
  for row:=0 to FData.Rows-1 do
      FData.Cells[0,row]:=IntToStr(row+1);
end;

procedure TSheet.Prepare(const AColumns:TColumns);

  procedure CenterText(const AHeader:TColumnHeader);
  begin
    AHeader.TextAlignment:=TColumnTextAlign.Custom;
    AHeader.TextAlign.Horizontal:=THorizontalAlign.Center;
  end;

var col : Integer;
    tmp : TColumn;
begin
  InitRowNumbers(AColumns[0]);

  // Resize column based on cells width, for visible rows only
  //Columns[0].AutoWidthVisible:=True;

  for col:=1 to AColumns.Count-1 do
  begin
    tmp:=AColumns[col];

    CenterText(tmp.Header);

    tmp.OnPaint:=PaintCell;
  end;
end;

function TSheet.Resolve(const S:String; IsFunction:Boolean):TExpression;
var tmpCol : TColumn;
    tmpRow : Integer;
begin
  if FData.Parse(S,tmpCol,tmpRow) then
     result:=TCellExpression.Create(FData,tmpCol,tmpRow)
  else
     result:=nil;
end;

function TSheet.NewExpression(const S:String):TExpression;
begin
  result:=TExpression.FromString(S,Resolve,ParseError)
end;

function TSheet.ParseError(const APos:Integer; const AMessage:String):Boolean;
begin
  result:=False;
end;


initialization
  TFont.DefaultSize:=12;
  TFont.DefaultName:='Calibri';
end.
