unit Tee.Sheet;

interface

{
   Common unit (VCL, Firemonkey and Lazarus LCL)
}

uses
  Tee.Grid, Tee.Grid.Columns, Tee.Format, Tee.Grid.Data.Strings,
  BI.Expression;

type
  TSheetData=class(TStringsData)
  private
    IFormulas : Array of TExpressions;

    function GetFormula(const Column, Row: Integer): String;
    procedure SetFormula(const Column, Row: Integer; const Value: String);
  protected
    function GetCell(const AColumn,ARow: Integer): String; override;
    procedure InternalResize; override;
    procedure SetCell(const AColumn,ARow: Integer; const Value: String); override;
  public
    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property Formulas[const Column,Row:Integer]:String read GetFormula write SetFormula;
  end;

  TSheet=class
  private
    FData : TSheetData;

    IGrid : TCustomTeeGrid;

    function CreateData:TSheetData;
    procedure FillRows;
    procedure InitHeaderFormat(const AFormat:TTextFormat);
    procedure InitRowNumbers(const AColumn:TColumn);
    procedure InitSelectedFormat(const AFormat:TTextFormat);
    procedure PrepareHeaders(const AColumns:TColumns);
  public
    Constructor Create(const AGrid:TCustomTeeGrid);

    property Data:TSheetData read FData;
    property Grid:TCustomTeeGrid read IGrid;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.UITypes,

  Tee.Renders, Tee.Painter;

{ TSheet }

Constructor TSheet.Create(const AGrid:TCustomTeeGrid);
begin
  inherited Create;

  IGrid:=AGrid;

  FData:=CreateData;
  IGrid.Data:=FData;

  InitRowNumbers(IGrid.Columns[0]);

  // Resize column based on cells width, for visible rows only
  //Columns[0].AutoWidthVisible:=True;

  FillRows;
  PrepareHeaders(IGrid.Columns);

  InitHeaderFormat(IGrid.Header.Format);

  IGrid.Rows.Back.Stroke.Show;

  InitSelectedFormat(IGrid.Selected.Format);

  // Advance to next row when pressing the enter key in editing mode
  IGrid.Editing.EnterKey:=TEditingEnter.NextRow;

  // Automatically start the cell editing mode when pressing a letter or number key
  IGrid.Editing.AutoEdit:=True;
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

procedure TSheet.InitRowNumbers(const AColumn:TColumn);
begin
  AColumn.Locked:=TColumnLocked.Left;
  AColumn.Width.Value:=30;
  AColumn.Selectable:=False;

  AColumn.TextAlignment:=TColumnTextAlign.Custom;
  AColumn.TextAlign.Horizontal:=THorizontalAlign.Center;

  AColumn.ParentFormat:=False;
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
end;

procedure TSheet.FillRows;
var row : Integer;
begin
  for row:=0 to FData.Rows-1 do
      FData.Cells[0,row]:=IntToStr(row+1);
end;

procedure TSheet.PrepareHeaders(const AColumns:TColumns);

  procedure CenterText(const AHeader:TColumnHeader);
  begin
    AHeader.TextAlignment:=TColumnTextAlign.Custom;
    AHeader.TextAlign.Horizontal:=THorizontalAlign.Center;
  end;

var col : Integer;
begin
  for col:=1 to AColumns.Count-1 do
      CenterText(AColumns[col].Header);
end;

{ TSheetData }

{$IFNDEF AUTOREFCOUNT}
Destructor TSheetData.Destroy;
var t,tt : Integer;
begin
  for t:=0 to High(IFormulas) do
      for tt:=0 to High(IFormulas[t]) do
          IFormulas[t,tt].Free;

  inherited;
end;
{$ENDIF}

function TSheetData.GetCell(const AColumn, ARow: Integer): String;
var tmp : TExpression;
begin
  tmp:=IFormulas[AColumn,ARow];

  if tmp=nil then
     result:=inherited
  else
     result:=tmp.AsString
end;

function TSheetData.GetFormula(const Column, Row: Integer): String;
var tmp : TExpression;
begin
  {$IFOPT R+}
  RangeCheck(Column,Row);
  {$ENDIF}

  tmp:=IFormulas[Column,Row];

  if tmp=nil then
     result:=''
  else
     result:=tmp.ToString;
end;

procedure TSheetData.InternalResize;
begin
  inherited;
  SetLength(IFormulas,Columns,Rows);
end;

procedure TSheetData.SetCell(const AColumn, ARow: Integer; const Value: String);
var tmp : String;
begin
  tmp:=Trim(Value);

  if Copy(tmp,1,1)='=' then
  begin
    Formulas[AColumn,ARow]:=Copy(Value,2,Length(Value));
  end
  else
    inherited;
end;

procedure TSheetData.SetFormula(const Column, Row: Integer;
  const Value: String);
begin
  {$IFOPT R+}
  RangeCheck(Column,Row);
  {$ENDIF}

  IFormulas[Column,Row].Free;
  IFormulas[Column,Row]:=TExpression.FromString(Value);

  Repaint;
end;

initialization
  TFont.DefaultSize:=12;
  TFont.DefaultName:='Calibri';
end.
