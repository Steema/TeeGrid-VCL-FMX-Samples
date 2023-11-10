unit Tee.Sheet.Data;

interface

uses
  {System.}Classes,
  Tee.Renders, Tee.GridData.Strings, Tee.Grid.Columns, Tee.Painter,

  // This example requires units from "TeeBI" project (www.steema.com)
  BI.Expression;

type
  TSheetCell=class
  private
    FExpression : TExpression;
    FRender : TRender;

    ValidValue : Boolean;

    procedure SetExpression(const Value: TExpression);
    procedure SetRender(const Value: TRender);
  public
    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property Expression:TExpression read FExpression write SetExpression;
    property Render:TRender read FRender write SetRender;
  end;

  TSheetColumn=Array of TSheetCell;

  TSheetCells=class
  public
    Cells : Array of TSheetColumn;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}
  end;

  TNewExpressionEvent=function(const S:String):TExpression of object;

  TSheetData=class(TStringsData)
  private
    FCells : TSheetCells;
    FNewExpression : TNewExpressionEvent;

    function GetFormula(const AColumn, ARow: Integer): String;
    procedure Invalidate(const AColumn, ARow: Integer);
    procedure SetFormula(const AColumn, ARow: Integer; const Value: String);
    function TryCreate(const AColumn:TColumn; const ARow:Integer):TSheetCell;
  protected
    function GetCell(const AColumn,ARow: Integer): String; override;
    procedure InternalResize; override;
    procedure SetCell(const AColumn,ARow: Integer; const Value: String); override;
  public
    GridColumns : TColumns;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    function AutoHeight(const APainter:TPainter; const AColumn:TColumn; const ARow:Integer; out AHeight:Single):Boolean; override;
    function ForceRender(const AColumn:TColumn; const ARow:Integer; const AChanged:TNotifyEvent):TRender;
    function HasRender(const AColumn:TColumn; const ARow:Integer):TRender;
    function Parse(const S:String; out AColumn:TColumn; out ARow:Integer):Boolean;

    property Formulas[const AColumn,ARow:Integer]:String read GetFormula write SetFormula;
    property NewExpression:TNewExpressionEvent read FNewExpression write FNewExpression;
    property SheetCells:TSheetCells read FCells;
  end;

implementation

uses
  {System.}SysUtils,
  Tee.Cell.Expression;

{ TSheetCell }

{$IFNDEF AUTOREFCOUNT}
Destructor TSheetCell.Destroy;
begin
  FExpression.Free;
  FRender.Free;
  inherited;
end;
{$ENDIF}

procedure TSheetCell.SetExpression(const Value: TExpression);
begin
  if FExpression<>Value then
  begin
    FExpression.Free;
    FExpression:=Value;

    ValidValue:=False;
  end;
end;

procedure TSheetCell.SetRender(const Value: TRender);
begin
  FRender.Free;
  FRender:=Value;
end;

{ TSheetCells }

{$IFNDEF AUTOREFCOUNT}
Destructor TSheetCells.Destroy;
var t,tt : Integer;
begin
  for t:=0 to High(Cells) do
      for tt:=0 to High(Cells[t]) do
          Cells[t,tt].Free;

  inherited;
end;
{$ENDIF}

{ TSheetData }

{$IFNDEF AUTOREFCOUNT}
Destructor TSheetData.Destroy;
begin
  FCells.Free;
  inherited;
end;
{$ENDIF}

function TSheetData.AutoHeight(const APainter: TPainter; const AColumn: TColumn;
  const ARow: Integer; out AHeight: Single): Boolean;
var tmpCol : Integer;
    tmpCell : TSheetCell;
begin
  tmpCol:=AColumn.Index;

  tmpCell:=FCells.Cells[tmpCol,ARow];

  result:=(tmpCell<>nil) and (tmpCell.Render is TTextRender);

  if result then
     AHeight:=TTextRender(tmpCell.Render).CalcHeight(APainter,AsString(AColumn,ARow));
end;

function TSheetData.ForceRender(const AColumn: TColumn;
  const ARow: Integer; const AChanged:TNotifyEvent): TRender;
var tmp : TSheetCell;
begin
  tmp:=TryCreate(AColumn,ARow);

  if tmp.Render=nil then
     tmp.Render:=TTextRender.Create(AChanged);

  result:=tmp.Render;
end;

function TSheetData.GetCell(const AColumn, ARow: Integer): String;
var tmp : TSheetCell;
begin
  tmp:=FCells.Cells[AColumn][ARow];

  if (tmp<>nil) and (tmp.Expression<>nil) and (not tmp.ValidValue) then
  begin
    inherited SetCell(AColumn,ARow,tmp.Expression.AsString);
    tmp.ValidValue:=True;
  end;

  result:=inherited;
end;

function TSheetData.GetFormula(const AColumn, ARow: Integer): String;
var tmp : TSheetCell;
begin
  {$IFOPT R+}
  RangeCheck(AColumn,ARow);
  {$ENDIF}

  tmp:=FCells.Cells[AColumn][ARow];

  if (tmp=nil) or (tmp.Expression=nil) then
     result:=''
  else
     result:=tmp.Expression.ToString;
end;

function TSheetData.HasRender(const AColumn: TColumn;
  const ARow: Integer): TRender;
var tmp : TSheetCell;
begin

  if ARow=-1 then
     result:=nil
  else
  begin
    tmp:=FCells.Cells[AColumn.Index,ARow];

    if tmp=nil then
       result:=nil
    else
       result:=tmp.Render;
  end;
end;

procedure TSheetData.InternalResize;
begin
  inherited;

  if FCells=nil then
     FCells:=TSheetCells.Create;

  SetLength(FCells.Cells,Columns,Rows);
end;

type
  TInvalidator=class
  private
    IData : TSheetData;
    Column : TColumn;
    Row : Integer;

    TargetColumn,
    TargetRow : Integer;
  public
    procedure Invalidate(const Item:TExpression);
  end;

procedure TInvalidator.Invalidate(const Item:TExpression);
var tmp : TCellExpression;
begin
  if Item is TCellExpression then
  begin
    tmp:=TCellExpression(Item);

    if (tmp.Column=Column) and (tmp.Row=Row) then
       IData.Invalidate(TargetColumn,TargetRow);
  end;
end;

procedure TSheetData.Invalidate(const AColumn, ARow: Integer);
var col,
    row : Integer;
    tmp : TSheetCell;
    tmpInv : TInvalidator;
begin
  tmp:=FCells.Cells[AColumn,ARow];

  if tmp<>nil then
     tmp.ValidValue:=False;

  tmpInv:=TInvalidator.Create;
  try
    tmpInv.IData:=Self;
    tmpInv.Column:=ColumnList[AColumn];
    tmpInv.Row:=ARow;

    for col:=0 to Columns-1 do
        for row:=0 to Rows-1 do
            if (col<>AColumn) or (row<>ARow) then
            begin
              tmp:=FCells.Cells[col,row];

              if tmp<>nil then
                 if tmp.FExpression<>nil then
                 begin
                   tmpInv.TargetColumn:=col;
                   tmpInv.TargetRow:=row;

                   tmp.FExpression.Traverse(tmpInv.Invalidate);
                 end;
            end;
  finally
    tmpInv.Free;
  end;
end;

function IsAZ(const C:Char):Boolean;
const
  A=Ord('A');
  Z=Ord('Z');
begin
  result:=(Ord(C)>=A) and (Ord(C)<=Z);
end;

function Letters(var S:String):String;
var C : Char;
begin
  result:='';

  while S<>'' do
  begin
    C:=UpCase(S[1]);

    if IsAZ(C) then
    begin
      result:=result+C;
      Delete(S,1,1);
    end
    else
      break;
  end;
end;

function TSheetData.Parse(const S: String; out AColumn: TColumn;
  out ARow: Integer): Boolean;
var tmp,
    tmpCol : String;
begin
  tmp:=Trim(S);

  result:=tmp<>'';

  if result then
  begin
    tmpCol:=Letters(tmp);

    result:=tmpCol<>'';

    if result then
    begin
      AColumn:=GridColumns.FindFirst(tmpCol);

      result:=AColumn<>nil;

      if result then
         if TryStrToInt(tmp,ARow) then
         begin
           result:=ARow>0;

           if result then
              ARow:=ARow-1;
         end;
    end;
  end;
end;

procedure TSheetData.SetCell(const AColumn, ARow: Integer; const Value: String);
var tmp : String;
    Dummy : Extended;
begin
  tmp:=Trim(Value);

  if Copy(tmp,1,1)='=' then
  begin
    Formulas[AColumn,ARow]:=Copy(Value,2,Length(Value));
  end
  else
  begin
    inherited;

    if ColumnList[AColumn].TextAlignment=TColumnTextAlign.Automatic then
       if TryStrToFloat(GetCell(AColumn,ARow),Dummy) then
          ColumnList[AColumn].InitAlign(THorizontalAlign.Right)
       else
          ColumnList[AColumn].InitAlign(THorizontalAlign.Left);
  end;

  if AColumn>0 then
     Invalidate(AColumn,ARow);
end;

procedure TSheetData.SetFormula(const AColumn, ARow: Integer;
  const Value: String);
begin
  {$IFOPT R+}
  RangeCheck(AColumn,ARow);
  {$ENDIF}

  if FCells.Cells[AColumn,ARow]=nil then
     FCells.Cells[AColumn,ARow]:=TSheetCell.Create;

  FCells.Cells[AColumn,ARow].Expression:=NewExpression(Value);

  Invalidate(AColumn,ARow);

  Repaint;
end;

function TSheetData.TryCreate(const AColumn: TColumn;
  const ARow: Integer): TSheetCell;
var tmpCol : Integer;
begin
  tmpCol:=AColumn.Index;

  result:=FCells.Cells[tmpCol,ARow];

  if result=nil then
  begin
    result:=TSheetCell.Create;
    FCells.Cells[tmpCol,ARow]:=result;
  end;
end;

end.
