unit Tee.Cell.Expression;

interface

uses
  Tee.Grid.Columns, Tee.Sheet.Data,
  BI.Expression;

type
  TCellExpression=class(TExpression)
  private
    IData : TSheetData;
  public
    Column : TColumn;
    Row : Integer;

    Constructor Create(const AData:TSheetData; const AColumn:TColumn; const ARow:Integer);

    procedure Assign(const Source:TExpression); override;
    function Value:TData; override;
    function ToString:String; override;
  end;

implementation

uses
  {System.}SysUtils;

{ TCellExpression }

Constructor TCellExpression.Create(const AData:TSheetData; const AColumn: TColumn; const ARow: Integer);
begin
  inherited Create;

  IData:=AData;
  Column:=AColumn;
  Row:=ARow;
end;

procedure TCellExpression.Assign(const Source: TExpression);
begin
  if Source is TCellExpression then
  begin
    Column:=TCellExpression(Source).Column;
    Row:=TCellExpression(Source).Row;
  end;

  inherited;
end;

function TCellExpression.ToString: String;
begin
  result:=Column.Header.Text+IntToStr(Row+1);
end;

function TCellExpression.Value: TData;
var tmp : String;
    tmpValue : Extended;
begin
  tmp:=IData.Cells[Column.Index,Row];

  if TryStrToFloat(tmp,tmpValue) then
     result:=tmpValue
  else
     result:=tmp;
end;

end.
