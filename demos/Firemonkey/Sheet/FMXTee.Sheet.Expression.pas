unit FMXTee.Sheet.Expression;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox,

  Tee.Sheet, FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit, Tee.Grid.Columns;

type
  TSheetExpression = class(TForm)
    CBCurrent: TComboEdit;
    Splitter1: TSplitter;
    CBFormula: TComboEdit;
    procedure CBCurrentExit(Sender: TObject);
    procedure CBCurrentChange(Sender: TObject);
    procedure CBFormulaChange(Sender: TObject);
    procedure CBFormulaExit(Sender: TObject);
    procedure CBFormulaKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }

    IChanging : Boolean;

    Sheet : TSheet;

    FOnChangeCurrent : TNotifyEvent;

    function SelectedCellText:String;
    function SelectedColumn:TColumn;
    function SelectedToString:String;
    function TryChangeSelected:Boolean;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TControl):TSheetExpression; static;

    procedure Refresh(const ASheet:TSheet);

    procedure RefreshFormula;
    procedure RefreshSelected;

    property OnChangeCurrent:TNotifyEvent read FOnChangeCurrent write FOnChangeCurrent;
  end;

implementation

{$R *.fmx}

uses
  FMXTee.Editor.Painter.Stroke;

{ TSheetTools }

function TSheetExpression.TryChangeSelected:Boolean;
var tmp : String;
    tmpColumn : TColumn;
    tmpRow : Integer;
begin
  tmp:=Trim(CBCurrent.Text);

  result:=Sheet.Data.Parse(tmp,tmpColumn,tmpRow);

  if result then
  begin
    Sheet.Grid.Selected.Column:=tmpColumn;
    Sheet.Grid.Selected.Row:=tmpRow;
  end;
end;

procedure TSheetExpression.CBCurrentChange(Sender: TObject);
begin
  if not IChanging then
     TryChangeSelected;
end;

procedure TSheetExpression.CBCurrentExit(Sender: TObject);
var tmp : String;
begin
  if TryChangeSelected then
  begin
    tmp:=Trim(CBCurrent.Text);

    if CBCurrent.Items.IndexOf(tmp)=-1 then
       CBCurrent.Items.Add(tmp);

    if Assigned(FOnChangeCurrent) then
       FOnChangeCurrent(Self);
  end;
end;

procedure TSheetExpression.CBFormulaChange(Sender: TObject);
begin
  if not IChanging then
     CBFormulaExit(Self);
end;

procedure TSheetExpression.CBFormulaExit(Sender: TObject);
var tmpCol : TColumn;
    tmpRow : Integer;
begin
  tmpCol:=Sheet.Grid.Selected.Column;
  tmpRow:=Sheet.Grid.Selected.Row;

  if (tmpCol<>nil) and (tmpRow<>-1) then
      Sheet.Data.Cells[tmpCol.Index,tmpRow]:=CBFormula.Text;
end;

procedure TSheetExpression.CBFormulaKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkEscape then
     CBFormula.Text:='';
end;

class function TSheetExpression.Embedd(const AOwner: TComponent;
  const AParent: TControl): TSheetExpression;
begin
  result:=TSheetExpression.Create(AOwner);
  TStrokeEditor.AddForm(result,AParent);
end;

procedure TSheetExpression.Refresh(const ASheet: TSheet);
begin
  Sheet:=ASheet;

  IChanging:=True;
  try
    RefreshSelected;
    RefreshFormula;
  finally
    IChanging:=False;
  end;
end;

function TSheetExpression.SelectedCellText:String;
var tmp : TColumn;
    tmpRow : Integer;
begin
  result:='';

  tmp:=SelectedColumn;

  if tmp<>nil then
  begin
    tmpRow:=Sheet.Grid.Selected.Row;

    if tmpRow<>-1 then
    begin
      result:=Sheet.Data.Formulas[tmp.Index,tmpRow];

      if result='' then
         result:=Sheet.Data.Cells[tmp.Index,tmpRow]
      else
         result:='='+result;
    end;
  end;
end;

procedure TSheetExpression.RefreshSelected;
var Old : Boolean;
begin
  Old:=IChanging;

  IChanging:=True;
  try
    CBCurrent.Text:=SelectedToString;
  finally
    IChanging:=Old;
  end;
end;

procedure TSheetExpression.RefreshFormula;
var Old : Boolean;
begin
  Old:=IChanging;

  IChanging:=True;
  try
    CBFormula.Text:=SelectedCellText;
  finally
    IChanging:=Old;
  end;
end;

function TSheetExpression.SelectedColumn:TColumn;
begin
  if Sheet=nil then
     result:=nil
  else
     result:=Sheet.Grid.Selected.Column;
end;

function TSheetExpression.SelectedToString:String;
var tmp : TColumn;
begin
  tmp:=SelectedColumn;

  if tmp=nil then
     result:=''
  else
  begin
    result:=tmp.Header.Text;

    if Sheet.Grid.Selected.Row<>-1 then
       result:=result+IntToStr(Sheet.Grid.Selected.Row+1);
  end;
end;

end.
