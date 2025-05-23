unit Unit_Editors;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMXTee.Control,
  FMXTee.Grid, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,

  FMX.DateTimeCtrls, FMX.ListBox, FMX.Colors, FMX.NumberBox, FMX.Text,

  Tee.Grid.Columns;

type
  TFormCellEditors = class(TForm)
    TeeGrid1: TTeeGrid;
    Layout1: TLayout;
    CBCustomEditors: TCheckBox;
    Text1: TText;
    Layout2: TLayout;
    Text2: TText;
    CBAutoEdit: TCheckBox;
    Text3: TText;
    CBAlwaysVisible: TCheckBox;
    Text4: TText;
    CBSelectedText: TCheckBox;
    Text5: TText;
    Label1: TLabel;
    CBEnterKey: TComboBox;
    Label2: TLabel;
    CBSelectingEnterKey: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBCustomEditorsChange(Sender: TObject);
    procedure CBAutoEditChange(Sender: TObject);
    procedure CBAlwaysVisibleChange(Sender: TObject);
    procedure CBSelectedTextChange(Sender: TObject);
    procedure CBEnterKeyChange(Sender: TObject);
    procedure TeeGrid1CellEditing(const Sender: TObject;
      const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);
    procedure TeeGrid1CellEdited(const Sender: TObject; const AEditor: TControl;
      const AColumn: TColumn; const ARow: Integer; var ChangeData: Boolean;
      var NewData: string);
    procedure CBSelectingEnterKeyChange(Sender: TObject);
  private
    { Private declarations }

    procedure SetupCustomEditors;
  public
    { Public declarations }
  end;

var
  FormCellEditors: TFormCellEditors;

implementation

{$R *.fmx}

uses
  Unit_Example_Data, Unit_Utils,

  Tee.Grid, Tee.Grid.Selection;

procedure TFormCellEditors.CBAlwaysVisibleChange(Sender: TObject);
begin
  // When True, moving from one cell to another will keep the cell editor visible
  TeeGrid1.Editing.AlwaysVisible:=CBAlwaysVisible.IsChecked;
end;

procedure TFormCellEditors.CBAutoEditChange(Sender: TObject);
begin
  // When True, pressing any key will show the cell editor automatically.
  // When False, only pressing F2 or double-clicking the cell starts editing.

  TeeGrid1.Editing.AutoEdit:=CBAutoEdit.IsChecked;
end;

procedure TFormCellEditors.CBCustomEditorsChange(Sender: TObject);
begin
  if CBCustomEditors.IsChecked then
     SetupCustomEditors
  else
     ResetCustomEditors(TeeGrid1);
end;

procedure TFormCellEditors.CBEnterKeyChange(Sender: TObject);
begin
  // Several options when pressing the Enter key while editing a cell
  TeeGrid1.Editing.EnterKey:=TEditingEnter(CBEnterKey.ItemIndex);
end;

procedure TFormCellEditors.CBSelectedTextChange(Sender: TObject);
begin
  // When True, editing a cell using a TEdit will select all text
  TeeGrid1.Editing.Text.Selected:=CBSelectedText.IsChecked;
end;

procedure TFormCellEditors.CBSelectingEnterKeyChange(Sender: TObject);
begin
  TeeGrid1.Selected.EnterKey:=TSelectingEnter(CBSelectingEnterKey.ItemIndex);
end;

procedure TFormCellEditors.FormCreate(Sender: TObject);
begin
  // Use random sample data
  TeeGrid1.Data:=SampleData;

  // Set some column Formatting strings
  TeeGrid1.Columns['Height'].DataFormat.Float:='0.##';
  TeeGrid1.Columns['BirthDate'].DataFormat.DateTime:='mm-dd-yyyy';

  // Do not show cell editor with all text selected by default
  TeeGrid1.Editing.Text.Selected:=False;

  SetupCustomEditors;
end;

procedure TFormCellEditors.SetupCustomEditors;
begin
  // Custom cell editor controls (default is TEdit):

  TeeGrid1.Columns['Height'].EditorClass:=TTrackBar;

  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateEdit;

  TeeGrid1.Columns['Vehicle'].EditorClass:=TComboBox;

  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboColorBox;

  TeeGrid1.Columns['Holidays'].EditorClass:=TCheckBox;

  TeeGrid1.Columns['Happiness'].EditorClass:=TNumberBox;
end;

procedure TFormCellEditors.TeeGrid1CellEdited(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer;
  var ChangeData: Boolean; var NewData: string);
var tmp : String;
begin
  if AColumn=TeeGrid1.Columns['Height'] then
  begin
    if AEditor is TTrackBar then
    begin
      ChangeData:=False;

      tmp:=FloatToStr(TTrackBar(AEditor).Value*0.01);

      TeeGrid1.Data.SetValue(AColumn,ARow,tmp);
    end;
  end;
end;

procedure TFormCellEditors.TeeGrid1CellEditing(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);
var tmpValue : String;
begin
  if AEditor is TDateEdit then
     TDateEdit(AEditor).DateTime:=TeeGrid1.Data.AsFloat(AColumn,ARow);

  if AEditor is TComboBox then
  begin
    tmpValue:=TeeGrid1.Data.AsString(AColumn,ARow);

    if AColumn=TeeGrid1.Columns['Vehicle'] then
       FillCombo(TComboBox(AEditor),
                 ['None','Bicycle','MotorBike','Car','Caravan','Truck','Boat','Plane'],
                 tmpValue);
  end
  else
  if (AColumn=TeeGrid1.Columns['EyeColor']) and (AEditor is TComboColorBox)  then
     TComboColorBox(AEditor).Color:=Round(TeeGrid1.Data.AsFloat(AColumn,ARow))
  else
  if (AColumn=TeeGrid1.Columns['Height']) and (AEditor is TTrackBar) then
  begin
    TTrackBar(AEditor).Min:=400;
    TTrackBar(AEditor).Max:=700;
    TTrackBar(AEditor).Value:=100*TeeGrid1.Data.AsFloat(AColumn,ARow);
  end
  else
  if (AColumn=TeeGrid1.Columns['Happiness']) and (AEditor is TNumberBox) then
  begin
    TNumberBox(AEditor).ValueType:=TNumValueType.Float;
    TNumberBox(AEditor).Min:=0;
    TNumberBox(AEditor).Max:=1;
    TNumberBox(AEditor).DecimalDigits:=2;
  end;
end;

end.
