unit Unit_Editors;

interface

// This example shows how to use different custom "cell editor" controls,
// associated to grid columns.

// The grid event OnCellEditing is called before a cell editor is displayed,
// which is the appropiate place to customize the editor (for example to fill
// a combobox values).

// When the editor is finished (pressing the Enter key or moving to another cell),
// the OnCellEdited event is called, which is the place to use the editor control
// value (for example a TrackBar Position) to modify the grid data for that cell

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Samples.Spin,

  VCLTee.Control, VCLTee.Grid, Tee.Grid.Columns;

type
  TFormCellEditors = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    CBAutoEdit: TCheckBox;
    Label3: TLabel;
    CBAlwaysVisible: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    CBEnterKey: TComboBox;
    CBCustomEditors: TCheckBox;
    CBSelectedText: TCheckBox;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CBAutoEditClick(Sender: TObject);
    procedure CBAlwaysVisibleClick(Sender: TObject);
    procedure CBEnterKeyChange(Sender: TObject);
    procedure CBCustomEditorsClick(Sender: TObject);
    procedure CBSelectedTextClick(Sender: TObject);
    procedure TeeGrid1CellEditing(const Sender: TObject;
      const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);
    procedure TeeGrid1CellEdited(const Sender: TObject; const AEditor: TControl;
      const AColumn: TColumn; const ARow: Integer);
  private
    { Private declarations }

    procedure SetupCustomEditors;
  public
    { Public declarations }
  end;

var
  FormCellEditors: TFormCellEditors;

implementation

{$R *.dfm}

uses
  Unit_Example_Data, Unit_Utils,

  Tee.Grid, System.UITypes;

procedure TFormCellEditors.CBAlwaysVisibleClick(Sender: TObject);
begin
  // When True, moving from one cell to another will keep the cell editor visible
  TeeGrid1.Editing.AlwaysVisible:=CBAlwaysVisible.Checked;
end;

procedure TFormCellEditors.CBAutoEditClick(Sender: TObject);
begin
  // When True, pressing any key will show the cell editor automatically.
  // When False, only pressing F2 or double-clicking the cell starts editing.

  TeeGrid1.Editing.AutoEdit:=CBAutoEdit.Checked;
end;

procedure TFormCellEditors.CBCustomEditorsClick(Sender: TObject);
begin
  if CBCustomEditors.Checked then
     SetupCustomEditors
  else
     ResetCustomEditors(TeeGrid1);
end;

procedure TFormCellEditors.CBEnterKeyChange(Sender: TObject);
begin
  // Several options when pressing the Enter key while editing a cell
  TeeGrid1.Editing.EnterKey:=TEditingEnter(CBEnterKey.ItemIndex);
end;

procedure TFormCellEditors.CBSelectedTextClick(Sender: TObject);
begin
  // When True, editing a cell using a TEdit will select all text
  TeeGrid1.Editing.Text.Selected:=CBSelectedText.Checked;
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

// Different editor control for each column
procedure TFormCelleditors.SetupCustomEditors;
begin
  // Custom cell editor controls (default is TEdit):

  TeeGrid1.Columns['Height'].EditorClass:=TTrackBar;

  TeeGrid1.Columns['BirthDate'].EditorClass:=TDateTimePicker;

  TeeGrid1.Columns['Vehicle'].EditorClass:=TComboBox;

  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboBox;

  TeeGrid1.Columns['Holidays'].EditorClass:=TCheckBox;

  TeeGrid1.Columns['Happiness'].EditorClass:=TSpinEdit;
end;

// Event called when a cell editor exits
// (By pressing the Enter key, or up/down arrow keys,
// or clicking on another cell).
procedure TFormCellEditors.TeeGrid1CellEdited(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);
var tmp : TComboBox;
    tmpValue : String;
begin
  if AEditor is TComboBox then
  begin
    tmp:=TComboBox(AEditor);

    if tmp.ItemIndex=-1 then
       tmpValue:=''
    else
       tmpValue:=tmp.Items[tmp.ItemIndex];

    // Bug with Rtti data:
    // TeeGrid1.Data.SetValue(AColumn,ARow,tmpValue);
  end;
end;

// Event called whenever a cell editor is displayed
procedure TFormCellEditors.TeeGrid1CellEditing(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);

var tmpValue : String;
begin
  if AEditor is TDateTimePicker then
     TDateTimePicker(AEditor).DateTime:=TeeGrid1.Data.AsFloat(AColumn,ARow);

  if AEditor is TComboBox then
  begin
    tmpValue:=TeeGrid1.Data.AsString(AColumn,ARow);

    TComboBox(AEditor).Style:=csDropDownList;

    if AColumn=TeeGrid1.Columns['Vehicle'] then
       FillCombo(TComboBox(AEditor),
                 ['None','Bicycle','MotorBike','Car','Caravan','Truck','Boat','Plane'],
                 tmpValue)
    else
    if AColumn=TeeGrid1.Columns['EyeColor'] then
       FillCombo(TComboBox(AEditor),
                 ['Black','Brown','Green','Blue'],
                 ColorOf(StrToInt(tmpValue)));
  end;

  if AColumn=TeeGrid1.Columns['Height'] then
  begin
    TTrackBar(AEditor).Min:=400;
    TTrackBar(AEditor).Max:=700;
    TTrackBar(AEditor).Position:=Round(100*TeeGrid1.Data.AsFloat(AColumn,ARow));
  end;
end;

end.
