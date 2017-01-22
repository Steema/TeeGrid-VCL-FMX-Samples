unit Unit_Editors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,

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

    procedure ResetCustomEditors;
    procedure SetupCustomEditors;
  public
    { Public declarations }
  end;

var
  FormCellEditors: TFormCellEditors;

implementation

{$R *.dfm}

uses
  Unit_Example_Data, Tee.Grid, System.UITypes;

procedure TFormCellEditors.CBAlwaysVisibleClick(Sender: TObject);
begin
  // When True, moving from one cell to another will keep the cell
  // editor visible
  TeeGrid1.Editing.AlwaysVisible:=CBAlwaysVisible.Checked;
end;

procedure TFormCellEditors.CBAutoEditClick(Sender: TObject);
begin
  // When True, pressing any key will show the cell editor automatically.
  // When False, pressing F2 or double-clicking the cell starts editing.

  TeeGrid1.Editing.AutoEdit:=CBAutoEdit.Checked;
end;

procedure TFormCellEditors.CBCustomEditorsClick(Sender: TObject);
begin
  if CBCustomEditors.Checked then
     SetupCustomEditors
  else
     ResetCustomEditors;
end;

procedure TFormCellEditors.CBEnterKeyChange(Sender: TObject);
begin
  TeeGrid1.Editing.EnterKey:=TEditingEnter(CBEnterKey.ItemIndex);
end;

procedure TFormCellEditors.CBSelectedTextClick(Sender: TObject);
begin
  TeeGrid1.Editing.Text.Selected:=CBSelectedText.Checked;
end;

procedure TFormCellEditors.FormCreate(Sender: TObject);
begin
  TeeGrid1.Data:=SampleData;

  // Formatting
  TeeGrid1.Columns['Height'].DataFormat.Float:='0.##';
  TeeGrid1.Columns['BirthDate'].DataFormat.DateTime:='mm-dd-yyyy';

  // Do not show cell editor with all text selected
  TeeGrid1.Editing.Text.Selected:=False;

  SetupCustomEditors;
end;

procedure TFormCelleditors.SetupCustomEditors;
begin
  // Custom cell editor controls (default is TEdit)

  TeeGrid1.Columns['Height'].EditorClass:=TTrackBar;

//  TeeGrid1.Columns['BirthDate'].EditorClass:=TComboCalendar;

  TeeGrid1.Columns['Vehicle'].EditorClass:=TComboBox;

//  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboColor;
  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboBox;

  TeeGrid1.Columns['Holidays'].EditorClass:=TCheckBox;

  TeeGrid1.Columns['Happiness'].EditorClass:=TUpDown;
end;

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

procedure TFormCellEditors.TeeGrid1CellEditing(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);

  procedure FillCombo(const ACombo:TComboBox; const Values:Array of String);
  var t : Integer;
  begin
    ACombo.Items.BeginUpdate;
    try
      ACombo.Clear;

      for t:=Low(Values) to High(Values) do
          ACombo.Items.Add(Values[t]);
    finally
      ACombo.Items.EndUpdate;
    end;
  end;

  function FindValue(const ACombo:TComboBox; const AValue:String):Integer;
  var t : Integer;
  begin
    for t:=0 to ACombo.Items.Count-1 do
        if SameText(ACombo.Items[t],AValue) then
           Exit(t);

    result:=-1;
  end;

  function ColorOf(const AColor:TColor):String;
  begin
    case AColor of
      TColors.Black : result:='Black';
      TColors.Brown : result:='Brown';
      TColors.Green : result:='Green';
    else
      result:='Blue';
    end;
  end;

var tmpValue : String;
begin
  if AEditor is TComboBox then
  begin
    TComboBox(AEditor).Style:=csDropDownList;

    tmpValue:=TeeGrid1.Data.AsString(AColumn,ARow);

    if AColumn=TeeGrid1.Columns['Vehicle'] then
       FillCombo(TComboBox(AEditor),
         ['None','Bicycle','MotorBike','Car','Caravan','Truck','Boat','Plane'])
   else
    if AColumn=TeeGrid1.Columns['EyeColor'] then
    begin
      FillCombo(TComboBox(AEditor),
         ['Black','Brown','Green','Blue']);

      tmpValue:=ColorOf(StrToInt(tmpValue));
    end;

    TComboBox(AEditor).ItemIndex:=FindValue(TComboBox(AEditor),tmpValue);
  end;
end;

procedure TFormCellEditors.ResetCustomEditors;

  procedure ResetColumns(const AColumns:TColumns);
  var t : Integer;
  begin
    for t:=0 to AColumns.Count-1 do
    begin
      AColumns[t].EditorClass:=nil; // <-- reset to default TEdit box

      ResetColumns(AColumns[t].Items);
    end;
  end;

begin
  ResetColumns(TeeGrid1.Columns);
end;

end.
