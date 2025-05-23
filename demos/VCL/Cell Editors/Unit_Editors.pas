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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, StrUtils,
  Vcl.ComCtrls, Vcl.Samples.Spin,

  VCLTee.Control, VCLTee.Grid, Tee.Grid.Columns;

type

  TComboBox = class(Vcl.StdCtrls.TComboBox)
  private
    FStoredItems: TStringList;
    procedure FilterItems;
    procedure StoredItemsChange(Sender: TObject);
    procedure SetStoredItems(const Value: TStringList);
    procedure CNCommand(var AMessage: TWMCommand); message CN_COMMAND;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property StoredItems: TStringList read FStoredItems write SetStoredItems;
  end;

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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label7: TLabel;
    CBSelectingEnter: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBAutoEditClick(Sender: TObject);
    procedure CBAlwaysVisibleClick(Sender: TObject);
    procedure CBEnterKeyChange(Sender: TObject);
    procedure CBCustomEditorsClick(Sender: TObject);
    procedure CBSelectedTextClick(Sender: TObject);
    procedure TeeGrid1CellEditing(const Sender: TObject;
      const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);
    procedure TeeGrid1CellEdited(const Sender: TObject; const AEditor: TControl;
      const AColumn: TColumn; const ARow: Integer;
      var ChangeData:Boolean; var NewData:String);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CBSelectingEnterChange(Sender: TObject);
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

  VCLTee.Painter,

  Tee.Grid, System.UITypes, Tee.Grid.Rows, Tee.Format, Tee.Grid.Selection;

procedure TFormCellEditors.Button1Click(Sender: TObject);
var cell : TCell;
    fontstyles :TFontStyles;
begin
  fontstyles := [TFontStyle.fsBold,TFontStyle.fsStrikeOut];

  cell := TeeGrid1.CellFormat.AddCell(1, TeeGrid1.Columns[1].Index);

  cell.Format.Font.Style := fontstyles;
  cell.Format.Brush.Color := clYellow;
  cell.Format.Font.Color := TColors.Red;
  cell.Format.Brush.Show;

  TeeGrid1.Invalidate;
end;

procedure TFormCellEditors.Button2Click(Sender: TObject);
var cell : TCell;
    fontstyles :TFontStyles;
begin
  fontstyles := [TFontStyle.fsBold,TFontStyle.fsItalic];

  cell := TeeGrid1.CellFormat.Cell[2, 1];
  cell.Format.Font.Style := fontstyles;
  cell.Format.Brush.Color := clYellow;
  cell.Format.Font.Color := TColors.Blue;
  cell.Format.Brush.Show;

  TeeGrid1.Invalidate;
end;

procedure TFormCellEditors.Button3Click(Sender: TObject);
var row : TRow;
begin
   row:=TeeGrid1.Rows.Items[3];

   row.Format.Brush.Show;
   row.Format.Brush.Color:=clWebTan;
   row.Format.Font.Color:=clRed;

   TeeGrid1.Invalidate;
end;

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

procedure TFormCellEditors.CBSelectingEnterChange(Sender: TObject);
begin
  // Several options when pressing the Enter key while NOT editing a cell (selecting cells only)
  TeeGrid1.Selected.EnterKey:=TSelectingEnter(CBSelectingEnter.ItemIndex);
end;

procedure TFormCellEditors.FormCreate(Sender: TObject);
begin
// Example, switch from Windows GDI+ graphics to GDI:
//  TeeGrid1.Painter:=TGdiPainter.Create(TeeGrid1.Canvas);

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

// Event called when a cell editor exits.
// (By pressing the Enter key, or up/down arrow keys,
// or clicking on another cell).
procedure TFormCellEditors.TeeGrid1CellEdited(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer;
                             var ChangeData:Boolean;
                             var NewData:String);
var tmpValue : String;
    tmp : Single;
begin
  // Example, retrieve position from TrackBar
  if AColumn=TeeGrid1.Columns['Height'] then
  begin
    if AEditor is TTrackBar then
    begin
      tmp:=0.01*TTrackBar(AEditor).Position;

      TeeGrid1.Data.SetValue(AColumn,ARow,FloatToStr(tmp));

      // Set to False, do not change grid cell data
      ChangeData:=False;
    end;
  end
  else
  if AEditor is TComboBox then
  begin
    tmpValue:=SelectedCombo(TComboBox(AEditor));

    if AColumn=TeeGrid1.Columns['EyeColor'] then
       tmpValue:=IntToStr(ColorFromString(tmpValue));

    //decide what to do if empty value entered.
    if tmpValue = '' then
      tmpValue := 'none';

    TeeGrid1.Refresh;

    TeeGrid1.Data.SetValue(AColumn,ARow,tmpValue);

    // Set to False, do not change grid cell data
    ChangeData:=False;
  end;
end;

// Event called whenever a cell editor is displayed
procedure TFormCellEditors.TeeGrid1CellEditing(const Sender: TObject;
  const AEditor: TControl; const AColumn: TColumn; const ARow: Integer);

  procedure SetupTrackBar(const ATrack:TTrackBar);
  begin
    ATrack.Min:=400;
    ATrack.Max:=700;
    ATrack.Frequency:=50;

    ATrack.Position:=Round(100*TeeGrid1.Data.AsFloat(AColumn,ARow));
  end;

var tmpValue : String;
begin
  // Example, custom comboboxes as cell editors
  if AEditor is TComboBox then
  begin
    tmpValue:=TeeGrid1.Data.AsString(AColumn,ARow);

    TComboBox(AEditor).Style:=csDropDown; //csDropDownList;

    if AColumn=TeeGrid1.Columns['Vehicle'] then
    Begin
       FillCombo(TComboBox(AEditor),
                 ['None','Bicycle','MotorBike','Car','Caravan','Truck','Boat','Plane'],
                 tmpValue);

       //setting up stored items offers a filter at runtime, taking characters entered
       //in the field as a filter on the list
       TComboBox(AEditor).StoredItems.BeginUpdate;
       TComboBox(AEditor).StoredItems.Clear;
       TComboBox(AEditor).StoredItems.AddStrings(TComboBox(AEditor).Items);
       TComboBox(AEditor).StoredItems.EndUpdate;
    end
    else
    if AColumn=TeeGrid1.Columns['EyeColor'] then
       FillCombo(TComboBox(AEditor),
                 ['Black','Brown','Green','Blue'],
                 ColorOf(StrToInt(tmpValue)));
  end;

  // Example, use a trackbar as cell editor
  if AColumn=TeeGrid1.Columns['Height'] then
     if AEditor is TTrackBar then
        SetupTrackBar(TTrackBar(AEditor));
end;

{ TComboBox }

constructor TComboBox.Create(AOwner: TComponent);
begin
  inherited;
  AutoComplete := False;
  FStoredItems := TStringList.Create;
  FStoredItems.OnChange := StoredItemsChange;
end;

destructor TComboBox.Destroy;
begin
  FStoredItems.Free;
  inherited;
end;

procedure TComboBox.CNCommand(var AMessage: TWMCommand);
begin
  inherited;

  if AMessage.NotifyCode = CBN_EDITUPDATE then
    // fill the items with the matches
    FilterItems;
end;

procedure TComboBox.FilterItems;
var
  I: Integer;
  Selection: TSelection;
begin
  // store the current combo edit selection
  SendMessage(Handle, CB_GETEDITSEL, WPARAM(@Selection.StartPos),
    LPARAM(@Selection.EndPos));

  //start filter cycle
  Items.BeginUpdate;
  try
    // if the combo edit is not empty, then clear the items
    // and search through the FStoredItems
    if Text <> '' then
    begin
      // clear all items
      Items.Clear;

      for I := 0 to FStoredItems.Count - 1 do
        if ContainsText(FStoredItems[I], Text) then
          Items.Add(FStoredItems[I]); //resulting matched items
    end
    else
      // if no match, ie. empty filter, add all items.
      Items.Assign(FStoredItems)
  finally
    Items.EndUpdate;
  end;
  // restore the last combo edit selection
  SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Selection.StartPos,
    Selection.EndPos));
end;

procedure TComboBox.StoredItemsChange(Sender: TObject);
begin
  if Assigned(FStoredItems) then
    FilterItems;
end;

procedure TComboBox.SetStoredItems(const Value: TStringList);
begin
  if Assigned(FStoredItems) then
    FStoredItems.Assign(Value)
  else
    FStoredItems := Value;
end;

end.
