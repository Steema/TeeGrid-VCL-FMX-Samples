unit Unit_Editors;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls,

  VCLTee.Control, VCLTee.Grid;

type
  TFormCellEditors = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCellEditors: TFormCellEditors;

implementation

{$R *.dfm}

uses
  Unit_Example_Data;

procedure TFormCellEditors.FormCreate(Sender: TObject);
begin
  // Pressing any key will show the cell editor automatically
//  TeeGrid1.Editing.AutoEdit:=True;

  // Moving from one cell to another will keep the cell editor visible
//  TeeGrid1.Editing.AlwaysVisible:=True;

  TeeGrid1.Data:=SampleData;

  // Formatting
  TeeGrid1.Columns['Height'].DataFormat.Float:='0.##';
  TeeGrid1.Columns['BirthDate'].DataFormat.DateTime:='mm-dd-yyyy';

  // Custom cell editor controls (default is TEdit)

  TeeGrid1.Columns['Height'].EditorClass:=TTrackBar;

//  TeeGrid1.Columns['BirthDate'].EditorClass:=TComboCalendar;

  TeeGrid1.Columns['Vehicle'].EditorClass:=TComboBox;

//  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboColor;
  TeeGrid1.Columns['EyeColor'].EditorClass:=TComboBox;

  TeeGrid1.Columns['Holidays'].EditorClass:=TCheckBox;

  TeeGrid1.Columns['Happiness'].EditorClass:=TUpDown;
end;

end.
