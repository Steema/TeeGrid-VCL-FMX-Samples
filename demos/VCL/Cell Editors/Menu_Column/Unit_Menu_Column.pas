unit Unit_Menu_Column;

{
   This example shows how to use a Combo box to edit grid column cells.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VCLTee.Control,
  VCLTee.Grid, Tee.Grid.Columns, Vcl.ExtCtrls;

type
  TForm27 = class(TForm)
    TeeGrid1: TTeeGrid;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TeeGrid1DataChanged(const Sender: TObject; const AColumn: TColumn;
      const ARow: Integer; const OldData, NewData: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form27: TForm27;

implementation

{$R *.dfm}

uses
  Tee.GridData.Strings,
  Tee.Grid.OptionRender;

procedure TForm27.FormCreate(Sender: TObject);
var Options : TOptionRender;
begin
  TeeGrid1.Data:=TCSVData.From(Memo2.Lines); // optional separator and quote

  Options:=TOptionRender.Create(TeeGrid1.DoChanged);
  Options.Items:=Memo1.Lines;

  TeeGrid1.Columns[1].Width.Value:=150;
  TeeGrid1.Columns[1].Render:=Options;
  TeeGrid1.Columns[1].EditorClass:=TComboBox;
end;

procedure TForm27.TeeGrid1DataChanged(const Sender: TObject;
  const AColumn: TColumn; const ARow: Integer; const OldData, NewData: string);
begin
  Memo2.Text:=TCSVData.ToCSVText(TeeGrid1.Data as TVirtualModeData);
end;

end.
