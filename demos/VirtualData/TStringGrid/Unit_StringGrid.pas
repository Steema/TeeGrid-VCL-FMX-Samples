unit Unit_StringGrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  Vcl.ExtCtrls, Tee.Grid.RowGroup, Vcl.StdCtrls;

type
  TStringGridForm = class(TForm)
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    EColumns: TEdit;
    Label2: TLabel;
    ERows: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TeeGrid1ClickedHeader(Sender: TObject);
    procedure TeeGrid1Select(const Sender: TRowGroup);
    procedure EColumnsChange(Sender: TObject);
    procedure ERowsChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StringGridForm1: TStringGridForm;

implementation

{$R *.dfm}

uses
  Tee.Grid.Data.Strings, Tee.Grid.Columns, VCLTee.Editor.Grid;

procedure TStringGridForm.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

procedure TStringGridForm.EColumnsChange(Sender: TObject);
var tmp : Integer;
begin
  if TryStrToInt(EColumns.Text,tmp) then
     TStringsData(TeeGrid1.Data).Columns:=tmp;
end;

procedure TStringGridForm.ERowsChange(Sender: TObject);
var tmp : Integer;
begin
  if TryStrToInt(ERows.Text,tmp) then
     TStringsData(TeeGrid1.Data).Rows:=tmp;
end;

procedure TStringGridForm.FormCreate(Sender: TObject);
var Data : TStringsData;
    t : Integer;
begin
  // Create data
  Data:=TStringsData.Create;

  // Initialize size
  //Data.Columns:=1000;
  //Data.Rows:=100000;

  Data.Resize(1000,100000);

  // Set header texts
  Data.Headers[0]:='A';
  Data.Headers[1]:='B';
  Data.Headers[2]:='C';

  // Fill rows and cells
  for t:=0 to Data.Rows-1 do
  begin
    Data[0,t]:='0 '+IntToStr(t);
    Data[1,t]:='1 '+IntToStr(t);
    Data[2,t]:='2 '+IntToStr(t);
  end;

  // Set data to grid
  TeeGrid1.Data:=Data;

  // Refresh edit boxes
  EColumns.Text:=IntToStr(Data.Columns);
  ERows.Text:=IntToStr(Data.Rows);
end;

procedure TStringGridForm.TeeGrid1ClickedHeader(Sender: TObject);
begin
  Panel1.Caption:='Clicked column header: '+(Sender as TColumn).Header.Text;
end;

procedure TStringGridForm.TeeGrid1Select(const Sender: TRowGroup);
begin
  if Sender.Selected.IsEmpty then
     Panel1.Caption:=''
  else
     Panel1.Caption:='Selected cell: '+Sender.Selected.Column.Header.Text+
                     ' Row: '+IntToStr(Sender.Selected.Row)+
                     ' Value: '+Sender.Data.AsString(Sender.Selected.Column,Sender.Selected.Row);
end;

end.
