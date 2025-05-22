unit Unit_Pictures;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid,
  VCLTee.Picture, Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    TeeGrid1: TTeeGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

uses
  Tee.GridData.Rtti;

type
  TPic=record
    Picture:TPicture;
  end;

var RPic1, RPic2 : TPic;

procedure TForm4.FormCreate(Sender: TObject);
begin
  RPic1.Picture:=TPicture.Create;
  RPic1.Picture.LoadFromFile('steema_logo.png');

  RPic2.Picture:=TPicture.Create;
  RPic2.Picture.LoadFromFile('embarcadero_logo.png');

  TeeGrid1.Data:=TVirtualArrayData<TPic>.Create([RPic1,RPic2]);

  TeeGrid1.Rows.Height.Value:=50;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  RPic2.Picture.Free;
  RPic1.Picture.Free;
end;

end.
