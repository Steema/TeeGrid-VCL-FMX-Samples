unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, VCLTee.Grid, db, dbf; //, DateTimePicker;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    Panel1: TPanel;
    TeeGrid1: TTeeGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  VCLTee.Editor.Grid;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Not necessary, DataSource property already set at design-time:
  // TeeGrid1.Data:=TVirtualDBData.From(DataSource1);

  // Increase speed when scrolling a grid with a dataset:
  TeeGrid1.Selected.ScrollToView:=True;

  // Open table:
  {$IFDEF LINUX}
  Dbf1.FilePath:='./';
  {$ENDIF}
  Dbf1.TableName:='disco.dbf';
  Dbf1.Open;

  // Testing cell editor:
//  TeeGrid1.Columns.FindFirst('Last_Sell').EditorClass:=TDateTimePicker;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

end.

