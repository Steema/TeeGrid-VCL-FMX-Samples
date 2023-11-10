unit Unit_Dataset;

interface

{
  Linking a TeeGrid with a TDataSource or TDataSet:

  uses Tee.GridData.DB;

  TeeGrid1.DataSource:= DataSource1;

  TeeGrid1.DataSource:= ClientDataset1;
}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCLTee.Control, VCLTee.Grid, Data.DB,
  Datasnap.DBClient, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.ExtCtrls, Vcl.Menus,
  Tee.Grid.Columns, Tee.Renders, Vcl.Buttons;

type
  TFormGridDataset = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    TeeGrid1: TTeeGrid;
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    ComboSource: TComboBox;
    Label1: TLabel;
    ClientDataSet2: TClientDataSet;
    DataSource2: TDataSource;
    Button2: TButton;
    ClientDataSet3: TClientDataSet;
    DataSource3: TDataSource;
    ClientDataSet3Name: TStringField;
    ClientDataSet3Height: TSingleField;
    ClientDataSet3Address: TStringField;
    ClientDataSet3Children: TIntegerField;
    PopupMenu1: TPopupMenu;
    BenchmarkScrolling1: TMenuItem;
    ClientDataSet3Password: TStringField;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BenchmarkScrolling1Click(Sender: TObject);
  private
    { Private declarations }

    procedure CheckBigDataset;

    procedure PaintPassword(const Sender:TColumn; var AData:TRenderData;
                            var DefaultPaint:Boolean);
  public
    { Public declarations }
  end;

var
  FormGridDataset: TFormGridDataset;

implementation

{$R *.dfm}

uses
  {System.}Diagnostics,

  VCLTee.Editor.Grid, Tee.Grid.RowGroup, Tee.Grid;

// Show the TeeGrid editor dialog
procedure TFormGridDataset.Button1Click(Sender: TObject);
begin
  TTeeGridEditor.Edit(Self,TeeGrid1);
end;

// Open or close the dataset
procedure TFormGridDataset.Button2Click(Sender: TObject);
begin
  TeeGrid1.Grid.Copy;
end;

procedure TFormGridDataset.CheckBox1Click(Sender: TObject);
begin
  ClientDataSet1.Active:=CheckBox1.Checked;
end;

// Make sure our sample ClientDataSet3 is filled with data
procedure TFormGridDataset.CheckBigDataset;

  function RandomPassword:String;
  const RandomPasswords:Array[0..3] of String=('1234','abracadabra','qwerty','dragon');
  begin
    result:=RandomPasswords[Random(1+High(RandomPasswords))];
  end;

  procedure AddSampleRecords;
  var t : Integer;
  begin
    for t:=1 to 10000 do
        ClientDataSet3.AppendRecord(['Abc',3.45,'Some St',t,RandomPassword]);
  end;

begin
  if ClientDataSet3.IsEmpty then
  begin
    Screen.Cursor:=crHourGlass;
    try
      ClientDataSet3.DisableControls;
      try
        ClientDataSet3.Close;
        ClientDataSet3.CreateDataSet;

        ClientDataSet3.Open;

        AddSampleRecords;

        ClientDataSet3.First;
      finally
        ClientDataSet3.EnableControls;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure ChangeRender(const AColumn:TColumn; const ARender:TRenderClass);
begin
  AColumn.Render:=ARender.Create(AColumn.Changed);
end;

procedure TFormGridDataset.ComboSourceChange(Sender: TObject);
begin
  // False = All rows same height
  TeeGrid1.Rows.Height.Automatic:=False;

  // 0 = Automatic row height (depends on Cells.Format.Font.Size)
  TeeGrid1.Rows.Height.Value:=0;

  case ComboSource.ItemIndex of
    0: begin
         TeeGrid1.DataSource:=nil;
         DBNavigator1.DataSource:=nil;
       end;

    1: begin
         TeeGrid1.DataSource:=ClientDataSet1;
         DBNavigator1.DataSource:=DataSource1;
       end;

    2: begin
         TeeGrid1.DataSource:=ClientDataSet2;

         // Just a test, set a custom row Height
         //TeeGrid1.Rows.Height.Automatic:=False;
         TeeGrid1.Rows.Height.Value:=100;

         DBNavigator1.DataSource:=DataSource2;
       end;

    3: begin
         // "Automatic=True" means row height will be recalculated for
         // every row, so considering possible multi-line text in cells
         TeeGrid1.Rows.Height.Automatic:=True;

         TeeGrid1.DataSource:=DataSource1;
         DBNavigator1.DataSource:=DataSource1;
       end;
  else
    begin
      CheckBigDataSet;

      TeeGrid1.DataSource:=DataSource3;
      DBNavigator1.DataSource:=DataSource3;

      // Use a render for password column:
      ChangeRender(TeeGrid1.Columns['Password'],TPasswordRender);

      // Alternative way:
      // TeeGrid1.Columns['Password'].OnPaint:=PaintPassword;
    end;
  end;

  TeeGrid1.SetFocus;
end;

procedure TFormGridDataSet.PaintPassword(const Sender:TColumn; var AData:TRenderData;
              var DefaultPaint:Boolean);
begin
  AData.Data:='######';
  DefaultPaint:=True;
end;

procedure TFormGridDataset.FormCreate(Sender: TObject);
begin
  // Enable drag / pan grid scrolling by finger touch and / or left-button mouse
  TeeGrid1.Scrolling.Mode:=TScrollingMode.Both;
//  TeeGrid1.Scrolling.Horizontal:=TScrollDirection.Disabled;

  // Change the selected row when scrolling the grid
  //TeeGrid1.Selected.ScrollToView:=True;

  TeeGrid1.DataSource:=DataSource1;
end;

// Internal Test. Scrolling / repainting speed
type
  TRowGroupAccess=class(TRowGroup);

procedure TFormGridDataset.BenchmarkScrolling1Click(Sender: TObject);
var t1 : TStopWatch;
    t,
    tmp : Integer;
    Y,
    tmpHeight : Single;
begin
  t1:=TStopwatch.StartNew;
  try
    tmp:=TeeGrid1.Data.Count;

    Y:=0;
    tmpHeight:=TeeGrid1.Rows.UpToRowHeight(1);

    for t:=0 to tmp-1 do
    begin
      TRowGroupAccess(TeeGrid1.Grid.Current).Scroll(0,Y);

      Y:=Y+tmpHeight;
    end;
  finally
    Caption:=t1.ElapsedMilliseconds.ToString;
  end;
end;

end.
