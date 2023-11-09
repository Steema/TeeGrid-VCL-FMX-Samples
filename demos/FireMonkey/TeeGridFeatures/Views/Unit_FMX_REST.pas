unit Unit_FMX_REST;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, FMX.StdCtrls,
  FMXTee.Control, FMXTee.Grid, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, REST.Response.Adapter, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Data.Bind.DBScope,
  Tee.Grid.Rows, Tee.Grid.Columns, Tee.GridData, Tee.Grid.RowGroup,
  Tee.Renders, Tee.Grid.Totals,
  Tee.GridData.DB, Tee.Grid, System.ImageList, FMX.ImgList, FMX.Objects,
  REST.Types;

type
  TRESTClientTeeGridForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    TeeGrid1: TTeeGrid;
    SpeedButton1: TSpeedButton;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    CBEnabled: TCheckBox;
    RESTClient2: TRESTClient;
    RESTRequest2: TRESTRequest;
    RESTResponse2: TRESTResponse;
    RESTResponseDataSetAdapter2: TRESTResponseDataSetAdapter;
    FDMemTable2: TFDMemTable;
    Rectangle1: TRectangle;
    ImageList1: TImageList;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
    procedure CBEnabledChange(Sender: TObject);
  private
    Expander : TExpanderRender;

    //procedure AddTotals;

    procedure AddRowGroups;
    procedure DetailNewGroup(const Sender,NewGroup:TRowGroup);
    procedure GetAlbums(const Sender: TExpanderRender; const ARow: Integer;
      out AData: TObject);
  public
  end;

var
  RESTClientTeeGridForm: TRESTClientTeeGridForm;

implementation

uses
  Tee.Format,
  System.JSON;

{$R *.fmx}

{
procedure TRESTClientTeeGridForm.AddTotals;
  var Totals : TColumnTotals;
begin
  Totals:= TColumnTotals.Create(TeeGrid1.Footer);

  //  showmessage(teegrid1.Columns.Items[0].ToString());
  Totals.Calculation.Add('userId', TColumnCalculation.Count);

  // Add also a band with total names
  TTotalsHeader.CreateTotals( TeeGrid1.Footer, Totals );
end;
}

procedure TRESTClientTeeGridForm.GetAlbums(const Sender: TExpanderRender; const ARow:Integer; out AData:TObject);
begin
  // Exexute the restRequest2 which returns a json with all the user albums
  RESTRequest2.Execute;
  TeeGrid1.DataSource := FDMemTable2;

  // Return a new Data using a clone of albums rows for a given userID
  AData:=TVirtualDBData.From(FDMemTable2.Fields[0]);

  // Data should be destroyed automatically
  if AData<>nil then
     TVirtualDBData(AData).OwnsData:=True;
end;

procedure TRESTClientTeeGridForm.CBEnabledChange(Sender: TObject);
begin
  if CBEnabled.IsChecked then
  begin
    // Create "Expander"
    Expander:=TeeGrid1.Grid.Current.NewExpander;

    // Setup event
    Expander.OnGetData:=GetAlbums;

    // We don't know in advance if a row can be expanded or not, so set Always
    Expander.AlwaysExpand:=True;

    // Set to first Column
    TeeGrid1.Columns[0].Render:=Expander;
  end
  else
  begin
    // Remove all detail grids
    TeeGrid1.Rows.Children.Clear;

    // Set first column render to default (no expander)
    TeeGrid1.Columns[0].Render:=nil;
  end;
end;

procedure TRESTClientTeeGridForm.DetailNewGroup(const Sender,
  NewGroup: TRowGroup);
var tmpTot : TColumnTotals;
begin
  // Create a Totals band
  tmpTot:=TColumnTotals.Create(NewGroup.Footer); // <--- set to Footer

  // Add some calculations
  tmpTot.Calculation.Add(NewGroup.Columns[0],TColumnCalculation.Count);
  tmpTot.Calculation.Add('userID',TColumnCalculation.Sum);

  // Add a Totals header:
  TTotalsHeader.CreateTotals(NewGroup.Footer,tmpTot);
end;


procedure TRESTClientTeeGridForm.FormCreate(Sender: TObject);
begin
  SpeedButton1Click(self);
end;

procedure TRESTClientTeeGridForm.RESTRequest1AfterExecute(
  Sender: TCustomRESTRequest);
begin
//  AddTotals;
  AddRowGroups;
end;

procedure TRESTClientTeeGridForm.SpeedButton1Click(Sender: TObject);
var
//  jValue:TJSONValue;
  i : integer;
begin
  RESTRequest1.Execute;

  // We can get the JSON value by using..
  //  jValue:=RESTResponse1.JSONValue;

  // Remember to set Active to try the RESTResponseDataSetAdapter, or by code
  //  RESTResponseDataSetAdapter1.Active := true;

  for i := 0 to TeeGrid1.Columns.Count-1 do
  begin
    TeeGrid1.Columns[i].Width.Automatic:= False;
    TeeGrid1.Columns[i].Width.Value:= 32;
    TeeGrid1.Columns[i].Width.Units:= TSizeUnits.Percent;  // or Pixels
  end;
end;

procedure TRestClientTeeGridForm.AddRowGroups;
begin
  // Initialize "Expander"
  CBEnabledChange(Self);

  // Optional:
  TeeGrid1.Grid.Current.OnNewDetail:=DetailNewGroup;
end;

end.
