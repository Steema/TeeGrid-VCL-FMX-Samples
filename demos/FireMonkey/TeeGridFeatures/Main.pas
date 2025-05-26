unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, Data.Bind.GenData,
  Fmx.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Objects, FMX.StdCtrls, FMX.ListView, FMX.ListView.Appearances,
  FMX.Layouts, FMX.MultiView,FMX.Memo, Fmx.Bind.Navigator, System.Actions, FMX.ActnList,
  FMX.ListView.Adapters.Base, FMX.ScrollBox, FMX.Controls.Presentation,
  Appearance, Unit_Editors, Unit_Locked_Columns, Unit_Custom_Sorting,
  Master_Detail_FireDAC, Unit_DataSet,
  Unit_FMX_Themes, Unit_FMX_Ticker, Exporting, Unit_FMX_REST, System.ImageList,
  FMX.ImgList, Unit_Master_Detail_Two_Grids, ArrayData, FMX.Menus
  ;

type
   TDemo = record
     title : string;
     name  : string;
     group : byte;
   end;

type
  TMainForm = class(TForm)
    MultiView1: TMultiView;
    Layout1: TLayout;
    ListView1: TListView;
    MasterToolbar: TToolBar;
    DetailToolbar: TToolBar;
    DetailLabel: TLabel;
    MasterButton: TSpeedButton;
    ActionList1: TActionList;
    LiveBindingsBindNavigateNext1: TFMXBindNavigateNext;
    LiveBindingsBindNavigatePrior1: TFMXBindNavigatePrior;
    LayoutDemo: TLayout;
    Rectangle1: TRectangle;
    ImageList1: TImageList;
    Rectangle2: TRectangle;
    Image1: TImage;
    LVersion: TLabel;
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure FormCreate(Sender: TObject);
  private
    CurrentDemo : TCustomForm;

    procedure AddDemosToListView;
    procedure DestroyCurrentDemo;
    procedure CreateFormFromName(const FormName: string);
    procedure EmbeddForm(AParent: TControl; AForm: TCustomForm);
    procedure RegisterClasses;
  public
  end;


var
  MainForm: TMainForm;


const


  GroupHeaders = 4;
  GroupHeadersArray : array [1..GroupHeaders] of string = ('Adding Data',
                                                           'Appearance',
                                                           'Exporting',
                                                           'Other Features');

  NumDemos = 12;
  Demos : array[1..NumDemos] of TDemo =
   (
     // Adding Data
     (title : 'Arrays as Data'; name : 'TArrayAsDataForm'; group : 1),
     (title : 'DataSet'; name : 'TFormGridDataSet'; group : 1),
     (title : 'Master Detail DB FireDAC'; name : 'TMasterDetail'; group : 1),
     (title : 'Master Detail 2 Grids'; name : 'TMasterDetail2GridsForm'; group : 1),
     (title : 'REST Client'; name : 'TRESTClientTeeGridForm'; group : 1),

     // Appearance
     (title : 'Changing Appearance'; name : 'TAppearanceForm'; group : 2),
     (title : 'Themes'; name : 'TFormGridThemes'; group : 2),
     (title : 'Cell Controls'; name : 'TFormCellEditors'; group : 2),

     // Exporting
     (title : 'Export Data'; name : 'TExportingForm'; group : 3),

     // Other
     (title : 'Sorting'; name : 'TFormCustomSorting'; group : 4),
     (title : 'Locked Columns'; name : 'TLockedColumnsForm'; group : 4),
     (title : 'Ticker'; name : 'TTickerForm'; group : 4)
   ) ;

implementation

{$R *.fmx}


procedure TMainForm.AddDemosToListView;
var
  LItem: TListViewItem;
  I : integer;

  Group: Integer;

begin

  ListView1.BeginUpdate;
  try

    for Group in [1..4] do
    begin
      with ListView1.Items.Add do
      begin
        Text := GroupHeadersArray[Group];
        Purpose := TListItemPurpose.Header;
      end;

      for I := 1 to NumDemos do
      if Demos[I].group = Group then
      begin
        LItem := ListView1.Items.Add;
        LItem.Text := Demos[I].title;
        LItem.TagString := Demos[I].name;
      end;
    end;

  finally
    ListView1.EndUpdate;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterClasses;
  AddDemosToListView;
end;

procedure TMainForm.DestroyCurrentDemo;
begin
  CurrentDemo.Free;
  CurrentDemo:=nil;

  while LayoutDemo.ChildrenCount>0 do
    LayoutDemo.Children[0].Free;
end;

procedure TMainForm.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
var s : string;
begin
  MultiView1.HideMaster;

  DestroyCurrentDemo;

  s := AItem.TagString;
  DetailLabel.Text := AItem.Text;

  CreateFormFromName(s);
end;

procedure TMainForm.RegisterClasses;
begin
  RegisterClass(TAppearanceForm);
  RegisterClass(TFormCellEditors);
  RegisterClass(TFormGridDataSet);
  RegisterClass(TMasterDetail);
  RegisterClass(TLockedColumnsForm);
  RegisterClass(TFormCustomSorting);
  RegisterClass(TFormGridThemes);
  RegisterClass(TTickerForm);
  RegisterClass(TExportingForm);
  RegisterClass(TRESTClientTeeGridForm);
  RegisterClass(TMasterDetail2GridsForm);
  RegisterClass(TArrayAsDataForm);
end;

procedure TMainForm.CreateFormFromName(const FormName : string);
var ObjClass: TFmxObjectClass;
begin
  ObjClass := TFmxObjectClass(GetClass(FormName));

  if ObjClass <> nil then
  begin
    CurrentDemo:= ObjClass.Create(Self) as TCustomForm;

    if Assigned(CurrentDemo) then
       EmbeddForm(LayoutDemo,CurrentDemo);
  end
end;

// AParent can be any control, such as a panel or a tabsheet item of a TabControl.
procedure TMainForm.EmbeddForm(AParent:TControl; AForm:TCustomForm);
begin
  while AForm.ChildrenCount>0 do
    AForm.Children[0].Parent:=AParent;
end;

end.
