unit Tee.Grid.OptionRender;

interface

uses
  Classes, Tee.Renders;

type
  TOptionRender=class(TTextRender)
  private
    FItems : TStrings;
    procedure SetItems(const Value: TStrings);
  protected
    procedure EditedCell(const AEditor:TComponent;
                         const ARow:Integer;
                         var NewData:String); override;
    procedure EditingCell(const AEditor:TComponent;
                          const ARow:Integer; const AData:String); override;
  public
    Constructor Create(const AChanged:TNotifyEvent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Paint(var AData:TRenderData); override;

    property Items:TStrings read FItems write SetItems;
  end;

implementation

uses
  StdCtrls;

{ TOptionRender }

Constructor TOptionRender.Create(const AChanged: TNotifyEvent);
begin
  inherited;
  FItems:=TStringList.Create;
end;

Destructor TOptionRender.Destroy;
begin
  Items.Free;
  inherited;
end;

procedure TOptionRender.EditedCell(const AEditor: TComponent;
  const ARow: Integer; var NewData: String);

  function IndexOfValue:Integer;
  var t : Integer;
  begin
    for t:=0 to Items.Count-1 do
        if Items.ValueFromIndex[t]=NewData then
        begin
          result:=t;
          Exit;
        end;

    result:=-1;
  end;

begin
  inherited;
  NewData:=Items.Names[IndexOfValue];
end;

procedure TOptionRender.EditingCell(const AEditor: TComponent;
  const ARow: Integer; const AData:String);
var Combo : TComboBox;
    t : Integer;
begin
  inherited;

  Combo:=AEditor as TComboBox;

  Combo.Items.BeginUpdate;
  try
    Combo.Clear;

    for t:=0 to Items.Count-1 do
        Combo.Items.Add(Items.ValueFromIndex[t]);

    Combo.ItemIndex:=Combo.Items.IndexOf(Items.Values[AData]);
  finally
    Combo.Items.EndUpdate;
  end;
end;

procedure TOptionRender.Paint(var AData: TRenderData);
begin
  AData.Data:=Items.Values[AData.Data];
  inherited;
end;

procedure TOptionRender.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

end.
