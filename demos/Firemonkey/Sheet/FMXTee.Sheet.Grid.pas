unit FMXTee.Sheet.Grid;

interface

{
  Special FMX TeeGrid, maintains an internal "Sheet" object
}

uses
  System.Classes,
  FMXTee.Grid, Tee.Sheet;

type
  TGridSheet=class(TTeeGrid)
  private
    FSheet : TSheet;
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property Sheet:TSheet read FSheet;
  end;

implementation

{ TGridSheet }

Constructor TGridSheet.Create(AOwner: TComponent);
begin
  inherited;
  FSheet:=TSheet.Create(Grid);
end;

{$IFNDEF AUTOREFCOUNT}
Destructor TGridSheet.Destroy;
begin
  FSheet.Free;
  inherited;
end;
{$ENDIF}

end.
