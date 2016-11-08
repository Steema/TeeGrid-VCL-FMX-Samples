unit Tee.Grid.Themes;

interface

uses
  Tee.Grid;

type
  TiOSGridTheme=record
  public
    class procedure ApplyTo(const AGrid:TCustomTeeGrid); static;
  end;

implementation
