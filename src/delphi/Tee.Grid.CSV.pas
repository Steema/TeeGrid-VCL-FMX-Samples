{*********************************************}
{  TeeGrid Software Library                   }
{  CSV Data Export                            }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.CSV;
{$I Tee.inc}

interface

{
  Returns a string with all cell contents from a Grid Selected range, in
  CSV (comma separated values) format.

  Supports single cell and range-selection.

  Example:

    var S : String;

    S:= TCSVData.From( TeeGrid1.Grid );  // <-- whole grid

    S:= TCSVData.From( TeeGrid1.Grid, TeeGrid1.Selected );

    S:= TCSVData.From( TeeGrid1.Grid, TeeGrid1.Selected, #9, #10, '"' );
}

uses
  Tee.Grid, Tee.Grid.Selection;

type
  TCSVData=record
  public
    class function From(const AGrid:TCustomTeeGrid;
                        const ASelected:TGridSelection=nil;
                        const ASeparator:String=',';
                        const ALineFeed:String=#13#10;
                        const ADelimiter:String='"'):String; static;
  end;

implementation
