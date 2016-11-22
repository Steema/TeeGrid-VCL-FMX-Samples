{*********************************************}
{  TeeGrid Software Library                   }
{  CSV Data Export                            }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.CSV;

interface

{
  Returns a string with all cell contents from a Grid Selected range, in
  CSV (comma separated values) format.

  Supports single cell and range-selection.

  Example:

    var S : String;

    S:= TCSVData.From( TeeGrid1.Grid, TeeGrid1.Selected );
}

uses
  Tee.Grid, Tee.Grid.Selection;

type
  TCSVData=record
  public
    class function From(const AGrid:TCustomTeeGrid; const ASelected:TGridSelection):String; static;
  end;

implementation
