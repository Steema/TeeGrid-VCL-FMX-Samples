{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Totals classes                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Totals;

interface

{
   Classes to paint "Totals" (or "SubTotals") grid bands.

   Automatic:  (all numeric visible columns as Sum)

     TeeGrid1.Footer.Add( TColumnTotals.From( TeeGrid.Data, TeeGrid1.Columns ) );

   Manual:

     var Totals : TColumnTotals;

     Totals:= TColumnTotals.Create(nil, TeeGrid1.Columns);

     Totals.Calculation.Add( TeeGrid1.Columns['Amount'], TColumnCalculation.Average );

     TeeGrid1.Footer.Add( Totals );

}

uses
  {System.}Classes,

  {$IFNDEF FPC}
  {System.}Types,
  {$ENDIF}

  Tee.Grid.Columns, Tee.Format, Tee.Renders,
  Tee.Grid.Data, Tee.Grid.Header;

type
  TBaseTotals=class(TColumnBand)
  private
    FRender : TTextRender;
  public
    Constructor Create(const AChanged:TNotifyEvent; const AColumns:TColumns); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Paint(var AData:TRenderData); override;
  end;

  TColumnTotals=class(TBaseTotals)
  private
    FData : TVirtualData;
  protected
    function AsString(const AColumn:TColumn):String; override;
  public
    type
      TTotalCalculation=record
      public
        Column: TColumn;
        Calculation : TColumnCalculation;
      end;

      TTotals=record
      private
        Items : Array of TTotalCalculation;
      public
        procedure Add(const AColumn:TColumn; const ACalculation:TColumnCalculation);
        procedure Clear; inline;
        function Count:Integer; inline;
        function Find(const AColumn:TColumn; out ACalculation:TColumnCalculation):Boolean;
      end;

    var
      Calculation : TTotals;

    Constructor Create(const AChanged:TNotifyEvent; const AColumns:TColumns; const AData:TVirtualData); reintroduce;

    // Create a Totals band automatically, with all columns that are numeric
    class function From(const AData:TVirtualData; const AColumns:TColumns):TColumnTotals; static;
  end;

  TTotalsHeader=class(TBaseTotals)
  private
    FTotals : TColumnTotals;

    procedure SetTotals(const Value: TColumnTotals);
  protected
    function AsString(const AColumn:TColumn):String; override;
  public
    Constructor CreateTotals(const ATotals:TColumnTotals);

    property Totals : TColumnTotals read FTotals write SetTotals;
  end;

implementation
