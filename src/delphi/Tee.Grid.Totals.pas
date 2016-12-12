{*********************************************}
{  TeeGrid Software Library                   }
{  Grid Totals classes                        }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Totals;
{$I Tee.inc}

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
  public
    Constructor Create(ACollection:TCollection); override;

    procedure Paint(var AData:TRenderData; const ARender:TRender); override;
  end;

  TColumnTotals=class(TBaseTotals)
  protected
    function AsString(const AColumn:TColumn):String; override;
  public
    type
      // A Column total calculation
      TTotalCalculation=record // <-- convert to TCollectionItem
      public
        Column: TColumn;
        Calculation : TColumnCalculation;
      end;

      // List of calculations
      TTotals=record  // <-- convert to TCollectionChange
      private
        ITotals : TColumnTotals;
      public
        Items : Array of TTotalCalculation;

        procedure Add(const AColumn:TColumn; const ACalculation:TColumnCalculation); overload;
        procedure Add(const AColumnName:String; const ACalculation:TColumnCalculation); overload;
        procedure Clear; inline;
        function Count:Integer; inline;
        procedure Delete(const AIndex:Integer);
        function Find(const AColumn:TColumn; out ACalculation:TColumnCalculation):Boolean;
        function IndexOf(const AColumn:TColumn):Integer;
      end;

    var
      Calculation : TTotals;

    Constructor Create(ACollection:TCollection); override;
    class function Description:String; override;
  end;

  // Displays the names of a "Column Totals" band items
  TTotalsHeader=class(TBaseTotals)
  private
    FTotals : TColumnTotals;

    procedure SetTotals(const Value: TColumnTotals);
  protected
    function AsString(const AColumn:TColumn):String; override;
  public
    Constructor CreateTotals(const ACollection:TCollection; const ATotals:TColumnTotals);

    class function Description:String; override;

    property Totals : TColumnTotals read FTotals write SetTotals;
  end;

implementation
