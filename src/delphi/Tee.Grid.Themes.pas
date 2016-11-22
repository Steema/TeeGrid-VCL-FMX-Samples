{*********************************************}
{  TeeGrid Software Library                   }
{  Themes                                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Themes;
{$I Tee.inc}

interface

{
  Small helper methods to change the formatting aspect of a TeeGrid.

  Example:

    TAndroidTheme.ApplyTo( TeeGrid1.Grid );
}

uses
  Tee.Grid, Tee.Grid.RowGroup;

type
  TDefaultTheme=record
  public
    class procedure ApplyTo(const AGrid:TCustomTeeGrid); static;
  end;

  TBlackTheme=record
  public
    class procedure ApplyTo(const AGrid:TCustomTeeGrid); static;
  end;

  TiOSTheme=record
  private
  const
    TealBlue=$FAC85A;
  public
    class procedure ApplyTo(const AGroup: TRowGroup); overload; static;
    class procedure ApplyTo(const AGrid:TCustomTeeGrid); overload; static;
  end;

  TAndroidTheme=record
  public
    class procedure ApplyTo(const AGroup:TRowGroup); overload; static;
    class procedure ApplyTo(const AGrid:TCustomTeeGrid); overload; static;
  end;

  TGridThemes=record
  private
    class procedure CheckScrollBars(const AGrid:TCustomTeeGrid); static;
  public
    class var
      Default : TDefaultTheme;
      Black : TBlackTheme;
      iOS : TiOSTheme;
      Android : TAndroidTheme;
  end;

implementation
