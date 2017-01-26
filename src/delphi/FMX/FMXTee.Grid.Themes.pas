{*********************************************}
{  TeeGrid Software Library                   }
{  Firemonkey Themes / Styles support         }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Grid.Themes;
{$I Tee.inc}

interface

uses
  {System.}Classes,
  FMXTee.Grid;

type
  TFMXGridThemes=record
  public
    class procedure ApplyTo(const AGrid:TTeeGrid); static;
    class procedure Available(const AItems:TStrings); static;
    class function StylesPath:String; static;
  end;

implementation
