{*********************************************}
{  TeeGrid Software Library                   }
{  VCL Themes / Styles support                }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Grid.Themes;
{$I Tee.inc}

interface

uses
  {System.}Classes,
  VCLTee.Grid;

type
  TVCLGridThemes=record
  public
    class procedure ApplyTo(const AGrid:TTeeGrid); static;
    class procedure Available(const AItems:TStrings); static;
  end;

implementation
