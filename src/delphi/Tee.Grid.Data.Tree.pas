{*********************************************}
{  TeeGrid Software Library                   }
{  Tree Emulation Virtual Data class          }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.Tree;

interface

uses
  Tee.Grid, Tee.Grid.Columns, Tee.Painter, Tee.Renders, Tee.Grid.Data;

type
  PNode=^TNode;

  TNodes=Array of PNode;

  TNode=record
  public
    Data : TVirtualData;
    Text : String;
    Items : TNodes;

    function Add(const AText:String):PNode;
    function Count:Integer; inline;
  end;

  TTreeGridData=class(TVirtualData)
  private
    FItems : TNodes;

    class function NodeOf(const AColumn:TColumn):PNode; inline; static;
  protected
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure AddColumns(const AColumns:TColumns); override;
    function AsFloat(const AColumn:TColumn; const ARow:Integer):TFloat; override;
    function AsString(const AColumn:TColumn; const ARow:Integer):String; override;
    function AutoWidth(const APainter:TPainter; const AColumn:TColumn):Single; override;

    function CanExpand(const Sender:TRender; const ARow:Integer):Boolean; override;
    function CanSortBy(const AColumn:TColumn):Boolean; override;

    function Count:Integer; override;

    function GetDetail(const ARow:Integer; const AColumns:TColumns; out AParent:TColumn): TVirtualData; override;

    function IsSorted(const AColumn:TColumn; out Ascending:Boolean):Boolean; override;

    procedure Load; override;
    procedure SetValue(const AColumn:TColumn; const ARow:Integer; const AText:String); override;
    procedure SortBy(const AColumn:TColumn); override;
  end;

implementation
