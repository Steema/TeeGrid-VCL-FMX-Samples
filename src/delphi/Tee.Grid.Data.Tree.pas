{*********************************************}
{  TeeGrid Software Library                   }
{  Tree Emulation Virtual Data class          }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Data.Tree;

interface

uses
  {System.}Classes,
  Tee.Grid, Tee.Grid.Columns, Tee.Painter, Tee.Renders, Tee.Grid.Data;

type
  TTreeColumn=class;

  PNode=^TNode;

  TNodes=record
  private
    Nodes : Array of PNode;

    Owner : PNode;
    Tree : TTreeColumn;

    procedure FreeAll;
    function Get(const Index: Integer): PNode; inline;
  public
    function Add(const AText:String):PNode;
    function Count:Integer; inline;

    property Node[const Index:Integer]:PNode read Get; default;
  end;

  PNodes=^TNodes;

  TNode=record
  public
    Expanded : Boolean;
    Nodes : TNodes;
    Parent : PNodes;
    Text : String;

    function Add(const AText:String):PNode; inline;
    function Count:Integer; inline;
    function Level:Integer;
  end;

  TTreeExpanderRender=class(TExpanderRender)
  private
    Tree : TTreeColumn;
  public
    procedure Paint(var AData:TRenderData); override;
  end;

  TTreeColumn=class(TColumn)
  private
    FExpander : TTreeExpanderRender;
    Flat : Array of PNode;
    Valid : Boolean;

    procedure CreateFlat;
    procedure Invalidate;
    function LeftMargin(const ALevel:Integer):Single;
    function NodeOf(const ARow:Integer):PNode;

    function CanExpand(const Sender:TRender; const ARow:Integer):Boolean;
    function ToggleNode(const Sender:TRender; const ARow:Integer):Boolean;
    function GetExpanded(const Sender:TRender; const ARow:Integer):Boolean;
  protected
    function AutoWidth(const APainter:TPainter):Single; override;
  public
    Nodes : TNodes;

    Constructor Create(ACollection:TCollection); override;
    Destructor Destroy; override;
  end;

implementation
