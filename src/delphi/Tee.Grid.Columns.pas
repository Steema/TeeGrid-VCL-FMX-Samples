{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Columns class                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Columns;

interface

uses
  System.Classes,
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  System.Types, System.UITypes,
  {$ENDIF}
  Tee.Painter, Tee.Format, Tee.Renders;

type
  TColumn=class;

  TColumnHeader=class(TTextRender)
  private
    FParentFormat: Boolean;

    procedure SetParentFormat(const Value: Boolean);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
  published
    property ParentFormat:Boolean read FParentFormat write SetParentFormat default True;
  end;

  TColumnWidth=class(TCoordinate);

  TColumnFormat=class(TPersistentChange)
  private
    FDate,
    FDateTime,
    FFloat,
    FTime : String;

    function IsFloatStored: Boolean;

    procedure SetDate(const Value: String);
    procedure SetDateTime(const Value: String);
    procedure SetFloat(const Value: String);
    procedure SetTime(const Value: String);
  protected
    const
      DefaultFloat='0.###';
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    procedure Assign(Source:TPersistent); override;
  published
    property Date:String read FDate write SetDate;
    property DateTime:String read FDateTime write SetDateTime;
    property Float:String read FFloat write SetFloat stored IsFloatStored;
    property Time:String read FTime write SetTime;
  end;

  TRenderClass=class of TTextRender;

  TColumns=class;

  TColumn=class(TCollectionItem)
  private
    FColumnFormat: TColumnFormat;
    FExpanded: Boolean;
    FHeader: TColumnHeader;
    FItems : TColumns;
    FTagObject : TObject;
    FReadOnly: Boolean;
    FRender: TTextRender;
    FVisible: Boolean;
    FWidth: TColumnWidth;

    ITopParent : TColumns;

    function GetFormat: TTextFormat;
    function GetItems: TColumns;
    function GetParent: TColumn;
    procedure IChanged(Sender:TObject);
    function MaxLevel:Integer;
    procedure SetColumnFormat(const Value: TColumnFormat);
    procedure SetExpanded(const Value: Boolean);
    procedure SetFormat(const Value: TTextFormat);
    procedure SetHeader(const Value: TColumnHeader);
    procedure SetItems(const Value: TColumns);
    procedure SetRender(const Value: TTextRender);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: TColumnWidth);
  protected
    procedure DoChanged;
  public
    EditorClass : TClass;

    Constructor Create(ACollection:TCollection); override;
    Destructor Destroy; override;

    function CanDisplay:Boolean;
    procedure ChangeRender(const ARenderClass:TRenderClass);
    function ParentColumns:TColumns;

    function HasFormat:Boolean; inline;
    function HasItems:Boolean; inline;

    procedure Paint(const APainter:TPainter; const AText:String; const ARect:TRectF);

    property Parent:TColumn read GetParent;
    property Render:TTextRender read FRender write SetRender;
    property TagObject:TObject read FTagObject write FTagObject;
    property TopParent:TColumns read ITopParent;
  published
    property ColumnFormat:TColumnFormat read FColumnFormat write SetColumnFormat;
    property Expanded:Boolean read FExpanded write SetExpanded default True;
    property Format:TTextFormat read GetFormat write SetFormat;
    property Header:TColumnHeader read FHeader write SetHeader;
    property Items:TColumns read GetItems write SetItems;
    property ReadOnly:Boolean read FReadOnly write FReadOnly default False;
    property Visible:Boolean read FVisible write SetVisible default True;
    property Width:TColumnWidth read FWidth write SetWidth;
  end;

  TColumnEvent=procedure(Sender:TObject; const AColumn:TColumn) of object;

  TColumns=class(TOwnedCollection)
  private
    FOnChanged : TNotifyEvent;

    function Get(const Index: Integer): TColumn;
    function GetParent: TColumn;
    function MaxLevel:Integer;
    procedure Put(const Index: Integer; const Value: TColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add:TColumn; overload; {$IFNDEF FPC}inline;{$ENDIF} // not yet supported in FPC
    function Add(const AText:String):TColumn; overload;

    function LevelCount:Integer;

    property Items[const Index:Integer]:TColumn read Get write Put; default;
    property Parent:TColumn read GetParent;
  published
    property OnChanged:TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TColumnUtils=record
  public
    class procedure ChangeHeaderFont(const AColumns:TColumns;
                                     const ASelected:TColumn;
                                     const AStyle:TFontStyles); static;
  end;

implementation
