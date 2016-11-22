{*********************************************}
{  TeeGrid Software Library                   }
{  Abstract Columns class                     }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit Tee.Grid.Columns;
{$I Tee.inc}

interface

{
   TColumn and TColumns classes, define a "column" in a TeeGrid.

   Columns have an "Items" property to (optionally) include sub-columns.

   Other TColumn properties:

     Expanded (default true, show column sub-columns, if they exist)

     Format (custom brush, stroke, font for a column)

     Header (text and formatting to display column names at TeeGrid headers.

     DataFormat (formatting strings for numbers and date-time data types)

     Margins (edge spacing inside grid cells)

     ReadOnly (enable or disable editing a grid cell content using keyboard)

     TextAlignment (automatic depending on data type, or custom)

     Visible (default True)

     Width (automatic or custom)


   Needs: Tee.Painter, Tee.Format and Tee.Renders
}

uses
  {System.}Classes,
  {$IFDEF FPC}
  Graphics,
  {$ELSE}
  {System.}Types,
  {$IFNDEF NOUITYPES}
  System.UITypes,
  {$ENDIF}
  {$ENDIF}

  Tee.Painter, Tee.Format, Tee.Renders;

type
  // String formatting properties for Date-Time and Float numbers.
  // TColumn includes this property to configure grid cell display
  TDataFormat=class(TPersistentChange)
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

  // Just an alias
  TCellRender=class(TVisibleTextRender);

  // Just an alias
  TColumnWidth=class(TCoordinate);

  // Cell text alignment. Automatic means to right-align numbers etc.
  TColumnTextAlign=(Automatic,Custom);

  // Properties to paint a column name at a TeeGrid header
  TColumnHeader=class(TCellRender)
  private
    FParentFormat: Boolean;
    FText: String;
    FTextAlignment: TColumnTextAlign;

    procedure SetParentFormat(const Value: Boolean);
    procedure SetText(const Value: String);
    procedure SetTextAlignment(const Value: TColumnTextAlign);
  public
    Constructor Create(const AChanged:TNotifyEvent); override;
    procedure Assign(Source:TPersistent); override;

    property ParentFormat:Boolean read FParentFormat write SetParentFormat default True;
  published
    property Text:String read FText write SetText;
    property TextAlignment:TColumnTextAlign read FTextAlignment
                     write SetTextAlignment default TColumnTextAlign.Automatic;
  end;

  TRenderClass=class of TRender;

  TColumns=class;

  // TColumn class
  // Defines the properties used to paint rows of a TeeGrid column,
  // and an optional "Items" property to contain sub-columns
  TColumn=class(TCollectionItem)
  private
    FDataFormat: TDataFormat;
    FExpanded: Boolean;
    FHeader: TColumnHeader;
    FItems : TColumns;
    FTagObject : TObject;
    FReadOnly: Boolean;
    FRender: TRender;
    FTextAlignment: TColumnTextAlign;
    FVisible: Boolean;
    FWidth: TColumnWidth;

    IHorizontal : THorizontalAlign;
    ITopParent : TColumns;

    function GetAlign: TTextAlign;
    function GetFormat: TTextFormat;
    function GetItems: TColumns;
    function GetMargins: TMargins;
    function GetParent: TColumn;
    function GetRender: TRender;
    function MaxLevel:Integer;
    procedure SetAlign(const Value: TTextAlign);
    procedure SetDataFormat(const Value: TDataFormat);
    procedure SetExpanded(const Value: Boolean);
    procedure SetFormat(const Value: TTextFormat);
    procedure SetHeader(const Value: TColumnHeader);
    procedure SetItems(const Value: TColumns);
    procedure SetMargins(const Value: TMargins);
    procedure SetRender(const Value: TRender);
    procedure SetTextAlignment(const Value: TColumnTextAlign);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: TColumnWidth);
  protected
    procedure DoChanged;

    property DefaultHorizAlign:THorizontalAlign read IHorizontal;
  public
    EditorClass : TClass;

    Left : Single;
    ValidWidth : Boolean;

    Constructor Create(ACollection:TCollection); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    function CanDisplay:Boolean;
    procedure Changed(Sender:TObject);
    procedure ChangeRender(const ARenderClass:TRenderClass);
    function ParentColumns:TColumns; inline;

    function HasFormat:Boolean; inline;
    function HasItems:Boolean; inline;
    function HasRender:Boolean; inline;

    function HorizAlign:THorizontalAlign; inline;

    procedure InitAlign(const AHorizontal:THorizontalAlign);
    function Level:Integer;
    function Right:Single; inline;

    procedure Paint(var AData:TRenderData; const ARender:TRender);

    property Parent:TColumn read GetParent;
    property Render:TRender read GetRender write SetRender;
    property TagObject:TObject read FTagObject write FTagObject;
    property TopParent:TColumns read ITopParent;
  published
    property DataFormat:TDataFormat read FDataFormat write SetDataFormat;
    property Expanded:Boolean read FExpanded write SetExpanded default True;
    property Format:TTextFormat read GetFormat write SetFormat;
    property Header:TColumnHeader read FHeader write SetHeader;
    property Items:TColumns read GetItems write SetItems;
    property Margins:TMargins read GetMargins write SetMargins;
    property ReadOnly:Boolean read FReadOnly write FReadOnly default False;
    property TextAlign:TTextAlign read GetAlign write SetAlign;
    property TextAlignment:TColumnTextAlign read FTextAlignment
                     write SetTextAlignment default TColumnTextAlign.Automatic;
    property Visible:Boolean read FVisible write SetVisible default True;
    property Width:TColumnWidth read FWidth write SetWidth;
  end;

  TColumnEvent=procedure(Sender:TObject; const AColumn:TColumn) of object;

  // Collection of TColumn,
  // with a "Spacing" property that determines the horizontal separation
  // between columns.
  TColumns=class(TCollectionChange)
  private
    FOnRemoved : TColumnEvent;
    FSpacing : TCoordinate;

    function Get(const Index: Integer): TColumn; inline;
    function GetByName(const AName:String): TColumn;
    function GetParent: TColumn;
    function GetSpacing:TCoordinate;
    function MaxLevel:Integer;
    procedure Put(const Index: Integer; const Value: TColumn);
    procedure SetSpacing(const Value: TCoordinate);
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    ValidWidth : Boolean;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    procedure Assign(Source: TPersistent); override;

    function Add:TColumn; overload; {$IFNDEF FPC}inline;{$ENDIF} // not yet supported in FPC
    function Add(const AText:String):TColumn; overload;

    function FindAt(const X,MaxRight:Single):TColumn;
    function FindFirst(const AName:String):TColumn;
    function FirstVisible:TColumn;
    function HasSpacing:Boolean; inline;
    function LevelCount:Integer;

    property Items[const Index:Integer]:TColumn read Get write Put; default;

    {$IFNDEF FPC}
    property Items[const AName:String]:TColumn read GetByName; default;
    {$ENDIF}

    property Parent:TColumn read GetParent;
  published
    property Spacing: TCoordinate read GetSpacing write SetSpacing;

    property OnRemoved:TColumnEvent read FOnRemoved write FOnRemoved;
  end;

  // Internal use.
  // Contains an array of TColumn with all "Visible" columns of a TeeGrid.
  TVisibleColumns=record
  private
    FVisible : Array of TColumn;

    function Get(const Index: Integer): TColumn; inline;
  public
    procedure Add(const AColumn:TColumn); overload;
    procedure Add(const AColumns:TColumns); overload;
    function AllSameFormat:Boolean;
    procedure Clear; inline;
    function Count:Integer; inline;
    function First:TColumn;
    function IndexOf(const AColumn: TColumn): Integer;
    function Last:TColumn;
    function MaxRight:Single;
    function Next(const AColumn:TColumn):TColumn;
    function Previous(const AColumn:TColumn):TColumn;

    property Items[const Index:Integer]:TColumn read Get; default;
  end;

  // Special column class.
  // Rectangle shape with symbols, to optionally paint as left-most first column
  TIndicator=class(TVisibleTextRender)
  private
    FWidth: TColumnWidth;

    procedure SetWidth(const Value: TColumnWidth);
    function Triangle(const R:TRectF):TPointsF;
  public
    const
      DefaultWidth=10;

    Constructor Create(const AChanged:TNotifyEvent); override;
    Destructor Destroy; override;

    procedure Paint(var AData:TRenderData); override;
  published
    property Width:TColumnWidth read FWidth write SetWidth;
  end;

  // Experimental, internal
  TColumnUtils=record
  public
    class procedure ChangeHeaderFont(const AColumns:TColumns;
                                     const ASelected:TColumn;
                                     const AStyle:TFontStyles); static;
  end;

implementation
