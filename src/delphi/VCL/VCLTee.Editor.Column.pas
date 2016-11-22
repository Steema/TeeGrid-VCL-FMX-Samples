{*********************************************}
{  TeeGrid Software Library                   }
{  VCL TColumn Editor                         }
{  Copyright (c) 2016 by Steema Software      }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Column;

interface

uses
  {Winapi.}Windows, {Winapi.}Messages, {System.}SysUtils, {System.}Classes, {Vcl.}Graphics,
  {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, VCLTee.Editor.Format.Text, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls, {Vcl.}ComCtrls,

  Tee.Grid.Columns,
  VCLTee.Editor.Coordinate, VCLTee.Editor.Margins, VCLTee.Editor.Render.Text;

type
  TColumnEditor = class(TForm)
    PageFormat: TPageControl;
    TabGeneral: TTabSheet;
    CBVisible: TCheckBox;
    CBExpanded: TCheckBox;
    CBReadOnly: TCheckBox;
    TabWidth: TTabSheet;
    TabData: TTabSheet;
    Label5: TLabel;
    MemoHeader: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    EFloatFormat: TEdit;
    EDateTimeFormat: TEdit;
    Label6: TLabel;
    EDateFormat: TEdit;
    Label7: TLabel;
    ETimeFormat: TEdit;
    TabFormat: TTabSheet;
    TabAlign: TTabSheet;
    TabMargins: TTabSheet;
    TabHeader: TTabSheet;
    Panel1: TPanel;
    CBParentFormat: TCheckBox;
    Panel2: TPanel;
    CBHeaderParent: TCheckBox;
    CBAutoAlign: TCheckBox;
    RGHorizAlign: TRadioGroup;
    RGVerticalAlign: TRadioGroup;
    PageHeader: TPageControl;
    TabHeaderFormat: TTabSheet;
    TabHeaderAlign: TTabSheet;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    procedure CBVisibleClick(Sender: TObject);
    procedure CBExpandedClick(Sender: TObject);
    procedure EFloatFormatChange(Sender: TObject);
    procedure EDateTimeFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CBReadOnlyClick(Sender: TObject);
    procedure PageFormatChange(Sender: TObject);
    procedure MemoHeaderChange(Sender: TObject);
    procedure EDateFormatChange(Sender: TObject);
    procedure ETimeFormatChange(Sender: TObject);
    procedure RGHorizAlignClick(Sender: TObject);
    procedure RGVerticalAlignClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CBAutoAlignClick(Sender: TObject);
    procedure CBParentFormatClick(Sender: TObject);
    procedure CBHeaderParentClick(Sender: TObject);
    procedure PageHeaderChange(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }

    Column : TColumn;

    IHeader : TTextRenderEditor;
    IFormat : TTextFormatEditor;
    IMargins : TMarginsEditor;
    IWidth : TCoordinateEditor;

    IChangingAlign : Boolean;

    FOnChangedHeader : TNotifyEvent;

    procedure RefreshColumnFormat(const AFormat: TDataFormat);
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const AColumn:TColumn):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const AColumn:TColumn):TColumnEditor; overload; static;

    procedure RefreshColumn(const AColumn:TColumn);

    property OnChangedHeader:TNotifyEvent read FOnChangedHeader
                                          write FOnChangedHeader;
  end;

implementation
