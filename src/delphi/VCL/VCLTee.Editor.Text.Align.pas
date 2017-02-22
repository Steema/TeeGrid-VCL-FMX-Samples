{*********************************************}
{  TeeGrid Software Library                   }
{  VCL Text Alignment Editor                  }
{  Copyright (c) 2016-2017 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit VCLTee.Editor.Text.Align;

interface

uses
  {$IFDEF MSWINDOWS}
  {Winapi.}Windows, {Winapi.}Messages,
  {$ENDIF}
  {System.}SysUtils, {System.}Classes,
  {Vcl.}Graphics, {Vcl.}Controls, {Vcl.}Forms, {Vcl.}Dialogs, {Vcl.}StdCtrls,
  {Vcl.}ExtCtrls,

  Tee.Renders;

type
  TTextAlignEditor = class(TForm)
    RGHorizontal: TRadioGroup;
    RGVertical: TRadioGroup;
    procedure RGHorizontalClick(Sender: TObject);
    procedure RGVerticalClick(Sender: TObject);
  private
    { Private declarations }

    IAlign : TTextAlign;
    FOnChange : TNotifyEvent;

    procedure Dochanged;
  public
    { Public declarations }

    class function Edit(const AOwner:TComponent; const ATextAlign:TTextAlign):Boolean; static;

    class function Embedd(const AOwner:TComponent; const AParent:TWinControl;
                          const ATextAlign:TTextAlign):TTextAlignEditor; overload; static;

    procedure RefreshAlign(const ATextAlign:TTextAlign);

    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
