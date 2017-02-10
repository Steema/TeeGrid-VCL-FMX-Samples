unit VCLTee.Picture;
{$I Tee.inc}

interface

uses
  { Vcl.}Graphics, { Vcl.}ExtCtrls, Tee.Format;

type
  TVCLPicture=record
  private
    type
      TVCLGraphic=Graphics.TGraphic;
  public
    class function From(const AGraphic:TVCLGraphic):TPicture; overload; static;
    class function From(const APicture:Graphics.TPicture):TPicture; overload; static;
    class function From(const AImage:TImage):TPicture; overload; static;
    class function From(const AFileName:String):TPicture; overload; static;

    class function Load(const AFileName:String):Graphics.TPicture; overload; static;
    class procedure Load(const APicture:TPicture; const AFileName:String); overload; static;

    class function TypeOfGraphic(const AGraphic:TGraphic):String; static;
  end;

implementation
