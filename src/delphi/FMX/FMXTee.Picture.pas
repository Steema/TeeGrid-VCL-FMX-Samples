unit FMXTee.Picture;
{$I Tee.inc}

interface

uses
  FMX.Types,

  {$IF CompilerVersion<=25}
  {$DEFINE HASFMX20}
  {$IFEND}

  {$IFNDEF HASFMX20}
  FMX.Graphics,
  {$ENDIF}

  {$IF CompilerVersion>24}
  FMX.StdCtrls,
  {$IFEND}

  FMX.Objects,

  Tee.Format;

type
  TFMXPicture=record
  private
  public
    class function From(const ABitmap:TBitmap):TPicture; overload; static;
    class function From(const AImage:TImage):TPicture; overload; static;
    class function From(const AImage:TImageControl):TPicture; overload; static;
    class function From(const AFileName:String):TPicture; overload; static;

    class function Load(const AFileName:String):TBitmap; overload; static;
    class procedure Load(const APicture:TPicture; const AFileName:String); overload; static;

    class function TypeOfGraphic(const ABitmap: TBitmap): String; static;
  end;

implementation
