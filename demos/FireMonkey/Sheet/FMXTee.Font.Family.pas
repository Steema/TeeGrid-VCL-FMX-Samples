unit FMXTee.Font.Family;

interface

uses
  {System.}Classes;

type
  TFMXFonts=record
  public
    class function Installed:TStringList; static;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  {Winapi.}Windows;
{$ENDIF}

{$IFDEF MACOS}
{$IFNDEF IOS}
uses
  Macapi.AppKit, Macapi.Foundation;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  Temp : String;
  CBFamily : TStringList;
begin
  Temp := LogFont.lfFaceName;

  CBFamily:=TStringList(Data);
  if CBFamily.IndexOf(Temp)=-1 then
     CBFamily.Add(Temp);

  Result := 1;
end;
{$ENDIF}

class function TFMXFonts.Installed:TStringList;
{$IFDEF MSWINDOWS}
var DC : HDC;
    LFont : TLogFont;
{$ELSE}
{$IFDEF MACOS}
{$IFNDEF IOS}
// http://delphiscience.wordpress.com/2012/11/20/getting-system-fonts-list-in-firemonkey-the-new-tplatformextensions-class/
var
  fManager: NsFontManager;
  list:NSArray;
  lItem:NSString;
  t: Integer;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
  result:=TStringList.Create;

  {$IFDEF MSWINDOWS}
  DC := GetDC(0);
  try
    FillChar(LFont, sizeof(LFont), 0);
    LFont.lfCharset := DEFAULT_CHARSET;

    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, NativeInt(result), 0);
  finally
    ReleaseDC(0, DC);
  end;
  {$ELSE}

  {$IFDEF MACOS}

  {$IFDEF IOS}
  // ??
  {$ELSE}

  fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
  List := fManager.availableFontFamilies;

  if List <> nil then
     for t := 0 to List.Count-1 do
     begin
       lItem := TNSString.Wrap(List.objectAtIndex(t));
       result.Add(String(lItem.UTF8String));
     end;

  {$ENDIF}

  {$ELSE}

  {$IFDEF ANDROID}
  result.Add('Droid Sans');
  result.Add('Droid Serif');
  result.Add('Droid Sans Mono');
  result.Add('Roboto');
  {$ELSE}

  {$ENDIF}

  {$ENDIF}
  {$ENDIF}

  result.Sorted:=True;
end;

end.
