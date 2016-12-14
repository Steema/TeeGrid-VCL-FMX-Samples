unit FMXTee.Sheet.Editor.Font;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  Tee.Format, FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit, FMX.StdCtrls,
  FMX.Colors;

type
  TSheetFontEditor = class(TForm)
    CBFamily: TComboEdit;
    CBSize: TComboEdit;
    BColor: TComboColorBox;
    CBBack: TComboColorBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SBBold: TSpeedButton;
    SBItalic: TSpeedButton;
    SBUnderline: TSpeedButton;
    procedure CBSizeChangeTracking(Sender: TObject);
    procedure CBFamilyChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SBBoldClick(Sender: TObject);
    procedure SBItalicClick(Sender: TObject);
    procedure SBUnderlineClick(Sender: TObject);
    procedure BColorChange(Sender: TObject);
    procedure CBBackChange(Sender: TObject);
  private
    { Private declarations }

    IBack : TBrush;
    IFont : TFont;

    procedure ChangeStyle(const AButton:TSpeedButton; const AStyle:TFontStyle);
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AFont:TFont;
                          const ABack:TBrush):TSheetFontEditor; static;

    procedure Refresh(const AFont:TFont; const ABack:TBrush);
  end;

implementation

{$R *.fmx}

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}

  FMXTee.Editor.Painter.Stroke;

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

function InstalledFonts:TStringList;
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

{ TSheetFontEditor }

procedure TSheetFontEditor.BColorChange(Sender: TObject);
begin
  IFont.Color:=BColor.Color;
end;

procedure TSheetFontEditor.CBBackChange(Sender: TObject);
begin
  IBack.Color:=CBBack.Color;
  IBack.Show;
end;

procedure TSheetFontEditor.CBFamilyChange(Sender: TObject);
var tmp : String;
begin
  tmp:=Trim(CBFamily.Text);

  if tmp<>'' then
     IFont.Name:=tmp;
end;

procedure TSheetFontEditor.CBSizeChangeTracking(Sender: TObject);
var tmp : String;
    tmpValue : Single;
begin
  tmp:=Trim(CBSize.Text);

  if tmp<>'' then
     if TryStrToFloat(tmp,tmpValue) then
        IFont.Size:=tmpValue;
end;

class function TSheetFontEditor.Embedd(const AOwner: TComponent;
                                       const AParent: TControl;
                                       const AFont:TFont;
                                       const ABack:TBrush): TSheetFontEditor;
begin
  result:=TSheetFontEditor.Create(AOwner);

  result.Refresh(AFont,ABack);
  TStrokeEditor.AddForm(result,AParent);
end;

function SizeAsString(const ASize:Single):String;
begin
  result:=FormatFloat('0.##',ASize);
end;

procedure TSheetFontEditor.Refresh(const AFont: TFont; const ABack:TBrush);
var tmp : String;
    tmpFonts : TStringList;
begin
  IFont:=AFont;
  IBack:=ABack;

  if CBFamily.Count=0 then
  begin
    tmpFonts:=InstalledFonts;
    try
      CBFamily.Items:=tmpFonts;
    finally
      tmpFonts.Free;
    end;
  end;

  CBFamily.ItemIndex:=CBFamily.Items.IndexOf(IFont.Name);

  tmp:=SizeAsString(IFont.Size);

  CBSize.ItemIndex:=CBSize.Items.IndexOf(tmp);

  if CBSize.ItemIndex=-1 then
     CBSize.Text:=tmp;

  SBBold.IsPressed:=TFontStyle.fsBold in IFont.Style;
  SBItalic.IsPressed:=TFontStyle.fsItalic in IFont.Style;
  SBUnderline.IsPressed:=TFontStyle.fsUnderline in IFont.Style;

  CBBack.Color:=TColorHelper.SwapCheck(IBack.Color);
  BColor.Color:=TColorHelper.SwapCheck(IFont.Color);
end;

procedure TSheetFontEditor.ChangeStyle(const AButton:TSpeedButton; const AStyle:TFontStyle);
begin
//  AButton.IsPressed:=not AButton.IsPressed;

  if AButton.IsPressed then
     IFont.Style:=IFont.Style+[AStyle]
  else
     IFont.Style:=IFont.Style-[AStyle];
end;

procedure TSheetFontEditor.SBBoldClick(Sender: TObject);
begin
  ChangeStyle(SBBold,TFontStyle.fsBold);
end;

procedure TSheetFontEditor.SBItalicClick(Sender: TObject);
begin
  ChangeStyle(SBItalic,TFontStyle.fsItalic);
end;

procedure TSheetFontEditor.SBUnderlineClick(Sender: TObject);
begin
  ChangeStyle(SBUnderline,TFontStyle.fsUnderline);
end;

procedure TSheetFontEditor.SpeedButton1Click(Sender: TObject);
begin
  CBSize.Text:=SizeAsString(IFont.Size+1);
end;

procedure TSheetFontEditor.SpeedButton2Click(Sender: TObject);
begin
  if IFont.Size>6 then
     CBSize.Text:=SizeAsString(IFont.Size-1);
end;

end.
