unit FMXTee.Sheet.Editor.Font;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit, FMX.StdCtrls, FMX.Colors,

  Tee.Format, Tee.Renders, Tee.Sheet;

type
  TRenderChange=procedure(const Sender:TRender) of object;
  TRenderChangeSingle=procedure(const Sender:TRender; const Value:Single) of object;

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

    IChanging : Boolean;

    IFormat: TTextFormat;
    ISheet : TSheet;

    procedure ChangeStyle(const AButton:TSpeedButton; const AStyle:TFontStyle);
    procedure ForceFormat;
  public
    { Public declarations }

    class function Embedd(const AOwner:TComponent; const AParent:TControl;
                          const AFormat:TTextFormat;
                          const ASheet:TSheet):TSheetFontEditor; static;

    procedure Refresh(const AFormat:TTextFormat; const ASheet:TSheet);
  end;

implementation

{$R *.fmx}

uses
  FMXTee.Font.Family,
  FMXTee.Editor.Painter.Stroke;

{ TSheetFontEditor }

procedure TSheetFontEditor.BColorChange(Sender: TObject);
begin
  if not IChanging then
  begin
    ForceFormat;
    IFormat.Font.Color:=TColorHelper.SwapCheck(BColor.Color);
  end;
end;

procedure TSheetFontEditor.CBBackChange(Sender: TObject);
begin
  if not IChanging then
  begin
    ForceFormat;

    IFormat.Brush.Color:=TColorHelper.SwapCheck(CBBack.Color);
    IFormat.Brush.Show;
  end;
end;

procedure TSheetFontEditor.CBFamilyChange(Sender: TObject);
var tmp : String;
begin
  if not IChanging then
  begin
    tmp:=Trim(CBFamily.Text);

    if tmp<>'' then
    begin
      ForceFormat;
      IFormat.Font.Name:=tmp;
    end;
  end;
end;

procedure TSheetFontEditor.CBSizeChangeTracking(Sender: TObject);
var tmp : String;
    tmpValue : Single;
begin
  if not IChanging then
  begin
    tmp:=Trim(CBSize.Text);

    if tmp<>'' then
       if TryStrToFloat(tmp,tmpValue) then
       begin
         ForceFormat;
         IFormat.Font.Size:=tmpValue;
       end;
  end;
end;

class function TSheetFontEditor.Embedd(const AOwner: TComponent;
                                       const AParent: TControl;
                                       const AFormat:TTextFormat;
                                       const ASheet:TSheet): TSheetFontEditor;
begin
  result:=TSheetFontEditor.Create(AOwner);

  result.Refresh(AFormat,ASheet);
  TStrokeEditor.AddForm(result,AParent);
end;

procedure TSheetFontEditor.ForceFormat;
begin
  IFormat:=ISheet.CurrentRender.Format;
  ISheet.Grid.Rows.Height.Automatic:=True;
end;

function SizeAsString(const ASize:Single):String;
begin
  result:=FormatFloat('0.##',ASize);
end;

procedure TSheetFontEditor.Refresh(const AFormat:TTextFormat; const ASheet:TSheet);

  procedure RefreshFont(const AFont:TFont);
  var tmp : String;
  begin
    CBFamily.ItemIndex:=CBFamily.Items.IndexOf(AFont.Name);

    tmp:=SizeAsString(AFont.Size);

    CBSize.ItemIndex:=CBSize.Items.IndexOf(tmp);

    if CBSize.ItemIndex=-1 then
       CBSize.Text:=tmp;

    SBBold.IsPressed:=TFontStyle.fsBold in AFont.Style;
    SBItalic.IsPressed:=TFontStyle.fsItalic in AFont.Style;
    SBUnderline.IsPressed:=TFontStyle.fsUnderline in AFont.Style;

    BColor.Color:=TColorHelper.SwapCheck(AFont.Color);
  end;

  procedure TryFillFonts;
  var tmpFonts : TStringList;
  begin
    if CBFamily.Count=0 then
    begin
      tmpFonts:=TFMXFonts.Installed;
      try
        CBFamily.Items:=tmpFonts;
      finally
        tmpFonts.Free;
      end;
    end;
  end;

begin
  ISheet:=ASheet;
  IFormat:=AFormat;

  IChanging:=True;
  try
    TryFillFonts;
    RefreshFont(IFormat.Font);
    CBBack.Color:=TColorHelper.SwapCheck(IFormat.Brush.Color);
  finally
    IChanging:=False;
  end;
end;

procedure TSheetFontEditor.ChangeStyle(const AButton:TSpeedButton; const AStyle:TFontStyle);

  procedure DoChange(const AFont:TFont);
  begin
    if AButton.IsPressed then
       AFont.Style:=AFont.Style+[AStyle]
    else
       AFont.Style:=AFont.Style-[AStyle];
  end;

begin
  if not IChanging then
  begin
    ForceFormat;
    DoChange(IFormat.Font);
  end;
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
  ForceFormat;
  CBSize.Text:=SizeAsString(IFormat.Font.Size+1);
end;

procedure TSheetFontEditor.SpeedButton2Click(Sender: TObject);
begin
  if IFormat.Font.Size>6 then
  begin
    ForceFormat;
    CBSize.Text:=SizeAsString(IFormat.Font.Size-1);
  end;
end;

end.
