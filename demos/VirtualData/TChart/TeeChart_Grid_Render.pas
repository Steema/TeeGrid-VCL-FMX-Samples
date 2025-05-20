unit TeeChart_Grid_Render;

interface

uses
  Tee.Renders, TeeProcs;

implementation

uses
  Graphics, Types, {VCLTee.}Picture, Tee.Format;

type
  TTeePanelRender=class(TRender)
  public
    procedure Paint(var AData:TRenderData); override;
  end;

{ TTeePanelRender }

procedure TTeePanelRender.Paint(var AData: TRenderData);
var Picture : Tee.Format.TPicture;
    Bitmap : TBitmap;
    Panel : TCustomTeePanel;
begin
  if AData.Instance is TCustomTeePanel then
  begin
    Panel:=TCustomTeePanel(AData.Instance);

    Bitmap:=Panel.TeeCreateBitmap(Panel.Color,TRect.Create(0,0,Panel.Width,Panel.Height));
    Picture:=TVCLPicture.From(Bitmap);

//  Picture.Transparent := True;   //example transparency
//  Picture.TransparentColor := clGray;
//  Picture.TransparentMode := TTeeTransparentMode.ttmFixed;

    try
      AData.Painter.Draw(Picture,AData.Bounds);
    finally
      Bitmap.Free;
      Picture.Free;
    end;
  end;
end;

initialization
  TRenderClasses.Register(TCustomTeePanel,TTeePanelRender);
finalization
  TRenderClasses.UnRegister(TCustomTeePanel);
end.
