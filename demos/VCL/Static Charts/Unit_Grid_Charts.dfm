object FormGridCharts: TFormGridCharts
  Left = 0
  Top = 0
  Caption = 'TeeGrid - Displaying static TeeChart in grid cells'
  ClientHeight = 647
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TeeGrid1: TTeeGrid
    Left = 0
    Top = 41
    Width = 443
    Height = 606
    Columns = <>
    ReadOnly = False
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    ExplicitLeft = 16
    ExplicitTop = 72
    ExplicitWidth = 329
    ExplicitHeight = 537
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
  object Chart1: TChart
    Left = 160
    Top = 304
    Width = 201
    Height = 201
    BackWall.Brush.Gradient.Direction = gdBottomTop
    BackWall.Brush.Gradient.EndColor = clWhite
    BackWall.Brush.Gradient.StartColor = 15395562
    BackWall.Brush.Gradient.Visible = True
    BackWall.Transparent = False
    Foot.Font.Color = clBlue
    Foot.Font.Name = 'Verdana'
    Gradient.Direction = gdBottomTop
    Gradient.EndColor = clWhite
    Gradient.MidColor = 15395562
    Gradient.StartColor = 15395562
    Gradient.Visible = True
    LeftWall.Color = 14745599
    Legend.Font.Name = 'Verdana'
    Legend.Shadow.Transparency = 0
    Legend.Visible = False
    RightWall.Color = 14745599
    Title.Font.Name = 'Verdana'
    Title.Text.Strings = (
      'Template')
    BottomAxis.Axis.Color = 4210752
    BottomAxis.Grid.Color = 11119017
    BottomAxis.LabelsFormat.Font.Name = 'Verdana'
    BottomAxis.TicksInner.Color = 11119017
    BottomAxis.Title.Font.Name = 'Verdana'
    DepthAxis.Axis.Color = 4210752
    DepthAxis.Grid.Color = 11119017
    DepthAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthAxis.TicksInner.Color = 11119017
    DepthAxis.Title.Font.Name = 'Verdana'
    DepthTopAxis.Axis.Color = 4210752
    DepthTopAxis.Grid.Color = 11119017
    DepthTopAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthTopAxis.TicksInner.Color = 11119017
    DepthTopAxis.Title.Font.Name = 'Verdana'
    LeftAxis.Axis.Color = 4210752
    LeftAxis.Grid.Color = 11119017
    LeftAxis.LabelsFormat.Font.Name = 'Verdana'
    LeftAxis.TicksInner.Color = 11119017
    LeftAxis.Title.Font.Name = 'Verdana'
    RightAxis.Axis.Color = 4210752
    RightAxis.Grid.Color = 11119017
    RightAxis.LabelsFormat.Font.Name = 'Verdana'
    RightAxis.TicksInner.Color = 11119017
    RightAxis.Title.Font.Name = 'Verdana'
    TopAxis.Axis.Color = 4210752
    TopAxis.Grid.Color = 11119017
    TopAxis.LabelsFormat.Font.Name = 'Verdana'
    TopAxis.TicksInner.Color = 11119017
    TopAxis.Title.Font.Name = 'Verdana'
    View3D = False
    TabOrder = 0
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TBarSeries
      ColorEachPoint = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 104
    ExplicitTop = 320
    ExplicitWidth = 185
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 121
      Height = 25
      Caption = '&Chart Template...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 184
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Grid...'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
