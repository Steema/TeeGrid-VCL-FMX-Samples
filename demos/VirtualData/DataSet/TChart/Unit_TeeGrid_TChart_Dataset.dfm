object Form224: TForm224
  Left = 0
  Top = 0
  Caption = 'TeeGrid and TeeChart working together'
  ClientHeight = 659
  ClientWidth = 1123
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 593
    Top = 0
    Height = 659
    ExplicitLeft = 568
    ExplicitTop = 296
    ExplicitHeight = 100
  end
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 593
    Height = 659
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
    LeftWall.Color = clLightyellow
    Legend.Font.Name = 'Verdana'
    Legend.Shadow.Transparency = 0
    RightWall.Color = clLightyellow
    Title.Font.Name = 'Verdana'
    Title.Text.Strings = (
      'TChart')
    OnClickSeries = Chart1ClickSeries
    BottomAxis.Axis.Color = 4210752
    BottomAxis.Grid.Color = clDarkgray
    BottomAxis.LabelsFormat.Font.Name = 'Verdana'
    BottomAxis.TicksInner.Color = clDarkgray
    BottomAxis.Title.Font.Name = 'Verdana'
    DepthAxis.Axis.Color = 4210752
    DepthAxis.Grid.Color = clDarkgray
    DepthAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthAxis.TicksInner.Color = clDarkgray
    DepthAxis.Title.Font.Name = 'Verdana'
    DepthTopAxis.Axis.Color = 4210752
    DepthTopAxis.Grid.Color = clDarkgray
    DepthTopAxis.LabelsFormat.Font.Name = 'Verdana'
    DepthTopAxis.TicksInner.Color = clDarkgray
    DepthTopAxis.Title.Font.Name = 'Verdana'
    LeftAxis.Axis.Color = 4210752
    LeftAxis.Grid.Color = clDarkgray
    LeftAxis.LabelsFormat.Font.Name = 'Verdana'
    LeftAxis.TicksInner.Color = clDarkgray
    LeftAxis.Title.Font.Name = 'Verdana'
    RightAxis.Axis.Color = 4210752
    RightAxis.Grid.Color = clDarkgray
    RightAxis.LabelsFormat.Font.Name = 'Verdana'
    RightAxis.TicksInner.Color = clDarkgray
    RightAxis.Title.Font.Name = 'Verdana'
    TopAxis.Axis.Color = 4210752
    TopAxis.Grid.Color = clDarkgray
    TopAxis.LabelsFormat.Font.Name = 'Verdana'
    TopAxis.TicksInner.Color = clDarkgray
    TopAxis.Title.Font.Name = 'Verdana'
    View3D = False
    Align = alLeft
    TabOrder = 0
    ExplicitLeft = -3
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object Series1: TLineSeries
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {
        001900000000000000003071400000000000906A400000000000906A40000000
        0000405A400000000000E05F400000000000E05A400000000000206240000000
        0000806B400000000000F074400000000000D06B400000000000E87240000000
        00001078400000000000307B4000000000009877400000000000F07940000000
        0000087B400000000000407F400000000000E882400000000000407F40000000
        0000E07F4000000000006C81400000000000B880400000000000388340000000
        0000F881400000000000448140}
    end
  end
  object TeeGrid1: TTeeGrid
    Left = 596
    Top = 0
    Width = 527
    Height = 659
    Columns = <
      item
        Header.Text = 'Color'
        Link = 'Color'
      end
      item
        Header.Text = 'X'
        Link = 'X'
      end
      item
        Header.Text = 'Y'
        Link = 'Y'
      end
      item
        Header.Text = 'Label'
        Link = 'Label'
      end>
    DataSource = DataSource1
    Align = alClient
    UseDockManager = False
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    ExplicitLeft = 599
    _Headers = (
      1
      'TColumnHeaderBand'
      <
        item
        end>)
  end
  object DataSource1: TDataSource
    DataSet = ChartDataSet1
    Left = 264
    Top = 136
  end
  object ChartDataSet1: TChartDataSet
    Active = True
    Chart = Chart1
    Left = 152
    Top = 136
  end
end
