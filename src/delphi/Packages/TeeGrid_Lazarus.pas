unit TeeGrid_Lazarus;
interface
procedure Register;
implementation
uses
  Tee.Control,
  Tee.Format,
  Tee.Grid.Bands,
  Tee.Grid.Columns,
  Tee.Grid.CSV,
  Tee.Grid.Data.DB,
  Tee.Grid.Data,
  Tee.Grid.Data.Strings,
  Tee.Grid.Data.Tree,
  Tee.Grid.Header,
  Tee.Grid,
  Tee.Grid.RowGroup,
  Tee.Grid.Rows,
  Tee.Grid.Selection,
  Tee.Grid.Themes,
  Tee.Grid.Ticker,
  Tee.Grid.Totals,
  Tee.Painter,
  Tee.Renders,
  VCLTee.Control,
  VCLTee.Editor.Borders,
  VCLTee.Editor.Brush,
  VCLTee.Editor.Column,
  VCLTee.Editor.ColumnBand,
  VCLTee.Editor.Coordinate,
  VCLTee.Editor.Font,
  VCLTee.Editor.Format,
  VCLTee.Editor.Format.Text,
  VCLTee.Editor.Gradient,
  VCLTee.Editor.Grid.Band.Text,
  VCLTee.Editor.Grid.Band.Totals,
  VCLTee.Editor.Grid.Bands,
  VCLTee.Editor.Grid,
  VCLTee.Editor.Grid.Ticker,
  VCLTee.Editor.Header,
  VCLTee.Editor.Margins,
  VCLTee.Editor.Render.Text,
  VCLTee.Editor.Stroke,
  VCLTee.Editor.Text.Align,
  VCLTee.Grid,
  VCLTee.Painter,
  VCLTee.Picture,
  VCLTee.Grid.About,
  VCLTeeGridRegister;

procedure Register;
begin
  VCLTeeGridRegister.Register;
end;

end.
