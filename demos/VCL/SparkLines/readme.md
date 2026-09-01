## Spark lines

Add new columns to draw "spark lines" (mini charts) using data from other columns.
You can add as many as you wish.

<img width="823" height="844" alt="image" src="https://github.com/user-attachments/assets/7ba04e80-5123-4e0a-8371-0ef447c9f2ef" />

```delphi
uses Tee.Renders.SparkLines, UITypes;

var Sparks : TSparkLines;

// 1 and 10 are the start and finish columns you want to plot
Sparks := TSparkLines.AddTo(TeeGrid1.Grid, 1,10, 'Sparks');

Sarks.Stroke.Size:=2;
Sparks.Stroke.Color:=TColors.Blue;

```
