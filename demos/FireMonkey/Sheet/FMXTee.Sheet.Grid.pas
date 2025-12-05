{*********************************************}
{  TeeGrid Software Library                   }
{  TGridSheet, a specialized TeeGrid          }
{  Copyright (c) 2015-2025 by Steema Software }
{  All Rights Reserved                        }
{*********************************************}
unit FMXTee.Sheet.Grid;

interface

{
   TSheets, a collection of TGridSheet controls.
}

uses
  System.Classes,

  FMXTee.Grid, Tee.Sheet, Tee.Grid.Columns;

type
  TGridSheet=class(TTeeGrid)
  private
    FSheet : TSheet;

    procedure ColumnResized(Sender:TObject; const AColumn:TColumn);
  public
    Constructor Create(AOwner:TComponent); override;

    {$IFNDEF AUTOREFCOUNT}
    Destructor Destroy; override;
    {$ENDIF}

    property Sheet:TSheet read FSheet;
  end;

  TSheetItem=class(TCollectionItem)
  public
    Sheet : TGridSheet;
  end;

  TSheets=class(TOwnedCollection)
  private
    function NewName:String;
    function Get(const Index: Integer): TSheetItem;
    procedure Put(const Index: Integer; const Value: TSheetItem);
  public
    function Add: TSheetItem;

    property Items[const Index:Integer]:TSheetItem read Get write Put; default;
  end;

implementation

uses
  {System.}SysUtils;

{ TGridSheet }

Constructor TGridSheet.Create(AOwner: TComponent);
begin
  inherited;

  FSheet:=TSheet.Create(Grid);

  Selected.Range.Enabled:=True;

  // OnColumnResized is no longer necessary, there is a property to do this:
  // Header.ResizeRepaint:=True;

  OnColumnResized:=ColumnResized;
end;

{$IFNDEF AUTOREFCOUNT}
Destructor TGridSheet.Destroy;
begin
  FSheet.Free;
  inherited;
end;
{$ENDIF}

procedure TGridSheet.ColumnResized(Sender:TObject; const AColumn:TColumn);
begin
  Repaint;
end;

{ TSheets }

function TSheets.Add: TSheetItem;
begin
  result:=inherited Add as TSheetItem;

  result.Sheet:=TGridSheet.Create(Owner as TComponent);
  result.Sheet.Name:=NewName;
  result.Sheet.Sheet.Name:=result.Sheet.Name;
end;

function TSheets.Get(const Index: Integer): TSheetItem;
begin
  result:=(inherited Items[Index] as TSheetItem);
end;

function TSheets.NewName:String;

  function Find(const AName:String):TSheetItem;
  var t : Integer;
  begin
    for t:=0 to Count-1 do
        if SameText(Items[t].Sheet.Name,AName) then
           Exit(Items[t]);

    result:=nil;
  end;

var tmp : Integer;
begin
  tmp:=1;

  repeat
    result:='Sheet'+tmp.ToString;

    if Find(result)=nil then
       break
    else
       Inc(tmp);

  until False;
end;

procedure TSheets.Put(const Index: Integer; const Value: TSheetItem);
begin
  inherited Items[Index]:=Value;
end;

end.
