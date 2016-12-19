Connecting TeeGrid with a custom TList

```delphi
uses
  Tee.Grid.Data.Rtti;
  
  TeeGrid1.Data:= TVirtualListData<TPerson>.Create(MyData);
```
