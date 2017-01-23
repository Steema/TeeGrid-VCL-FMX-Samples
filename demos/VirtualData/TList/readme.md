Connecting TeeGrid with a custom TList of records or objects

```delphi
uses
  Tee.Grid.Data.Rtti;
  
type
  TPerson=record // <-- can also be a class
    ....fields and properties...
  end;
  
var
  MyPersons : TList<TPerson>;
  
  MyPersons:= TList<TPerson>.Create; ... fill list ...
  
  TeeGrid1.Data:= TVirtualListData<TPerson>.Create(MyPersons);
  
  // Optional parameters:
  // uses System.Rtti, System.TypInfo;
  
  // Member visibility (Private, Protected, Public and / or Published)
  
  TVirtualListData<TPerson>.Create(MyPersons, [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished]);
  
  // Fields and / or Properties
  
  TVirtualListData<TPerson>.Create(MyPersons, [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished],
          TRttiMembers.Both);
          
  // Just this class or all the ancestor classes also
  
  TVirtualListData<TPerson>.Create(MyPersons, [TMemberVisibility.mvPublic, TMemberVisibility.mvPublished],
          TRttiMembers.Both,
          True);
  
```

Setting the TeeGrid Data property will create all grid columns as usually.

Columns can be customized after that, in the normal way, for example:

```delphi
  TeeGrid1.Columns[0].ParentFormat := False;
  TeeGrid1.Columns[0].Format.Font.Color := TColors.Navy;
```
