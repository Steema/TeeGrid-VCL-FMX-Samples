object SampleData: TSampleData
  OldCreateOrder = False
  Height = 337
  Width = 267
  object Sqlite_demoConnection: TFDConnection
    Params.Strings = (
      'Database=D:\apps\Embarcadero\Studio\19.0\Samples\Data\FDDemo.sdb'
      'ConnectionDef=SQLite_Demo')
    Connected = True
    LoginPrompt = False
    Left = 47
    Top = 22
  end
  object CustomersTable: TFDQuery
    Connection = Sqlite_demoConnection
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    SQL.Strings = (
      'SELECT * FROM Customers')
    Left = 47
    Top = 85
  end
  object OrdersTable: TFDQuery
    Active = True
    Connection = Sqlite_demoConnection
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    SQL.Strings = (
      'SELECT * FROM Orders'
      'where CustomerID = :Cust')
    Left = 49
    Top = 147
    ParamData = <
      item
        Name = 'CUST'
        DataType = ftString
        ParamType = ptInput
        Value = Null
      end>
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 48
    Top = 208
  end
end
