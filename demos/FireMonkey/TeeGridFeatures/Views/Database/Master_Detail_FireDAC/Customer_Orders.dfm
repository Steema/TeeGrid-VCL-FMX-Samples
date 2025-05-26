object SampleData: TSampleData
  Height = 337
  Width = 267
  object Sqlite_demoConnection: TFDConnection
    Params.Strings = (
      'Database=FDDemo.sdb'
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 103
    Top = 38
  end
  object CustomersTable: TFDQuery
    Connection = Sqlite_demoConnection
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    SQL.Strings = (
      'SELECT * FROM Customers')
    Left = 103
    Top = 101
  end
  object OrdersTable: TFDQuery
    Connection = Sqlite_demoConnection
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    SQL.Strings = (
      'SELECT * FROM Orders'
      'where CustomerID = :Cust')
    Left = 105
    Top = 163
    ParamData = <
      item
        Name = 'CUST'
        DataType = ftString
        ParamType = ptInput
        Value = Null
      end>
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 104
    Top = 224
  end
end
