object SampleData: TSampleData
  OnCreate = DataModuleCreate
  Height = 337
  Width = 267
  object Sqlite_demoConnection: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\Public\Documents\Embarcadero\Studio\23.0\Sampl' +
        'es\data\FDDemo.sdb'
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 111
    Top = 22
  end
  object CustomersTable: TFDQuery
    Connection = Sqlite_demoConnection
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    SQL.Strings = (
      'SELECT * FROM Customers')
    Left = 111
    Top = 85
  end
  object OrdersTable: TFDQuery
    Connection = Sqlite_demoConnection
    FetchOptions.AssignedValues = [evRecordCountMode]
    FetchOptions.RecordCountMode = cmTotal
    SQL.Strings = (
      'SELECT * FROM Orders'
      'where CustomerID = :Cust')
    Left = 105
    Top = 147
    ParamData = <
      item
        Name = 'CUST'
        ParamType = ptInput
      end>
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 104
    Top = 216
  end
end
