object SampleData: TSampleData
  OldCreateOrder = False
  Height = 337
  Width = 267
  object Sqlite_demoConnection: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    Connected = True
    LoginPrompt = False
    Left = 47
    Top = 22
  end
  object CustomersTable: TFDQuery
    Active = True
    Connection = Sqlite_demoConnection
    SQL.Strings = (
      'SELECT * FROM Customers')
    Left = 47
    Top = 85
  end
  object DataSource1: TDataSource
    DataSet = CustomersTable
    Left = 144
    Top = 85
  end
  object OrdersTable: TFDQuery
    Connection = Sqlite_demoConnection
    SQL.Strings = (
      'SELECT * FROM Orders'
      'where CustomerID = :Cust')
    Left = 49
    Top = 147
    ParamData = <
      item
        Name = 'CUST'
        ParamType = ptInput
      end>
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 48
    Top = 216
  end
end
