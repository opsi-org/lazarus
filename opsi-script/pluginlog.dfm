object DataModule1: TDataModule1
  OldCreateOrder = False
  Left = 454
  Top = 916
  Height = 150
  Width = 215
  object IdTCPClient1: TIdTCPClient
    MaxLineAction = maException
    ReadTimeout = 0
    Host = '127.0.0.1'
    Port = 0
    Left = 56
    Top = 40
  end
  object IdUDPClient1: TIdUDPClient
    Active = True
    Host = '127.0.0.1'
    Port = 4445
    Left = 136
    Top = 40
  end
end
