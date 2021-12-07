object Form1: TForm1
  Left = 480
  Top = 270
  Caption = 'A Simple MPS Solver'
  ClientHeight = 296
  ClientWidth = 444
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -8
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button2: TButton
    Left = 140
    Top = 264
    Width = 69
    Height = 21
    Caption = 'Solve'
    TabOrder = 0
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 232
    Top = 264
    Width = 69
    Height = 21
    Caption = 'Quit'
    TabOrder = 1
    OnClick = Button1Click
  end
  object gbx2: TGroupBox
    Left = 224
    Top = 104
    Width = 193
    Height = 145
    Caption = 'Solution'
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 49
      Height = 13
      Caption = 'Iterations :'
    end
    object Label2: TLabel
      Left = 8
      Top = 20
      Width = 48
      Height = 13
      Caption = 'Objective:'
    end
    object Label3: TLabel
      Left = 8
      Top = 60
      Width = 48
      Height = 13
      Caption = 'Status     :'
    end
    object labObj: TLabel
      Left = 64
      Top = 20
      Width = 18
      Height = 13
      Caption = '???'
    end
    object labIter: TLabel
      Left = 64
      Top = 40
      Width = 18
      Height = 13
      Caption = '???'
    end
    object labStat: TLabel
      Left = 64
      Top = 60
      Width = 18
      Height = 13
      Caption = '???'
    end
    object Label10: TLabel
      Left = 21
      Top = 84
      Width = 49
      Height = 13
      Caption = '- Best Obj:'
    end
    object Label12: TLabel
      Left = 79
      Top = 84
      Width = 18
      Height = 13
      Caption = '???'
    end
    object Label13: TLabel
      Left = 8
      Top = 84
      Width = 6
      Height = 13
      Caption = '?'
    end
  end
  object gbx1: TGroupBox
    Left = 24
    Top = 8
    Width = 393
    Height = 81
    Caption = 'LINDO API'
    TabOrder = 3
    object Label4: TLabel
      Left = 8
      Top = 20
      Width = 41
      Height = 13
      Caption = 'Version :'
    end
    object Label5: TLabel
      Left = 8
      Top = 40
      Width = 50
      Height = 13
      Caption = 'Build date:'
    end
    object labVer: TLabel
      Left = 56
      Top = 20
      Width = 30
      Height = 13
      Caption = 'labVer'
    end
    object labBuiltOn: TLabel
      Left = 64
      Top = 40
      Width = 46
      Height = 13
      Caption = 'labBuitOn'
    end
  end
  object gbx3: TGroupBox
    Left = 24
    Top = 104
    Width = 193
    Height = 145
    Caption = 'Model'
    TabOrder = 4
    object Label8: TLabel
      Left = 8
      Top = 18
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object label11: TLabel
      Left = 16
      Top = 52
      Width = 46
      Height = 13
      Caption = 'Variables:'
    end
    object Label6: TLabel
      Left = 16
      Top = 76
      Width = 55
      Height = 13
      Caption = 'Constraints:'
    end
    object Label7: TLabel
      Left = 16
      Top = 96
      Width = 41
      Height = 13
      Caption = 'Integers:'
    end
    object Label9: TLabel
      Left = 16
      Top = 118
      Width = 48
      Height = 13
      Caption = 'Nonzeros:'
    end
    object labvar: TLabel
      Left = 88
      Top = 52
      Width = 12
      Height = 13
      Caption = '***'
    end
    object labcon: TLabel
      Left = 88
      Top = 76
      Width = 12
      Height = 13
      Caption = '***'
    end
    object labint: TLabel
      Left = 88
      Top = 96
      Width = 12
      Height = 13
      Caption = '***'
    end
    object labnonz: TLabel
      Left = 88
      Top = 118
      Width = 12
      Height = 13
      Caption = '***'
    end
    object Edit1: TEdit
      Left = 48
      Top = 16
      Width = 137
      Height = 21
      TabOrder = 0
      Text = 'input file'
    end
  end
end
