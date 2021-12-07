program ex_mps;

uses
  Forms,
  unit1 in 'unit1.pas' {Form1},
  Lindo;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
