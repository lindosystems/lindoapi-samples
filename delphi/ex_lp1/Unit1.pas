{  A Delphi programming example of interfacing with the
'  LINDO API.
'
'  The problem:
'
'     Max = 20 * A + 30 * C
'     S.T.       A +  2 * C  <= 120
'                A           <=  60
'                         C  <=  50
'
'   Solving such a problem with the LINDO API involves
'   the following steps:
'
'      1. Create a LINDO environment.
'      2. Create a model in the environment.
'      3. Specify the model.
'      4. Perform the optimization.
'      5. Retrieve the solution.
'      6. Delete the LINDO environment.}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Lindo;


{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
//         A simple error checker
//
procedure  ErrorCheck (nEnv: Pointer; nErr: Integer);
var
   strErr : Array[0..255] of TLindoChar;
begin

   if (nErr <> LSERR_NO_ERROR) then
   begin
      LSgetErrorMessage(nEnv, nErr, strErr);
      ShowMessage(strErr);
      halt;
   end;
end;

////////////////////////////////////////////////////////////////////////////////
//         Solve on Click
//
procedure TForm1.Button1Click(Sender: TObject);
var con_type : TLindoPChar;
    szLicence : string;
    nEnv      : Pointer ;
    errorcode  : integer;
    m         : integer;
    n         : integer;
    nz        : integer;
    prob      : Pointer;
    Abegcol   : array of Integer;
    Arowndx   : array of Integer;
    Acoef     : array of Double;
    b         : array of Double;
    c         : array of Double;
    obj       : double;
    x         : array of Double;
    adL       : array of Double;
    adU       : array of Double;
    acAcols   : array of Integer;
    LindoFile : TLindoPChar;
    status    : integer;
    szVersion, szBuiltOn, szBuf, szBuf2 : string;
var
  Msg, Title   : array[0..255] of WideChar;

 begin

    Math.SetExceptionMask(exAllArithmeticExceptions);

  //>>> Step 1 <<<:
    SetLength(szVersion, 255); SetLength(szBuiltOn, 255); SetLength(szBuf, 1024); SetLength(szBuf2, 64);
    LSgetVersionInfo(TLindoPChar(szVersion), TLindoPChar(szBuiltOn));

    // read license from file
    SetLength(szLicence, LS_MAX_ERROR_MESSAGE_LENGTH);
    LSloadLicenseString (TLindoPChar('..\..\..\license\lndapi130.lic'),TLindoPChar(szLicence));

    // create Lindo environment handle
    nEnv := LScreateEnv (errorcode, TLindoPChar(szLicence));
    ErrorCheck(nEnv, errorCode);

  //>>> Step 2 <<<:

    // Create a model in the environment.
    prob := LScreateModel(nEnv, errorcode);
    ErrorCheck(nEnv, errorCode);

    LSgetParamMacroName(nEnv, LS_IPARAM_OBJSENSE, TLindoPChar(szBuf2));
    LSgetParamLongDesc(nEnv, LS_IPARAM_OBJSENSE, TLindoPChar(szBuf));
    StringToWideChar(TLindoPChar(szBuf),Msg,255);
    StringToWideChar(TLindoPChar(szBuf2),Title,255);
    Application.MessageBox(Msg,Title,0);

    LSgetParamMacroName(nEnv, LS_IPARAM_NLP_AUTODERIV, TLindoPChar(szBuf2));
    LSgetParamLongDesc(nEnv, LS_IPARAM_NLP_AUTODERIV, TLindoPChar(szBuf));
    StringToWideChar(TLindoPChar(szBuf),Msg,255);
    StringToWideChar(TLindoPChar(szBuf2),Title,255);
    Application.MessageBox(Msg,Title,0);

  //'>>> Step 3 <<<:  Specify the model.

    //'Set the problem sizes

    //'number of constraints
    m := 3 ;

    //'number of variables
    n := 2 ;

    //'objective coefficients
    SetLength(c, n);
    c[0] := 20;
    c[1] := 30;

    //'right-hand-sides of constraints
    SetLength(b, m);
    b[0] := 120;
    b[1] := 60;
    b[2] := 50;

    //'constraint types
    con_type := 'LLL';

    //'index of first nonzero in each column
    SetLength(Abegcol, n+1);
    Abegcol[0] := 0;
    Abegcol[1] := 2;
    Abegcol[2] := 4;

    //'number of nonzeros in constraint matrix
    nz := 4;

    //'the nonzero coefficients
    SetLength(Acoef, nz);
    Acoef[0] := 1;
    Acoef[1] := 1;
    Acoef[2] := 2;
    Acoef[3] := 1;

    //'the row indices of the nonzeros
    SetLength(Arowndx, nz);
    Arowndx[0] := 0;
    Arowndx[1] := 1;
    Arowndx[2] := 0;
    Arowndx[3] := 2;

    SetLength(adL, n);
    adL[0] := 0;
    adL[1] := 0;

    SetLength(adU, n);
    adU[0] := 200;
    adU[1] := 200;

    SetLength(acAcols, n);
    acAcols[0] := 2;
    acAcols[1] := 2;

    // IMPORTANT NOTE:
    // acAcols (number of nonzeros in each column) is optional.
    // A C-type NULL can be passed for all optional arguments in LINDO API.

    // In Delphi, this can be done by passing a zero by *value*, rather than
    // by reference. This requires modifying the associated argument's type
    // in LINDO.PAS. For details on passing by-value and by-reference, refer
    // to your favorite Delphi resource.
    errorcode := LSloadLPData(prob, m, n, LS_MAX, 0, c[0], b[0], con_type, nz,
                  Abegcol[0], acAcols[0] ,Acoef[0], Arowndx[0], adL[0], adU[0]);
    ErrorCheck(nEnv, errorCode);

    LindoFile := 'Lindo.ltx';
    LSwriteLINDOFile(prob, LindoFile);

  //'>>> Step 4 <<<:  Perform the optimization.
    errorcode := LSoptimize(prob, LS_METHOD_PSIMPLEX, status);
    ErrorCheck(nEnv, errorCode);

  //'>>> Step 5 <<<:  Retrieve the solution.
    SetLength(x, n);
    //'Print the objective value and primals
    errorcode := LSgetInfo(prob, LS_DINFO_POBJ, obj);
    ErrorCheck(nEnv, errorCode);

    errorcode := LSgetPrimalSolution(prob, x[0]);
    ErrorCheck(nEnv, errorCode);
    ShowMessage ('Objective value: ' + FloattoStr(obj));

  //'>>> Step 6 <<< Delete the LINDO environment.
    LSdeleteEnv(nEnv)

end;

end.
