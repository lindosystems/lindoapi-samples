{  A Delphi programming example of interfacing with the
'  LINDO API.
'
'  The problem:
'
'  A nonlinear model with multiple local minimizers.
'
'           maximize  abs( x0 + 1) + .4 * x1;
'           s.t.     x0           + x1 - 4      <= 0;
'                    x0 * x1      + x1 - 6      <= 0;
'                    x0 * x1                    <= 0;
'                    max(x0 , x1 + 1)           >= 0;
'                    if(x1, 1, x1)              <= 0;
'                   (x1 * 2 * x1  -  x1) * x0  <= 0;
'                   -100  <=  x0  <=  100
'                    x1 is binary
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
var
szLicence : string;
nEnv : Pointer;
errorcode : Integer;
prob : Pointer;
status : Integer;
nobjs : Integer;
ncons : Integer;
nvars : Integer;
nnums : Integer;
lsize : Integer;
objsense : array of Integer;
ctype : TLindoPChar;
vtype : TLindoPChar;
code : array of Integer;
varndx : array of Integer;
numval : array of Double;
varval : array of Double;
objs_beg : array of Integer;
objs_length : array of Integer;
cons_beg : array of Integer;
cons_length : array of Integer;
lwrbnd : array of Double;
uprbnd : array of Double;
nLinearz : Integer;
nAutoDeriv : Integer;
ikod : Integer;
iobj : Integer;
icon : Integer;
nLinearity : Integer;
objval : Double;
primal : array of Double;

 begin

  //>>> Step 1 <<<:

    Math.SetExceptionMask(exAllArithmeticExceptions);
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

  //'>>> Step 3 <<<:  Specify the model.

  SetLength(objsense, 1);
  SetLength(code, 100);
  SetLength(varndx, 2);
  SetLength(numval, 8);
  SetLength(varval, 2);
  SetLength(objs_beg, 1);
  SetLength(objs_length, 1);
  SetLength(cons_beg, 6);
  SetLength(cons_length , 6);
  SetLength(lwrbnd, 2);
  SetLength(uprbnd, 2);

      //Number of constraints
      ncons := 6 ;
      // Number of objectives
      nobjs := 1;
      // Number of variables
      nvars := 2;
      // Number of real number constants
      nnums := 5;
      // Variable index
      varndx[0]:= 1;
      varndx[1]:= 2;
      // Lower bounds of variables
      lwrbnd[0]:=-100.0;
      lwrbnd[1]:=0.0;
      // Upper bounds of variables
      uprbnd[0]:=100.0;
      uprbnd[1]:=1.0;
      // Starting point of variables
      varval[0]:=4.0;
      varval[1]:=0.0;
      // Variable type, C= continuous, B = binary
      vtype := 'CB';
      // L= less than or equal to, G= greater than or equal to
      ctype := 'LLLGLL';

     // Double Precision constants in the model
      numval[0]:=1.0;
      numval[1]:=0.4;
      numval[2]:=6.0;
      numval[3]:=4.0;
      numval[4]:=2.0;
      // Count for instruction code
    ikod := 0;
      // Count for objective row
    iobj := 0;
      // Count for constraint row
    icon := 0;

        //  Instruction code of the objective:
       //
      //  max abs( x0 + 1) + .4 * x1;

      //  Direction of optimization
      objsense[iobj]:= LS_MAX;
      // Beginning position of objective
      objs_beg[iobj]:= ikod;

      // Instruction list code
      ikod := 0;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PLUS;
      ikod := ikod+1;
      code[ikod]:= EP_ABS;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_MULTIPLY;
      ikod := ikod+1;
      code[ikod]:= EP_PLUS;
      ikod := ikod+1;
  // Length of objective
      objs_length[iobj] := ikod - objs_beg[iobj];

       //  Instruction code of constraint 0:
       //
       //  x0  + x1 - 4 <= 0;

  // Constraint type

      // Beginning position of constraint 0
      cons_beg[icon]:= ikod;
      // Instruction list code
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_PLUS;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 3;
      ikod := ikod+1;
      code[ikod]:= EP_MINUS;
      ikod := ikod+1;
  // Length of constraint 0
      cons_length[icon] := ikod - cons_beg[icon];
      // Increment the constraint count
  icon := icon+1;


       //  Instruction code of constraint 1:
       //
       //  x0 * x1      + x1 - 6 <= 0;

  // Constraint type
      // Beginning position of constraint 1
      cons_beg[icon]:=  ikod;
      // Instruction list code
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_MULTIPLY;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_PLUS;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 2;
      ikod := ikod+1;
      code[ikod]:= EP_MINUS;
      ikod := ikod+1;
      // Length of constraint 1
      cons_length[icon] := ikod - cons_beg[icon];
      // Increment the constraint count
  icon := icon+1;
 

       //  Instruction code of constraint 2:
       //
       // x0 * x1           <= 0;

  // Constraint type
      // Beginning position of constraint 2
      cons_beg[icon]:=  ikod;
      // Instruction list code
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_MULTIPLY;
      ikod := ikod+1;
  // Length of constraint 2
      cons_length[icon] := ikod - cons_beg[icon];
      // Increment the constraint count
      icon := icon+1;


       //  Instruction code of constraint 3:
      //  max(x0 , x1 + 1)        >= 0;

  // Constraint type
      // Beginning position of constraint 3
      cons_beg[icon]:=  ikod;
      // Instruction list code
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PLUS;
      ikod := ikod+1;
      code[ikod]:= EP_MAX;
      ikod := ikod+1;
      code[ikod]:= 2;
      ikod := ikod+1;
  // Length of constraint 3
      cons_length[icon] := ikod - cons_beg[icon];
      // Increment the constraint count
  icon := icon+1;


       //  Instruction code of constraint 4:
       //  if(x1, 1, x1)        <= 0;

  // Constraint type
      // Beginning position of constraint 4
      cons_beg[icon]:=  ikod;
      // Instruction list code
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_IF;
      ikod := ikod+1;
  // Length of constraint 4
      cons_length[icon] := ikod - cons_beg[icon];
      // Increment the constraint count
  icon := icon+1;


       //  Instruction code of constraint 5:
      //  (x1 * 2 * x1  -  x1) * x0      <= 0;

  // Constraint type
      // Beginning position of constraint 5
      cons_beg[icon]:= ikod;
      // Instruction list code
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_NUM;
      ikod := ikod+1;
      code[ikod]:= 4;
      ikod := ikod+1;
      code[ikod]:= EP_MULTIPLY;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_MULTIPLY;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 1;
      ikod := ikod+1;
      code[ikod]:= EP_MINUS;
      ikod := ikod+1;
      code[ikod]:= EP_PUSH_VAR;
      ikod := ikod+1;
      code[ikod]:= 0;
      ikod := ikod+1;
      code[ikod]:= EP_MULTIPLY;
      ikod := ikod+1;
  // Length of constraint 5
      cons_length[icon] := ikod - cons_beg[icon];
 
      // Total number of items in the instruction list
      lsize := ikod;
      // Set linearization level, before a call to LSloadInstruct.
      // If not specified, the solver will decide
      nLinearz := 1;
      errorCode := LSsetModelIntParameter (prob, LS_IPARAM_NLP_LINEARZ, nLinearz);
      ErrorCheck(nEnv, errorCode);

      // Set up automatic differentiation, before a call to
      // LSloadInstruct. If not specified, the numerical derivative
      // will be applied
      nAutoDeriv := 1;
      errorCode := LSsetModelIntParameter (prob, LS_IPARAM_NLP_AUTODERIV, nAutoDeriv);
      ErrorCheck(nEnv, errorCode);
      // Pass the instruction list to problem structure
      // by a call to LSloadInstruct()
      errorCode := LSloadInstruct (prob, ncons, nobjs, nvars, nnums, objsense[0], ctype, vtype, code[0], lsize, varndx[0], numval[0], varval[0], objs_beg[0], objs_length[0], cons_beg[0], cons_length[0], lwrbnd[0], uprbnd[0]);
      ErrorCheck(nEnv, errorCode);

  //'>>> Step 4 <<<:  Perform the optimization.
    errorcode := LSsolveMIP(prob, status);
    ErrorCheck(nEnv, errorCode);

  //'>>> Step 5 <<<:  Retrieve the solution.
      SetLength(primal, 100);
      objval :=0.0;
      // Get the optimization result
      LSgetInfo(prob, LS_DINFO_MIP_OBJ, objval);
      ErrorCheck(nEnv, errorCode);

      LSgetMIPPrimalSolution( prob, primal[0]) ;
      ErrorCheck(nEnv, errorCode);

      ShowMessage ('Objective value: ' + FloattoStr(objval));
      ShowMessage ('primal[0]: ' + FloattoStr(primal[0]));
      ShowMessage ('primal[1]: ' + FloattoStr(primal[1]));

      // Get the linearity of the solved model
      errorCode := LSgetInfo (prob, LS_IINFO_NLP_LINEARITY, nLinearity);
      ErrorCheck(nEnv, errorCode);

      // Report the status of solution
      if nLinearity > 0 then
      begin
      ShowMessage ('Solution Status: Globally Optimal');
      end
      else
      ShowMessage ('Model is nonlinear. Solution Status: Locally Optimal');


  //'>>> Step 6 <<< Delete the LINDO environment.
    LSdeleteEnv(nEnv)

end;

end.
