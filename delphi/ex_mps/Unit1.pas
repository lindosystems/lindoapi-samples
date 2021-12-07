unit unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Math;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Button1: TButton;
    gbx2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labObj: TLabel;
    labIter: TLabel;
    labStat: TLabel;
    gbx1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    labVer: TLabel;
    labBuiltOn: TLabel;
    gbx3: TGroupBox;
    Label8: TLabel;
    label11: TLabel;
    Edit1: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    labvar: TLabel;
    labcon: TLabel;
    labint: TLabel;
    labnonz: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Privates }
  public
    { Publics }
  end;

var
  Form1: TForm1;



implementation

// Lindo.pas should be in search path.
// Update [Search path] entry in Project|Options|Directories tab if necessary.
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
//         A simple callback implementation
//
function  MyCallback (prob: Pointer; nLocation: Integer;
                      nUserData : Pointer):Integer; stdcall;
var
   pobj : double;
   status, iter : integer;
   is_mip: ^integer;
begin

   is_mip :=  nUserData;

   if ( (is_mip^ = 0) and ((nLocation =LSLOC_PRIMAL) or (nLocation = LSLOC_DUAL) or
        ((nLocation = LSLOC_EXIT_SOLVER) and (is_mip^ = 0))) )  then
   begin
      { get intermediate optimization statistics }
      LSgetCallbackInfo(prob,nLocation,LS_IINFO_SIM_ITER,iter);
      LSgetCallbackInfo(prob,nLocation,LS_DINFO_POBJ,pobj);
      LSgetCallbackInfo(prob,nLocation,LS_IINFO_STATUS,status);

      { update progress information }
      form1.labObj.Caption := FloatToStr(pobj);
      form1.labIter.Caption := IntToStr(iter);
      form1.labStat.Caption := IntToStr(status);
      if (status = LS_STATUS_BASIC_OPTIMAL) then
      begin
        form1.labStat.Caption := 'Optimal Solution is found.';
      end;
   end
   else
   begin
      { get intermediate optimization statistics }
      LSgetCallbackInfo(prob,nLocation,LS_IINFO_MIP_SIM_ITER,iter);
      LSgetCallbackInfo(prob,nLocation,LS_DINFO_MIP_OBJ,pobj);
      LSgetCallbackInfo(prob,nLocation,LS_IINFO_MIP_STATUS,status);

      { update progress information }
      form1.labObj.Caption := FloatToStr(pobj);
      form1.labIter.Caption := IntToStr(iter);
      form1.labStat.Caption := IntToStr(status);
      if (status = LS_STATUS_OPTIMAL) then
      begin
        form1.labStat.Caption := 'Optimal Solution is found.';
      end;
   end;

   form1.Update;

   { return a nonzero value to halt optimization }
   MyCallback := 0;
end;

function  NextMIPCallback (prob: Pointer;
                      nUserData : Pointer;
                      dObj: Double ;
                    Var pX :Double):Integer; stdcall;
var
   cnt: ^integer;
begin

   cnt :=  nUserData;

   // count down from nMaxSols
   cnt^ := cnt^ + 1;

   { update progress information }
   form1.label12.Caption := FloatToStr(dObj);
   form1.label13.Caption := IntToStr(cnt^);
   form1.Update;

   { return a nonzero value to halt optimization }
   NextMIPCallback := 0;
end;

////////////////////////////////////////////////////////////////////////////////
//         ClickToSolve
//
procedure TForm1.Button2Click(Sender: TObject);
var
  szLicence : string;
  szMPSFile : TLindoString;
  nEnv,nModel : Pointer ;
  obj : double ;
  adX   : Array of double;
  adY   : Array of double;
  cbf   : cbFunc_t;
  cbfNext : cbMIPFunc_t;

  // errorcode should be checked after every call
  errorCode : integer ;

  status, nvars, ncons, ncont, nonz, nMaxSols, nBestSols : integer;
  is_mip : integer;

begin

    Math.SetExceptionMask(exAllArithmeticExceptions);

    // read license from file
    SetLength(szLicence, LS_MAX_ERROR_MESSAGE_LENGTH);
    LSloadLicenseString (TLindoPChar('..\..\..\license\lndapi130.lic'),TLindoPChar(szLicence));

    // create Lindo environment handle
    nEnv := LScreateEnv (errorcode, TLindoPChar(szLicence));
    ErrorCheck(nEnv, errorCode);

    // create Lindo model handle
    nModel :=  LScreateModel (nEnv,errorcode) ;
    ErrorCheck(nEnv, errorCode);

    // read the mps file
    szMPSFile :=  TLindoString(Edit1.Text);
    errorCode := LSreadMPSFile(nModel, TLindoPChar(szMPSFile),LS_UNFORMATTED_MPS) ;
    if (errorcode <> LSERR_NO_ERROR) then
    begin
      errorCode := LSreadLINDOFile(nModel, TLindoPChar(szMPSFile));
      if (errorcode <> LSERR_NO_ERROR) then
      begin
        errorCode := LSreadMPIFile(nModel, TLindoPChar(szMPSFile));
        ErrorCheck(nEnv, errorCode);
      end;
    end;

    // get simple model statistics
    errorCode := LSgetInfo(nModel, LS_IINFO_NUM_VARS, nvars) ;
    ErrorCheck(nEnv, errorCode);
    errorCode := LSgetInfo(nModel, LS_IINFO_NUM_CONS, ncons) ;
    ErrorCheck(nEnv, errorCode);
    errorCode := LSgetInfo(nModel, LS_IINFO_NUM_NONZ, nonz) ;
    ErrorCheck(nEnv, errorCode);
    errorCode := LSgetInfo(nModel, LS_IINFO_NUM_CONT, ncont) ;
    ErrorCheck(nEnv, errorCode);
    labcon.Caption := IntToStr(ncons);
    labvar.Caption := IntToStr(nvars);
    labint.Caption := IntToStr(nvars-ncont);
    labnonz.Caption := IntToStr(nonz);
    form1.Update;

    if (ncont = nvars) then begin
       is_mip := 0;
    end else begin
       is_mip := 1;
    end;

    // set the callback function
    cbf := MyCallback;
    errorCode := LSsetCallback(nModel, @cbf, @is_mip);
    ErrorCheck(nEnv, errorCode);

    if (is_mip = 0) then
    begin
    // if the input model is LP
    errorCode := LSoptimize(nModel, LS_METHOD_FREE, status) ;
    errorCode := LSgetInfo(nModel, LS_DINFO_POBJ, obj) ;

    // set length for primal solution vector
    setLength(adX,nvars);
    errorCode := LSgetPrimalSolution(nModel, adX[0]);
    end
    else
    begin
    // if the input model is MIP
    errorCode := LSSolveMIP(nModel, status) ;
    errorCode := LSgetInfo(nModel, LS_DINFO_MIP_OBJ, obj) ;
    // get k-best solutions (k=10)
    nMaxSols := 10;
    nBestSols := 0;
    cbfNext := NextMIPCallback;
    errorCode := LSsetCallback(nModel, nil, @is_mip);
    errorCode := LSgetKBestMIPSols(nModel,nil,@cbfNext,@nBestSols,nMaxSols);
    errorCode := LSgetMIPPrimalSolution(nModel, adX[0]);
    end;

    form1.labStat.Caption := 'Done';
    form1.Update;

    // write solution to a file
    errorCode := LSwriteSolution (nModel,'out.sol') ;

    LSdeleteEnv(nEnv) ;
end;

////////////////////////////////////////////////////////////////////////////////
//          ClickToQuit
//

procedure TForm1.Button1Click(Sender: TObject);
begin
halt;
end;

////////////////////////////////////////////////////////////////////////////////
//         LoadForm
//

procedure TForm1.FormCreate(Sender: TObject);
var
  szVersion, szBuiltOn : string;
begin

    // display Lindo API version
    //
    SetLength(szVersion, 255); SetLength(szBuiltOn, 255);
    LSgetVersionInfo(TLindoPChar(szVersion), TLindoPChar(szBuiltOn));
    labVer. Caption := TLindoPChar(szVersion);
    labBuiltOn. Caption := TLindoPChar(szBuiltOn);

    //  record the name of the MPS file to solve
    Edit1.Text := '..\..\data\bm23.mps';
    labObj.Caption := '';
    label12.Caption := '';
    label13.Caption := 'K';
    labIter.Caption := '';
    labStat.Caption := '';
end;

end.
