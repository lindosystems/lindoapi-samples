/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_nlp1.ox
  Purpose: Solve a NLP using the black-box stye interface.
  Model  : A nonlinear model with multiple local minimizers.

        minimize  f(x,y) =  3*(1-x)^2*exp(-(x^2) - (y+1)^2)
                         - 10*(x/5 - x^3 - y^5).*exp(-x^2-y^2)
                         - 1/3*exp(-(x+1)^2 - y^2);
        subject to
                         x^2 + y   <=  6;
                         x   + y^2 <=  6;
*/
#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>


/* the summands of the objective function */
g1(X,Y) {return ( exp( -pow(X  ,2)  - pow(Y+1,2) )  );}
g2(X,Y) {return ( exp( -pow(X  ,2)  - pow(Y  ,2) )  );}
g3(X,Y) {return ( exp( -pow(X+1,2)  - pow(Y  ,2) )  );}
f1(X,Y) {return ( pow(1-X,2)                        );}
f2(X,Y) {return ( X/5 - pow(X  ,3)  - pow(Y  ,5)    );}

/* partial derivatives of the summands */
dxg1(X,Y)  {return ( g1(X,Y)*(-2)*X     );}
dyg1(X,Y)  {return ( g1(X,Y)*(-2)*(Y+1) );}
dxg2(X,Y)  {return ( g2(X,Y)*(-2)*X     );}
dyg2(X,Y)  {return ( g2(X,Y)*(-2)*Y     );}
dxg3(X,Y)  {return ( g3(X,Y)*(-2)*(X+1) );}
dyg3(X,Y)  {return ( g3(X,Y)*(-2)*Y     );}
dxf1(X,Y)  {return ( 2*(1-X)            );}
dyf1(X,Y)  {return ( 0                  );}
dxf2(X,Y)  {return ( 1/5 - 3*pow(X,2)   );}
dyf2(X,Y)  {return ( -5*pow(Y,4)        );}

/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
static decl s_iCounter = 0, s_nCalls = 0;
print_log(const model, const iLoc, const cbData)
{
  decl iter=0,niter,biter,siter;
  decl npass;
  decl pfeas=0.0,pobj=0.0;
  decl bestobj;

  if (iLoc==LSLOC_LOCAL_OPT)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_NLP_ITER,&niter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_SIM_ITER,&siter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_BAR_ITER,&biter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MSW_PASS,&npass);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MSW_POBJ,&bestobj);
    iter = niter+siter+biter;
    println("(New Local Sol)","%5d", npass, "%6d", iter,"%12.3f", pobj,"%12.3f", pfeas,"%12.3f", bestobj);
    s_iCounter++;
  }

  else if (iLoc == LSLOC_CONOPT)
  {
    if (s_nCalls == 0)
    {
      println("%21s", "Pass","%6s", "Iter","%12s", "Objective","%12s", "Infeas","%12s","Best");
    }
    /*
    LSgetCallbackInfo(model,iLoc,LS_IINFO_NLP_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    print("                %5s %5d %11.3f %11.3f %11s\n","",iter,pobj,pfeas,"","");
    */
  }

  s_nCalls++;

  return 0;
} /*print_log*/

/****************************************************************
   Callback function to compute function values
 ****************************************************************/
Funcalc8(const pModel,const    pUserData,
                             const      nRow  ,const  pdX,
                             const      nJDiff,const  dXJBase,
                             const    pdFuncVal,const  pReserved)
{
  decl val=0.0, X = pdX[0], Y = pdX[1];
  decl nerr=0;
  /* compute objective's functional value*/
  if (nRow==-1)
    val = 3*f1(X,Y)*g1(X,Y) - 10*f2(X,Y)*g2(X,Y) - g3(X,Y)/3;
  /* compute constaint 0's functional value */
  else if (nRow==0)
    val = X*X + Y - 6.0;
  /* compute constaint 1's functional value */
  else if (nRow==1)
    val = X + Y*Y - 6.0;
  pdFuncVal[0]=val;
  return nerr;
} /*Funcalc8*/

/****************************************************************
  Callback function to compute derivatives
 ****************************************************************/
Gradcalc8(const pModel, const pUserData,
                           const nRow,const pdX, const lb,
                           const ub, const nNewPnt, const nNPar,
                           const parlist, const aPartial)
{
  decl i2,nerr=0;
  decl X=pdX[0], Y=pdX[1];
  decl partial = zeros(1, nNPar);

  /*zero out the partials */
  for (i2=0;i2<nNPar;i2++) partial[i2]=0.0;

  /* partial derivatives of the objective function */
  if (nRow==-1) {
     for (i2=0;i2<nNPar;i2++) {
       if (lb[parlist[i2]]!=ub[parlist[i2]]) {
           if (parlist[i2]==0) {
             partial[i2]=
                  3*(dxf1(X,Y)*g1(X,Y) + f1(X,Y)*dxg1(X,Y) )
              -  10*(dxf2(X,Y)*g2(X,Y) + f2(X,Y)*dxg2(X,Y) )
              - 1/3*(dxg3(X,Y));
           } else if (parlist[i2]==1) {
             partial[i2]=
                  3*(dyf1(X,Y)*g1(X,Y) + f1(X,Y)*dyg1(X,Y) )
              -  10*(dyf2(X,Y)*g2(X,Y) + f2(X,Y)*dyg2(X,Y) )
              - 1/3*(dyg3(X,Y));
           }
       }
     }
  }
  /* partial derivatives of Constraint 0 */
  else if (nRow==0) {
     for (i2=0;i2<nNPar;i2++) {
       if (lb[parlist[i2]]!=ub[parlist[i2]]) {
         if (parlist[i2]==0) {
           partial[i2]=2.0*X;
         } else if (parlist[i2]==1) {
           partial[i2]=1;
         }
       }
     }
  }
  /* partial derivatives of Constraint 1 */
  else if (nRow==1) {
     for (i2=0;i2<nNPar;i2++) {
       if (lb[parlist[i2]]!=ub[parlist[i2]]) {
         if (parlist[i2]==0) {
           partial[i2]=1;
         } else if (parlist[i2]==1) {
           partial[i2]=2.0*Y;
         }
       }
     }
  }
  aPartial[0] = partial;
  return nerr;
}
/* main entry point*/
main()
{
  decl nErrorCode;
  decl pEnv    = 0;
  decl model  = 0;
  decl errors=0, status;
  decl lb=zeros(1,2),ub=zeros(1,2),A=zeros(1,4),rhs=zeros(1,2),cost=zeros(1,2);
  decl Abegcol=zeros(1,3),Arowndx=zeros(1,4),Alencol=zeros(1,2),Nobjndx=zeros(1,2);
  decl m,n,nz,  Nnlobj, counter = 0;
  decl contype = new string[2];

 /*****************************************************************
  * Step 1: Create a model in the environment.
  *****************************************************************/
   pEnv = OxLScreateEnv();

  model = LScreateModel(pEnv,&nErrorCode);
  LSerrorCheck(pEnv, nErrorCode);

 /*****************************************************************
  * Step 2: Specify the LP portion of the model.
  *****************************************************************/
  /* model dimensions */
  m = n = 2;  nz = 4;

  /* The indices of the first nonzero in each column */
  Abegcol[0]=0; Abegcol[1]=2; Abegcol[2]=4;

  /* The length of each column */
  Alencol[0]=2; Alencol[1]=2;

  /* The row indices of the nonzero coefficients */
  Arowndx[0]=0; Arowndx[1]=1;  Arowndx[2]=0;  Arowndx[3]=1;

  /* The nonzero coefficients of the linear portion of the model*/
  /* The NLP elements have a zero at each of their occurence    */
  A[0]=0.0; A[1]=1.0; A[2]=1.0; A[3]=0.0;

  /* The objective coefficients of the linear portion of the model*/
  cost[0]=0.0; cost[1]=0.0;

  /* lower bounds on variables */
  lb[0]=-3.0  ; ub[0]= 3.0;   lb[1]=-3.0  ; ub[1]= 3.0;

  /* The right-hand sides of the constraints */
  rhs[0]=0.0; rhs[1]=0.0;

  /* The constraint types */
  contype[0]='L'; contype[1]='L';

  /* Load in nonzero structure and linear/constant terms.  */
  nErrorCode=LSloadLPData(model,m,n,LS_MIN,0.0,cost,rhs,contype,nz,
                         Abegcol,Alencol,A,Arowndx,lb,ub);
  LSerrorCheck(pEnv, nErrorCode);
 /*****************************************************************
  * Step 3: Specify the NLP portion of the model.
  *****************************************************************/
  /* The number of nonlinear variables in each column */
  Alencol[0]=1; Alencol[1]=1;

  /* The indices of the first nonlinear variable in each column */
  Abegcol[0]=0; Abegcol[1]=1; Abegcol[2]=2;

  /* The indices of nonlinear constraints */
  Arowndx[0]=0;
  Arowndx[1]=1;

  /* The indices of variables that are nonlinear in the objective*/
  Nobjndx[0]=0;
  Nobjndx[1]=1;

  /* Number nonlinear variables in cost. */
  Nnlobj = 2;

  /* Load the nonlinear structure */
  nErrorCode=LSloadNLPData(model,Abegcol,Alencol,
            <>,Arowndx,Nnlobj,Nobjndx,<>);
  print("\nThe model is installed successfully...\n");
 /*****************************************************************
  * Step 4: Set up callback functions
  *****************************************************************/
  /* Install the routine that will calculate the function values. */
  nErrorCode=LSsetFuncalc(model,Funcalc8,0);
  LSerrorCheck(pEnv, nErrorCode);

  /* Install the routine that will calculate the gradient */
  nErrorCode=LSsetGradcalc(model,Gradcalc8,0,0,<>);
  LSerrorCheck(pEnv, nErrorCode);

  /* Install a callback function */
  LSsetCallback(model, print_log, 0);

    /* Set the print level to 1 */
  nErrorCode=LSsetModelIntParameter(model,LS_IPARAM_NLP_PRINTLEVEL,1);

  /* Turn multi-start search on */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);

  /* Set maximum number of local optimizations */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_MAXLOCALSEARCH,1);

 /*****************************************************************
  * Step 5: Solve the model
  *****************************************************************/
  nErrorCode=LSoptimize(model,LS_METHOD_FREE, &status);
  if (nErrorCode!=LSERR_NO_ERROR)
    return nErrorCode;

  {
    decl i;
    decl objval, primal;
    nErrorCode = LSgetInfo(model, LS_DINFO_POBJ, &objval);
    nErrorCode = LSgetPrimalSolution(model, &primal);
    print("\n\n\nPrinting the best local optimum found.\n");
    println("obj  = ",objval);
    for (i=0; i<2; i++) println("x[", i, "] = ","%f",primal[i]);
  }
 /*****************************************************************
  * Step 6: Delete the model & pEnv space
  *****************************************************************/
  LSdeleteModel(&model);
  LSdeleteEnv(&pEnv);

  return nErrorCode;
} /*main*/
