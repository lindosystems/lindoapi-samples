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

  File   : ex_nlp3.ox
  Purpose: Solve a MINLP using the black-box stye interface. 

  Model  : A nonlinear model with multiple local minimizers. 
           
        minimize  f(x,y) =  3*(1-x)^2*exp(-(x^2) - (y+1)^2) 
                         - 10*(x/5 - x^3 - y^5).*exp(-x^2-y^2)
                         - 1/3*exp(-(x+1)^2 - y^2);
        subject to
                         x^2 + y   <=  6;
                         x   + y^2 <=  6;
                         x  integer
*/
#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>


/* the summands of the objective function */
g1(X,Y) {return ( exp( -pow(X  ,2)  - pow(Y+1,2) )  );}
g2(X,Y) {return ( exp( -pow(X  ,2)  - pow(Y  ,2) )  );}
g3(X,Y) {return ( exp( -pow(X+1,2)  - pow(Y  ,2) )  );} 
f1(X,Y) {return (        pow(1-X,2)                 );}
f2(X,Y) {return ( X/5 - pow(X  ,3)  - pow(Y  ,5)    );}

/****************************************************************
   Standard callback function to display local solutions
 ****************************************************************/ 
static decl s_iCounter = 0, s_nCalls = 0;
local_sol_log(const model, const iLoc, const cbData)
{
  decl iter=0,niter,biter,siter;
  decl npass, nbrn;
  decl pfeas=0.0,pobj=0.0; 
  decl bestobj;
  if (iLoc==LSLOC_LOCAL_OPT)
  {    
    if (s_iCounter == 0){
      println("%6s","Iter","%12s","Objective","%12s","Infeas","%12s","Best","%11s","Branches");
    }
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_NLP_ITER,&niter); 
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&siter); 
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BAR_ITER,&biter); 
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj); 
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);     
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MSW_POBJ,&bestobj);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BRANCHCOUNT,&nbrn);
    iter = niter+siter+biter;
    println("%6d",iter,"%12.3f",pobj,"%12.3f",pfeas,"%12.3f",bestobj,"%11d",nbrn);  
    s_iCounter++;
  }
  return 0;
} /*local_sol_log*/

/****************************************************************
   Callback function to compute function values
 ****************************************************************/
Funcalc8(const pModel,const pUserData, 
                             const      nRow  ,const  pdX, 
                             const      nJDiff,const  dXJBase,
                             const  pdFuncVal, const  pReserved)
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


/* main entry point*/
main()
{
  decl nErrorCode;
  decl pEnv    = 0;
  decl model  = 0;
  decl errors=0;
  decl lb=new matrix[2],ub=new matrix[2],A=new matrix[4],rhs=new matrix[2],cost=new matrix[2];
  decl Abegcol=new matrix[3],Arowndx=new matrix[4],Alencol=new matrix[2],Nobjndx=new matrix[2];
  decl m,n,nz, Nnlobj, howmany=0;
  decl contype=new string[2],vartype=new string[2];

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
  vartype[0]='I'; vartype[1]='C'; 

  /* Load in nonzero structure and linear/constant terms.  */
  nErrorCode=LSloadLPData(model,m,n,LS_MIN,0.0,cost,rhs,contype,nz,
                         Abegcol,Alencol,A,Arowndx,lb,ub);
  LSerrorCheck(pEnv, nErrorCode);

  nErrorCode=LSloadVarType(model,vartype);
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
 /*****************************************************************
  * Step 4: Set up callback functions
  *****************************************************************/
  /* Install the callback function to call at every local solution */
  LSsetCallback(model,local_sol_log,0);

  /* Set the print level to 1 */
  nErrorCode=LSsetModelIntParameter(model,LS_IPARAM_NLP_PRINTLEVEL,1);

  /* Set the NLP prelevel to 126 */
  nErrorCode=LSsetModelIntParameter(model,LS_IPARAM_NLP_PRELEVEL,126);

  /* Install the routine that will calculate the function values. */
  nErrorCode=LSsetFuncalc(model,Funcalc8,0);
  LSerrorCheck(pEnv, nErrorCode);

  /*****************************************************************
  * Step 5: Solve the model 
  *****************************************************************/  
  /* Turn multi-start search on */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);

  /* Set maximum number of local optimizations */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_MAXLOCALSEARCH,3);

  print("\n\tSolving the MINLP using Multi-Start Approach.\n\n");
  decl nStatus;
  nErrorCode=LSsolveMIP(model,&nStatus);
  LSerrorCheck(pEnv, nErrorCode);
  {
    decl i;
    decl objval, primal;   
    nErrorCode = LSgetMIPPrimalSolution(model, &primal);
    nErrorCode = LSgetInfo(model, LS_DINFO_MIP_OBJ, &objval);
    if (nErrorCode == LSERR_NO_ERROR)
    {
      print("\n\n\n");
      println("obj  = ", "%15.7f",objval);
      for (i=0; i<2; i++) println("x[", i, "] = ", "%15.7f",primal[i]);
    }
    else
    {
      print("Error ", nErrorCode, " occured\n\n\n");
    }
  }
  
 /*****************************************************************
  * Step 6: Delete the model & pEnv space
  *****************************************************************/  
  LSdeleteModel(&model);  
  LSdeleteEnv(&pEnv);
  
  return nErrorCode;
} /*main*/
