/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2011
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_nlp3.c
  Purpose: Solve a MINLP using the black-box stye interface.

  Model  : A nonlinear model with linear constraints.

        minimize  f(x,y) =  3*(1-x)^2*exp(-(x^2) - (y+1)^2)
                         - 10*(x/5 - x^3 - y^5).*exp(-x^2-y^2)
                         - 1/3*exp(-(x+1)^2 - y^2);
        subject to
                         x  + y   <=  3;
                            - y   <=  1;
                         x  integer
*/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "lindo.h"
#include "../common/commonutils.c"
#define APIERRORCHECK  \
  if (nErrorCode) \
   { \
   if ( pEnv) \
      { \
      LSgetErrorMessage( pEnv, nErrorCode, \
      cErrorMessage); \
      printf("Errorcode=%d:  %s\n", nErrorCode, \
      cErrorMessage); \
      } else {\
      printf( "Fatal Error\n"); \
      } \
      goto Terminate; \
   } \

/* the summands of the objective function */
#define  g1(X,Y) ( exp( -pow(X  ,2)  - pow(Y+1,2) )  )
#define  g2(X,Y) ( exp( -pow(X  ,2)  - pow(Y  ,2) )  )
#define  g3(X,Y) ( exp( -pow(X+1,2)  - pow(Y  ,2) )  )
#define  f1(X,Y) (        pow(1-X,2)                 )
#define  f2(X,Y) ( X/5 - pow(X  ,3)  - pow(Y  ,5)    )
#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\
/****************************************************************
   Standard callback function to display local solutions
 ****************************************************************/
int  LS_CALLTYPE local_sol_log(pLSmodel model,int iLoc, void *cbData)
{
  int iter=0,niter,biter,siter;
  int *nKKT = (int *) cbData, npass, nbrn;
  double pfeas=0.0,pobj=0.0;
  double bestobj,bestmipobj;
  if (iLoc==LSLOC_MSW)
  {
    if (*nKKT == 0){
      printf(" %5s %11s %11s %11s %11s %10s\n",
        "Iter","Objective","Infeas","Best","BestMIP","Branches");
    }
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_NLP_ITER,&niter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&siter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BAR_ITER,&biter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MSW_POBJ,&bestobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MIP_OBJ,&bestmipobj);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BRANCHCOUNT,&nbrn);
    iter = niter+siter+biter;
    printf(" %5d %11.3g %11.3g %11.3g %11.3g %10d\n",iter,pobj,pfeas,
      bestobj,bestmipobj,nbrn);
    (*nKKT)++;
  }
  return 0;
} /*local_sol_log*/

/****************************************************************
   Callback function to compute function values
 ****************************************************************/
int    CALLBACKTYPE Funcalc8(pLSmodel pModel,void    *pUserData,
                             int      nRow  ,double  *pdX,
                             int      nJDiff,double  dXJBase,
                             double   *pdFuncVal,int  *pReserved)
{
  double val=0.0, X = pdX[0], Y = pdX[1];
  int    nerr=0;
  /* compute objective's functional value*/
  if (nRow==-1)
    val = 3*f1(X,Y)*g1(X,Y) - 10*f2(X,Y)*g2(X,Y) - g3(X,Y)/3;
  /* compute constaint 0's functional value */
  else if (nRow==0)
    val = X + Y - 3.0;
  /* compute constaint 1's functional value */
  else if (nRow==1)
    val = - Y - 1.0;

  *pdFuncVal=val;
  return nerr;
} /*Funcalc8*/


/* main entry point*/
int main(int argc, char **argv)
{
  pLSenv pEnv      = NULL;
  pLSmodel model  = NULL;
  FILE *logfile   = stdout;
  int errors=0,nErrorCode=LSERR_NO_ERROR;
  double lb[2],ub[2],A[4],rhs[2],cost[2];
  int Abegcol[3],Arowndx[4],Alencol[2],Nobjndx[2];
  int m,n,nz, Nnlobj, howmany=0;
  char contype[2],vartype[2];
  char MY_LICENSE_KEY[1024];
  char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH];

 /*****************************************************************
  * Step 1: Create a model in the environment.
  *****************************************************************/
  nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
   if ( nErrorCode != LSERR_NO_ERROR)
   {
      printf( "Failed to load license key (error %d)\n",nErrorCode);
      exit( 1);
   }

  APIVERSION;
  pEnv = LScreateEnv(&nErrorCode,MY_LICENSE_KEY);
  APIERRORCHECK;

  model = LScreateModel(pEnv,&nErrorCode);
  APIERRORCHECK;

 /*****************************************************************
  * Step 2: Specify the LP portion of the model.
  *****************************************************************/
  /* model dimensions */
  m = n = 2;  nz = 3;

  /* The indices of the first nonzero in each column */
  Abegcol[0]=0; Abegcol[1]=1; Abegcol[2]=3;

  /* The length of each column */
  Alencol[0]=1; Alencol[1]=2;

  /* The row indices of the nonzero coefficients */
  Arowndx[0]=0; Arowndx[1]=0;  Arowndx[2]=1;

  /* The nonzero coefficients of the linear portion of the model*/
  /* The NLP elements have a zero at each of their occurence    */
  A[0]=1.0; A[1]=1.0; A[2]=-1.0;

  /* The objective coefficients of the linear portion of the model*/
  cost[0]=0.0; cost[1]=0.0;

  /* lower bounds on variables */
  lb[0]=-3.0  ; ub[0]= 3.0;   lb[1]=-3.0  ; ub[1]= 3.0;

  /* The right-hand sides of the constraints */
  rhs[0]=3.0; rhs[1]=1.0;

  /* The constraint types */
  contype[0]='L'; contype[1]='L';
  vartype[0]='I'; vartype[1]='C';

  /* Load in nonzero structure and linear/constant terms.  */
  nErrorCode=LSloadLPData(model,m,n,LS_MIN,0.0,cost,rhs,contype,nz,
                         Abegcol,Alencol,A,Arowndx,lb,ub);
  APIERRORCHECK;

  nErrorCode=LSloadVarType(model,vartype);
  APIERRORCHECK;

  nErrorCode=LSwriteLINDOFile(model,"lpModel.ltx");
  APIERRORCHECK;
 /*****************************************************************
  * Step 3: Specify the NLP portion of the model.
  *****************************************************************/
  /* The number of nonlinear variables in each column */
  Alencol[0]=0; Alencol[1]=0;

  /* The indices of the first nonlinear variable in each column */
  Abegcol[0]=0; Abegcol[1]=0; Abegcol[2]=0;

  /* The indices of nonlinear constraints */
  Arowndx[0]=0;

  /* The indices of variables that are nonlinear in the objective*/
  Nobjndx[0]=0;  Nobjndx[1]=1;

  /* Number nonlinear variables in cost. */
  Nnlobj = 2;
  /* Load the nonlinear structure */
  nErrorCode=LSloadNLPData(model,Abegcol,Alencol,
            NULL,Arowndx,Nnlobj,Nobjndx,NULL);

  APIERRORCHECK;

 /*****************************************************************
  * Step 4: Set up callback functions
  *****************************************************************/
  /* Install the callback function to call at every local solution */
  LSsetCallback(model,(cbFunc_t) local_sol_log,&howmany);

  /* Set the print level to 1 */
  nErrorCode=LSsetModelIntParameter(model,LS_IPARAM_NLP_PRINTLEVEL,1);

  /* Set the NLP prelevel to 126 */
  nErrorCode=LSsetModelIntParameter(model,LS_IPARAM_NLP_PRELEVEL,0);

  /* Install the routine that will calculate the function values. */
  nErrorCode=LSsetFuncalc(model,(Funcalc_type) Funcalc8,NULL);
  APIERRORCHECK;

  /*****************************************************************
  * Step 5: Solve the model
  *****************************************************************/
  /* Turn multi-start search on */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);

  /* Set maximum number of local optimizations */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_MAXLOCALSEARCH,5);

  printf("\n\tSolving the MINLP using Multi-Start Approach.\n\n");
  nErrorCode=LSsolveMIP(model,NULL);
  APIERRORCHECK;
  {
    int i;
    double objval, primal[2];
    nErrorCode = LSgetMIPPrimalSolution(model, primal);
    nErrorCode = LSgetInfo(model, LS_DINFO_MIP_OBJ, &objval);
    if (nErrorCode == LSERR_NO_ERROR)
    {
      printf("\n\n\n");
      printf("obj  = %15.7f \n",objval);
      for (i=0; i<2; i++) printf("x[%d] = %15.7f \n",i,primal[i]);
    }
    else
    {
      printf("Error %d occured\n\n\n",nErrorCode);
    }
  }
Terminate:
 /*****************************************************************
  * Step 6: Delete the model & env space
  *****************************************************************/
  LSdeleteModel(&model);
  LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

  return nErrorCode;
} /*main*/
