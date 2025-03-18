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

  File   : ex_nlp9_uc.c
  Purpose: Solve an unconstrained model over a box, using a dummy
           constraint.

  Model DeJong's fifth function is (Shekel's foxholes):


                 25                 1
                 ___    ---------------------------
    f(x1,x2) =   \              2
                 /__           ___
                 j=1      j +  \   (x[i] - a[i][j])^6
                               /__
                               i=1

   s.t. x1>=-100 (dummy constraint)
   where each x is in the range [-65.536,65.536]

*/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "lindo.h"
#include "../common/commonutils.c"
#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\
/* Define a macro to declare variables for
    error checking */
#define APIERRORSETUP  \
   int nErrorCode; \
   char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH] \

/* Define a macro to do our error checking */
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

static int a[2][25] ={
  {-32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
   -32, -16, 0, 16, 32, -32, -16, 0, 16, 32        },
  {-32, -32, -32, -32, -32, -16, -16, -16, -16, -16,
   16, 16, 16, 16, 16, 32, 32, 32, 32, 32  }
};


// compute f(x)
double fox(double *x) {
  int power, i,j;
  double lowtot,prod,total=0.002;

  for(j=0; j<25; j+=1) {
    lowtot=1.0 + (double)j;
    for(i=0; i<2; i+=1) {
      prod=1.0;
      for(power=0; power<6; power+=1)
        prod*=(x[i]-a[i][j]);
      lowtot+=prod;
    }
    total+=1.0/lowtot;
  }
  return total;
}


/****************************************************************
   Standard callback function to display local solutions
 ****************************************************************/
int  LS_CALLTYPE local_sol_log(pLSmodel pModel,int iLoc, void *cbData)
{
  int iter=0,niter,biter,siter;
  int *nKKT = (int *) cbData, npass=0, nbrn;
  double pfeas=0.0,pobj=0.0;
  double bestobj;
  if (iLoc==LSLOC_LOCAL_OPT)
  {
    if (*nKKT == 0){
      printf(" %5s %11s %11s %11s %10s\n",
        "Iter","Objective","Infeas","Best","Branches");
    }
    LSgetCallbackInfo(pModel,iLoc,LS_IINFO_MIP_NLP_ITER,&niter);
    LSgetCallbackInfo(pModel,iLoc,LS_IINFO_MIP_SIM_ITER,&siter);
    LSgetCallbackInfo(pModel,iLoc,LS_IINFO_MIP_BAR_ITER,&biter);
    LSgetCallbackInfo(pModel,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(pModel,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(pModel,iLoc,LS_DINFO_MSW_POBJ,&bestobj);
    LSgetCallbackInfo(pModel,iLoc,LS_IINFO_MIP_BRANCHCOUNT,&nbrn);
    iter = niter+siter+biter;
    printf(" %5d %11.3f %11.3f %11.3f %10d\n",iter,pobj,pfeas,
      bestobj,nbrn);
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
    val = fox(pdX);
  /* compute constaint 0's functional value */
  else if (nRow==0)
    val = X;

  *pdFuncVal=val;
  return nerr;
} /*Funcalc8*/


/* main entry point*/
int main(int argc, char **argv)
{
  APIERRORSETUP;
  pLSenv pEnv      = NULL;
  pLSmodel pModel  = NULL;
  FILE *logfile   = stdout;
  int errors=0;
  double lb[2],ub[2],A[4],rhs[2],cost[2];
  int Abegcol[3],Arowndx[4],Alencol[2],Nobjndx[2];
  int m,n,nz, Nnlobj, howmany=0;
  char contype[2],vartype[2];
  char MY_LICENSE_KEY[1024];

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
  if (nErrorCode!=LSERR_NO_ERROR) return nErrorCode;

  pModel = LScreateModel(pEnv,&nErrorCode);
  if (nErrorCode!=LSERR_NO_ERROR) return nErrorCode;

 /*****************************************************************
  * Step 2: Specify the LP portion of the model.
  *****************************************************************/
  /* model dimensions */
  m = 1; // number of constraints
  n = 2; // number of variables
  nz = 1;  // number of nonzeroes

  /* The indices of the first nonzero in each column */
  Abegcol[0]=0; Abegcol[1]=1; Abegcol[2]=1;

  /* The length of each column */
  Alencol[0]=1; Alencol[1]=0;

  /* The row indices of the nonzero coefficients */
  Arowndx[0]=0;

  /* The nonzero coefficients of the linear portion of the model*/
  /* The NLP elements have a zero at each of their occurence    */
  A[0]=1.0;

  /* The objective coefficients of the linear portion of the model*/
  cost[0]=0.0; cost[1]=0.0;

  /* lower bounds on variables */
  lb[0]=-65.536  ; ub[0]= +65.536;
  lb[1]=-65.536  ; ub[1]= +65.536;

  /* The right-hand sides of the dummy constraint x1>=0 */
  rhs[0]=0.0;

  /* The constraint types */
  contype[0]='G';

  /* The variable types */
  vartype[0]='C'; vartype[1]='C';

  /* Load in nonzero structure and linear/constant terms.  */
  nErrorCode=LSloadLPData(pModel,m,n,LS_MIN,0.0,cost,rhs,contype,nz,
                         Abegcol,Alencol,A,Arowndx,lb,ub);
  if (nErrorCode!=LSERR_NO_ERROR) return nErrorCode;

  nErrorCode=LSloadVarType(pModel,vartype);
  if (nErrorCode!=LSERR_NO_ERROR) return nErrorCode;

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
  nErrorCode=LSloadNLPData(pModel,Abegcol,Alencol,
            NULL,Arowndx,Nnlobj,Nobjndx,NULL);
  APIERRORCHECK;

 /*****************************************************************
  * Step 4: Set up callback functions
  *****************************************************************/
  /* Install the callback function to call at every local solution */
  nErrorCode=LSsetCallback(pModel,(cbFunc_t) local_sol_log,&howmany);
  APIERRORCHECK;

  /* Set the print level to 1 */
  nErrorCode=LSsetModelIntParameter(pModel,LS_IPARAM_NLP_PRINTLEVEL,1);
  APIERRORCHECK;

  /* Set the NLP prelevel to 0 */
  nErrorCode=LSsetModelIntParameter(pModel,LS_IPARAM_NLP_PRELEVEL,0);
  APIERRORCHECK;

  /* Install the routine that will calculate the function values. */
  nErrorCode=LSsetFuncalc(pModel,(Funcalc_type) Funcalc8,NULL);
  APIERRORCHECK;

  /*****************************************************************
  * Step 5: Solve the model
  *****************************************************************/
  /* Turn multi-start search on */
  nErrorCode=LSsetModelIntParameter(pModel,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);
  APIERRORCHECK;

  /* Set maximum number of local optimizations */
  nErrorCode=LSsetModelIntParameter(pModel,LS_IPARAM_NLP_MAXLOCALSEARCH,5);
  APIERRORCHECK;

  printf("\n\tSolving the DeJong's 5'th model using HS solver.\n\n");
  nErrorCode=LSsolveHS(pModel,LS_METHOD_GA,NULL);
  APIERRORCHECK;

  {
    int i;
    double objval, primal[2];
    nErrorCode = LSgetPrimalSolution(pModel, primal);
    nErrorCode = LSgetInfo(pModel, LS_DINFO_POBJ, &objval);
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
  LSdeleteModel(&pModel);
  LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

  return nErrorCode;
} /*main*/
