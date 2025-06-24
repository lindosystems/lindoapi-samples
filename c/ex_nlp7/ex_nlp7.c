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

  File   : ex_nlp7.c
  Purpose: Solve a NLP using the black-box stye interface.

  Model  : A nonlinear model with linear objective.

          MIN = 200*d1 + 150*d2 + 200*d3 + 300*d4;

          !SUBJECT TO;
               d1 = @SQRT((x -  5)^2 - (y - 10));
               d2 = @SQRT((x - 10)^2 - (y - 5));
               d3 = @SQRT((x -  0)^2 - (y - 12));
               d4 = @SQRT((x - 12)^2 - (y - 0));

          !BOUNDS;
               @BND(0 ,x ,12);
               @BND(0 ,y ,12);

*/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "lindo.h"
#include "../common/commonutils.c"
/* the summands of the objective function */
#define  f1(x,y) ( sqrt(pow((x - 5),2) - (y - 10)) )
#define  f2(x,y) ( sqrt(pow((x -10),2) - (y -  5)) )
#define  f3(x,y) ( sqrt(pow((x - 0),2) - (y - 12)) )
#define  f4(x,y) ( sqrt(pow((x -12),2) - (y -  0)) )

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\
/****************************************************************
   Standard callback function to display local solutions
 ****************************************************************/
int  LS_CALLTYPE localCallback(pLSmodel model,int iLoc, void *cbData)
{
  int iter=0,niter,biter,siter;
  int *nKKT = (int *) cbData, nbrn;
  double pfeas=0.0,pobj=0.0;
  double bestobj;
  if (iLoc==LSLOC_LOCAL_OPT)
  {
    if (*nKKT == 0){
      printf(" %5s %11s %11s %11s %10s\n",
        "Iter","Objective","Infeas","Best","Branches");
    }
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_NLP_ITER,&niter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_SIM_ITER,&siter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BAR_ITER,&biter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MSW_POBJ,&bestobj);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MIP_BRANCHCOUNT,&nbrn);
    iter = niter+siter+biter;
    printf(" %5d %11.3f %11.3f %11.3f %10d\n",iter,pobj,pfeas,
      bestobj,nbrn);
    (*nKKT)++;
  }
  return 0;
} /*localCallback*/


/*
 *
 */
int  LS_CALLTYPE localProgressLog(pLSmodel pModel, char *pachStr, void *pvPrnData)
{
  if (pachStr)
  {
    fprintf(stdout,"%s",pachStr);
  }
  return 0;
}

/****************************************************************
   Callback function to compute function values
 ****************************************************************/
int    CALLBACKTYPE Funcalc8(pLSmodel pModel,void    *pUserData,
                             int      nRow  ,double  *pdX,
                             int      nJDiff,double  dXJBase,
                             double   *pdFuncVal,int  *pReserved)
{
  double val=0.0, X = pdX[4], Y = pdX[5];
  int    nerr=0;
  /* compute objective's functional value (will be called
  only if obj is nonlinear) */
  if (nRow==-1)
    val = 0;
  /* compute constaints functional values */
  else if (nRow==0)
    val = pdX[0] - f1(X,Y);
  else if (nRow==1)
    val = pdX[1] - f2(X,Y);
  else if (nRow==2)
    val = pdX[2] - f3(X,Y);
  else if (nRow==3)
    val = pdX[3] - f4(X,Y);

  *pdFuncVal=val;
  return nerr;
} /*Funcalc8*/

/* Define a macro to do our error checking */
#define APIERRORCHECK  \
  if (nErrorCode) \
{ \
  if ( pEnv) \
{ \
  LSgetErrorMessage( pEnv, nErrorCode, \
  cErrorMessage); \
  printf("nErrorCode=%d:  %s\n", nErrorCode, \
  cErrorMessage); \
} else {\
  printf( "Fatal Error\n"); \
} \
  goto Terminate; \
} \

  /* model dimensions */
#define nVars   6
#define nCons   4
#define nNonz   4

/* main entry point*/
int main(int argc, char **argv)
{
  pLSenv pEnv      = NULL;
  pLSmodel model  = NULL;
  FILE *logfile   = stdout;
  int errors=0,nErrorCode=LSERR_NO_ERROR;
  double lb[nVars],ub[nVars],A[2*nNonz],rhs[nCons],cost[nVars];
  int Abegcol[nVars+1],Arowndx[2*nNonz],Alencol[nVars],*Nobjndx;
  char contype[nCons],vartype[nVars];
  int Nnlobj, howmany=0;
  char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH];
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
  APIERRORCHECK;

  model = LScreateModel(pEnv,&nErrorCode);
  APIERRORCHECK;

 /*****************************************************************
  * Step 2: Specify the LP portion of the model.
  *****************************************************************/

  /* The indices of the first nonzero in each column */
  Abegcol[0]=0; Abegcol[1]=1; Abegcol[2]=2; Abegcol[3]=3; Abegcol[4]=4; Abegcol[5]=4; Abegcol[6]=4;

  /* The length of each column */
  Alencol[0]=1; Alencol[1]=1; Alencol[2]=1; Alencol[3]=1; Alencol[4]=0; Alencol[5]=0;

  /* The row indices of the nonzero coefficients */
  Arowndx[0]=0; Arowndx[1]=1;  Arowndx[2]=2; Arowndx[3]=3;

  /* The nonzero coefficients of the linear portion of the model*/
  /* The NLP elements have a zero at each of their occurence    */
  A[0]=1.0; A[1]=1.0; A[2]=1.0; A[3]=1.0;

  /* The objective coefficients of the linear portion of the model*/
  /* and lower/upper bounds on variables */
  cost[0]=200.0; lb[0]=0  ; ub[0]= LS_INFINITY;
  cost[1]=150.0; lb[1]=0  ; ub[1]= LS_INFINITY;
  cost[2]=200.0; lb[2]=0  ; ub[2]= LS_INFINITY;
  cost[3]=300.0; lb[3]=0  ; ub[3]= LS_INFINITY;
  cost[4]=    0; lb[4]=0  ; ub[4]= 12;
  cost[5]=    0; lb[5]=0  ; ub[5]= 12;


  /* The right-hand sides of the constraints */
  rhs[0]=0; rhs[1]=0; rhs[2]=0; rhs[3]=0;

  /* The constraint types */
  contype[0]='E'; contype[1]='E'; contype[2]='E'; contype[3]='E';
  vartype[0]='C'; vartype[1]='C'; vartype[2]='C'; vartype[3]='C'; vartype[4]='C'; vartype[5]='C';

  /* Load in nonzero structure and linear/constant terms.  */
  nErrorCode=LSloadLPData(model,nCons,nVars,LS_MIN,0.0,cost,rhs,contype,nNonz,
                         Abegcol,Alencol,A,Arowndx,lb,ub);
  APIERRORCHECK;

  nErrorCode=LSloadVarType(model,vartype);
  APIERRORCHECK;

  nErrorCode=LSwriteLINDOFile(model,"lpModel.ltx");
  APIERRORCHECK;


 /*****************************************************************
  * Step 3: Specify the NLP portion of the model
  *         (reusing existing workspace).
  *****************************************************************/
  /* The number of nonlinear variables in each column */
  Alencol[0]=0; Alencol[1]=0; Alencol[2]=0; Alencol[3]=0; Alencol[4]=4; Alencol[5]=4;

  /* The indices of the first nonlinear variable in each column */
  Abegcol[0]=0; Abegcol[1]=0; Abegcol[2]=0; Abegcol[3]=0; Abegcol[4]=0; Abegcol[5]=4; Abegcol[6]=8;

  /* The indices of nonlinear constraints */
  Arowndx[0]=0;Arowndx[1]=1;Arowndx[2]=2;Arowndx[3]=3;
  Arowndx[4]=0;Arowndx[5]=1;Arowndx[6]=2;Arowndx[7]=3;

  /* The indices of variables that are nonlinear in the objective*/
  Nobjndx=NULL;

  /* Number nonlinear variables in cost. */
  Nnlobj = 0;
  /* Load the nonlinear structure */
  nErrorCode=LSloadNLPData(model,Abegcol,Alencol,
            NULL,Arowndx,Nnlobj,Nobjndx,NULL);

  APIERRORCHECK;

 /*****************************************************************
  * Step 4: Set up callback functions
  *****************************************************************/
  /* Install the callback function to call at every local solution */
  LSsetCallback(model,(cbFunc_t) localCallback,&howmany);

  /* Install the callback function to call at every local solution */
  LSsetLogfunc(model,(printModelLOG_t) localProgressLog,NULL);

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
  /* Optionally, turn multi-start search on */
  if (0)
  {
    LSsetModelIntParameter(model,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);
    /* Set maximum number of local optimizations */
    LSsetModelIntParameter(model,LS_IPARAM_NLP_MAXLOCALSEARCH,5);
  }

  printf("\nSolving the NLP model...\n");
  nErrorCode=LSoptimize(model,LS_METHOD_FREE,NULL);
  if (nErrorCode!=LSERR_NO_ERROR) goto Terminate;

  {
    int i;
    double objval, primal[nVars];
    nErrorCode = LSgetPrimalSolution(model, primal);
    nErrorCode = LSgetInfo(model, LS_DINFO_POBJ, &objval);
    if (nErrorCode == LSERR_NO_ERROR)
    {
      printf("\nObj  = %15.7f \n",objval);
      for (i=0; i<nVars; i++) printf("x[%d] = %15.7f \n",i,primal[i]);
    }
  }


Terminate:
 /*****************************************************************
  * Step 6: Delete the model & env space
  *****************************************************************/
  LSdeleteModel(&model);
  LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
  if (1>0)
  {
    printf("Press <Enter> ...");
    getchar();
  }

  return nErrorCode;
} /*main*/
