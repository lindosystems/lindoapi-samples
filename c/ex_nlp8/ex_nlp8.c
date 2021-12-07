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

  File   : ex_nlp8.c
  Purpose: Solve a NLP using the black-box stye interface.

  Model  : A nonlinear model with linear objective.

          MIN = 20*x*y*z - (x-y)^3;   --> f1(.)
          !SUBJECT TO;
               2*x*z + 3*x^2 <= 35;   --> f2(.)
               x*y*z + 2*y*z >= 10;   --> f3(.)
               x^2 + z^2 <= 23;       --> f4(.)
          !BOUNDS;
	          @BND(0,x,7);
	          @BND(0,y,7);
	          @BND(0,z,7);

*/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "lindo.h"

/* the summands of the objective function */
#define  f1(x,y,z) ( 20*x*y*z - pow((x-y),3)  )
#define  f2(x,y,z) ( 2*x*z + 3*pow(x,2)       )
#define  f3(x,y,z) ( x*y*z + 2*y*z            )
#define  f4(x,y,z) ( pow(x,2) + pow(z,2)      )
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
  double val=0.0, X = pdX[0], Y = pdX[1], Z = pdX[2];
  int    nerr=0;
  /* compute objective's functional value (will be called
  only if obj is nonlinear) */
  if (nRow==-1)
    val = f1(X,Y,Z);
  /* compute constaints' functional values */
  else if (nRow==0)
    val = f2(X,Y,Z) ;
  else if (nRow==1)
    val = f3(X,Y,Z) ;
  else if (nRow==2)
    val = f4(X,Y,Z) ;

  *pdFuncVal=val;
  return nerr;
} /*Funcalc8*/


  /* model dimensions */
#define nVars   3
#define nCons   3
#define nNonz   0

/* main entry point*/
int main(int argc, char **argv)
{
  pLSenv pEnv      = NULL;
  pLSmodel model  = NULL;
  FILE *logfile   = stdout;
  int errors=0,nErrorCode=LSERR_NO_ERROR;
  double lb[nVars],ub[nVars],*A=NULL,rhs[nCons],cost[nVars];
  int Abegcol[nVars+1],Arowndx[nCons*nVars],Alencol[nVars],Nobjndx[nVars];
  char contype[nCons],vartype[nVars];
  int Nnlobj, howmany=0;
  char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH];
  char MY_LICENSE_KEY[1024];

 /*****************************************************************
  * Step 1: Create a model in the environment.
  *****************************************************************/
  nErrorCode = LSloadLicenseString("../../../license/lndapi130.lic",MY_LICENSE_KEY);
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
  Abegcol[0]=0; Abegcol[1]=0; Abegcol[2]=0; Abegcol[3]=0;

  /* The length of each column */
  Alencol[0]=0; Alencol[1]=0; Alencol[2]=0;

  /* The row indices of the nonzero coefficients */
  //Arowndx_LP=NULL;

  /* The nonzero coefficients of the LP portion of the model*/
  A=NULL;

  /* The objective coefficients of the linear portion of the model*/
  /* and lower/upper bounds on variables */
  cost[0]=0.0; lb[0]=0  ; ub[0]= 7;
  cost[1]=0.0; lb[1]=0  ; ub[1]= 7;
  cost[2]=0.0; lb[2]=0  ; ub[2]= 7;


  /* The right-hand sides of the constraints */
  rhs[0]=35.0; rhs[1]=10; rhs[2]=23.0;

  /* The constraint types */
  contype[0]='L'; contype[1]='G'; contype[2]='L';
  vartype[0]='C'; vartype[1]='C'; vartype[2]='C';

  /* Load in nonzero structure and linear/constant terms.  */
  nErrorCode=LSloadLPData(model,nCons,nVars,LS_MIN,0.0,cost,rhs,contype,nNonz,
                         Abegcol,Alencol,A,NULL,lb,ub);
  APIERRORCHECK;

  nErrorCode=LSloadVarType(model,vartype);
  APIERRORCHECK;

  nErrorCode=LSwriteLINDOFile(model,"ex_nlp8.ltx");
  APIERRORCHECK;


 /*****************************************************************
  * Step 3: Specify the NLP portion of the model
  *         (reusing existing workspace).
  *****************************************************************/
  /* The number of nonlinear variables in each column */
  Alencol[0]=3; Alencol[1]=3; Alencol[2]=3;

  /* The indices of the first nonlinear variable in each column */
  Abegcol[0]=0; Abegcol[1]=3; Abegcol[2]=6; Abegcol[3]=9;

  /* The indices of nonlinear constraints */
  Arowndx[0]=0;Arowndx[1]=1;Arowndx[2]=2;
  Arowndx[3]=0;Arowndx[4]=1;Arowndx[5]=2;
  Arowndx[6]=0;Arowndx[7]=1;Arowndx[8]=2;

  /* The indices of variables that are nonlinear in the objective*/
  Nobjndx[0]=0; Nobjndx[1]=1; Nobjndx[2]=2;

  /* Number nonlinear variables in cost. */
  Nnlobj = 3;
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

  if (nErrorCode!=LSERR_NO_ERROR)
  {
    printf("\nError %d occured...",nErrorCode);
  }

 /*****************************************************************
  * Step 6: Delete the model & env space
  *****************************************************************/
  LSdeleteModel(&model);
  LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
  if (1)
  {
    printf("Press <Enter> ...");
    getchar();
  }

  return nErrorCode;
} /*main*/
