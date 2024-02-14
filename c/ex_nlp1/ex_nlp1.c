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

  File   : ex_nlp1.c
  Purpose: Solve a NLP using the black-box style interface.
  Model  : A nonlinear model with multiple local minimizers.

        minimize  f(x,y) =  3*(1-x)^2*exp(-(x^2) - (y+1)^2)
                         - 10*(x/5 - x^3 - y^5)*exp(-(x^2)-y^2)
                         - 1/3*exp(-((x+1)^2) - y^2);
        subject to
                         x^2 + y   <=  6;
                         x   + y^2 <=  6;
                         x, y unconstrained in sign;
*/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "lindo.h"

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
#define  f1(X,Y) ( pow(1-X,2)                        )
#define  f2(X,Y) ( X/5 - pow(X  ,3)  - pow(Y  ,5)    )

/* partial derivatives of the summands */
#define dxg1(X,Y)  ( g1(X,Y)*(-2)*X     )
#define dyg1(X,Y)  ( g1(X,Y)*(-2)*(Y+1) )
#define dxg2(X,Y)  ( g2(X,Y)*(-2)*X     )
#define dyg2(X,Y)  ( g2(X,Y)*(-2)*Y     )
#define dxg3(X,Y)  ( g3(X,Y)*(-2)*(X+1) )
#define dyg3(X,Y)  ( g3(X,Y)*(-2)*Y     )
#define dxf1(X,Y)  ( 2*(1-X)            )
#define dyf1(X,Y)  ( 0                  )
#define dxf2(X,Y)  ( 1/5 - 3*pow(X,2)   )
#define dyf2(X,Y)  ( -5*pow(Y,4)        )

/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
int  LS_CALLTYPE print_log(pLSmodel model,int iLoc, void *cbData)
{
  int iter=0,niter,biter,siter;
  int *nKKT = (int *) cbData, npass;
  double pfeas=0.0,pobj=0.0,dfeas=0.0;
  double bestobj;
  static int ncalls = 0;

  if (iLoc==LSLOC_LOCAL_OPT)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_NLP_ITER,&niter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_SIM_ITER,&siter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_BAR_ITER,&biter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_DINFEAS,&dfeas);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_MSW_PASS,&npass);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_MSW_POBJ,&bestobj);
    iter = niter+siter+biter;
    printf("%5d %5d %16.5e %16.5e %16.5e %16.5e\n",
      npass,iter,pobj,pfeas,dfeas,bestobj);
    (*nKKT)++;
  }

  else if (iLoc == LSLOC_CONOPT)
  {
    if (ncalls == 0)
    {
      printf("%5s %5s %16s %16s %16s %16s\n",
        "PASS","ITER","POBJ","PINFEAS","DINFEAS","BESTOBJ");
    }

    LSgetCallbackInfo(model,iLoc,LS_IINFO_NLP_ITER,&iter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_POBJ,&pobj);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_PINFEAS,&pfeas);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_DINFEAS,&dfeas);
    printf("%5s %5d %16.5e %16.5e %16.5e %16s\n",
      "",iter,pobj,pfeas,dfeas,"");

  }

  ncalls++;

  return 0;
} /*print_log*/

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
    val = X*X + Y - 6.0;
  /* compute constaint 1's functional value */
  else if (nRow==1)
    val = X + Y*Y - 6.0;
  *pdFuncVal=val;
  return nerr;
} /*Funcalc8*/

/****************************************************************
  Callback function to compute derivatives
 ****************************************************************/
int CALLBACKTYPE Gradcalc8(pLSmodel pModel, void *pUserData,
                           int nRow,double *pdX, double *lb,
                           double *ub, int nNewPnt, int nNPar,
                           int *parlist, double *partial)
{
  int i2,nerr=0;
  double X=pdX[0], Y=pdX[1];

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
  return nerr;
}

#define APIVERSION \
{\
    char szVersion[255], szBuild[255];\
    LSgetVersionInfo(szVersion,szBuild);\
    printf("\nLINDO API Version %s built on %s\n",szVersion,szBuild);\
}\

/* main entry point*/
int main(int argc, char **argv)
{
  pLSenv pEnv      = NULL;
  pLSmodel model  = NULL;
  FILE *logfile   = stdout;
  int errors=0,nErrorCode=LSERR_NO_ERROR, status;
  double lb[2],ub[2],A[4],rhs[2],cost[2], primal[2],objval;
  int Abegcol[3],Arowndx[4],Alencol[2],Nobjndx[2];
  int m,n,nz,  Nnlobj, counter = 0;
  char contype[2];
  char MY_LICENSE_KEY[1024];
  int i;
  double dinf, pinf, dobjval;
  char cErrorMessage[LS_MAX_ERROR_MESSAGE_LENGTH];

 /*****************************************************************
  * Step 1: Create a model in the environment.
  *****************************************************************/
  nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
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
  m = n = 2;  nz = 4;

  /* The indices of the first nonzero in each column */
  Abegcol[0]=0; Abegcol[1]=2; Abegcol[2]=4;

  /* The length of each column */
  Alencol[0]=2; Alencol[1]=2;

  /* The row indices of the nonzero coefficients */
  Arowndx[0]=0; Arowndx[1]=1;  Arowndx[2]=0;  Arowndx[3]=1;

  /* The nonzero coefficients of the linear portion of the model*/
  /* The NLP elements have a zero at each of their occurrence    */
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
  APIERRORCHECK;
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
            NULL,Arowndx,Nnlobj,Nobjndx,0);
  printf("\nThe model is installed successfully...\n");
 /*****************************************************************
  * Step 4: Set up callback functions
  *****************************************************************/
  /* Install the routine that will calculate the function values. */
  nErrorCode=LSsetFuncalc(model,(Funcalc_type) Funcalc8,NULL);
  APIERRORCHECK;

  /* Optionally, install the routine that will calculate the gradient.
  If not provided, LINDO API will calculate gradients itself.*/
  if (0) {
  //LSsetModelDouParameter(model,LS_DPARAM_NLP_PSTEP_FINITEDIFF,1e-5);
  //LSsetModelIntParameter(model,LS_IPARAM_NLP_USE_LINDO_CRASH,1);
  nErrorCode=LSsetGradcalc(model,Gradcalc8,NULL,0,NULL);
  }
  APIERRORCHECK;

  /* Install a callback function */
  LSsetCallback(model,(cbFunc_t) print_log, &counter);

    /* Set the print level to 1 */
  nErrorCode=LSsetModelIntParameter(model,LS_IPARAM_NLP_PRINTLEVEL,1);

  /* Turn multi-start search on */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_SOLVER,LS_NMETHOD_MSW_GRG);

  /* Set maximum number of local optimizations */
  LSsetModelIntParameter(model,LS_IPARAM_NLP_MAXLOCALSEARCH,2);

 /*****************************************************************
  * Step 5: Solve the model
  *****************************************************************/
  /* load an initial starting point */
  primal[0] = 0.25;  primal[1] = -1.65;
  nErrorCode=LSloadVarStartPoint(model,primal);


  if (1>0) {
    /* set up as MINLP and optimize */
    nErrorCode=LSloadVarType(model,"CI");
    APIERRORCHECK;
    nErrorCode=LSsolveMIP(model, &status);
    APIERRORCHECK;
    nErrorCode = LSgetInfo(model, LS_DINFO_MIP_OBJ, &objval);
    nErrorCode = LSgetInfo(model, LS_DINFO_MIP_BESTBOUND, &dobjval);
    nErrorCode = LSgetInfo(model, LS_DINFO_MIP_PFEAS, &pinf);
    nErrorCode = LSgetInfo(model, LS_DINFO_MIP_INTPFEAS, &dinf);

    nErrorCode = LSgetMIPPrimalSolution(model, primal);
    APIERRORCHECK;
    nErrorCode = LSgetMIPSlacks(model, rhs);
    APIERRORCHECK;
  } else {
    /* set up as NLP and optimize */
    nErrorCode=LSoptimize(model,LS_METHOD_FREE, &status);
    if (nErrorCode!=LSERR_NO_ERROR)
      goto Terminate;
    nErrorCode = LSgetInfo(model, LS_DINFO_POBJ, &objval);
    nErrorCode = LSgetInfo(model, LS_DINFO_DOBJ, &dobjval);
    nErrorCode = LSgetInfo(model, LS_DINFO_PINFEAS, &pinf);
    nErrorCode = LSgetInfo(model, LS_DINFO_DINFEAS, &dinf);

    nErrorCode = LSgetPrimalSolution(model, primal);
    nErrorCode = LSgetSlacks(model, rhs);
  }
  printf("\n\n\nPrinting the best local optimum found.\n");
  printf("obj  = %f \n",objval);
  printf("bound = %f \n",dobjval);
  printf("pinf = %16e \n",pinf);
  printf("dinf = %16e \n",dinf);
  printf("stat = %d \n",status);
  for (i=0; i<n; i++) printf("x[%d] = %f \n",i,primal[i]);
  for (i=0; i<m; i++) printf("s[%d] = %f \n",i,rhs[i]);

Terminate:
 /*****************************************************************
  * Step 6: Delete the model & env space
  *****************************************************************/
  LSdeleteModel(&model);
  LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

} /*main*/
