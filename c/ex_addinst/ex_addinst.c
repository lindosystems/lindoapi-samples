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

  File   : ex_addinst.c
  Purpose: Solve a NLP by first loading an LP matrix by coefficient
           and then adding nonlinear terms/constraints by the
       instruction-list style interface.
  Model  : A nonlinear model with multiple local minimizers.

            maximize  abs( x0 + 1) + .4 * x1;
            s.t.     x0           + x1 - 4      <= 0;
                     x0 * x1      + x1 - 6      <= 0;
                     x0 * x1                    <= 0;
                     max(x0 , x1 + 1)           >= 0;
                     if(x1, 1, x1)              <= 0;
                     (x1 * 2 * x1  -  x1) * x0  <= 0;
                     -100  <=  x0  <=  100
                     x1 is binary
*/

#include <stdio.h>
#include <stdlib.h>
/* LINDO API header file */
#include "lindo.h"

 int LS_CALLTYPE LSwriteMPIFile(pLSmodel pModel,
                             char     *pszFname);


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

/* main entry point */
int main()
{
   APIERRORSETUP;
   pLSenv pEnv;
   pLSmodel pModel;
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
   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE)
   {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;
/* >>> Step 2 <<< Create a model in the environment. */
   pModel = LScreateModel(pEnv,&nErrorCode);
   APIERRORCHECK;


   {

/* >>> Step 3a <<< Load the linear part of model.

 To specify our model, we make a call to LSloadLPData,
  passing it:

 - A pointer to the model which we are specifying(pModel)
 - The number of constraints in the model
 - The number of variables in the model
 - The direction of the optimization (i.e. minimize or
 -  maximize)
 - The value of the constant term in the objective (may
    be zero)
 - The coefficients of the objective function
 - The right-hand sides of the constraints
 - The types of the constraints
 - The number of nonzeros in the constraint matrix
 - The indices of the first nonzero in each column
 - The length of each column
 - The nonzero coefficients
 - The row indices of the nonzero coefficients
 - Simple upper and lower bounds on the variables
*/
/* Number of constraints */
      int nM = 6;

/* Number of variables */
      int nN = 2;

/* The direction of optimization */
      int nDir = LS_MAX;

/* The objective's constant term */
      double dObjConst = 0.;

/* The coefficients of the objective function */
      double adC[2] = { 0., 0.4};

/* The right-hand sides of the constraints */
      double adB[6] = { 4., 6., 0., 0., 0., 0.};

/* The constraint types */
      char acConTypes[6] = {'L', 'L', 'L', 'G', 'L', 'L'};

/* The number of nonzeros in the constraint matrix */
      int nNZ = 3;

/* The indices of the first nonzero in each column */
      int anBegCol[3] = { 0, 1, nNZ};

/* The length of each column.  Since we aren't leaving
    any blanks in our matrix, we can set this to NULL */
      int *pnLenCol = NULL;

/* The nonzero coefficients */
      double adA[3] = { 1., 1., 1.};

/* The row indices of the nonzero coefficients */
      int anRowX[4] = { 0, 0, 1};

/* Upper and lower bounds on the variables. */
      double pdLower[2] = {-100., 0.};
    double pdUpper[2] = {100., 1};

/* The variable types*/
    char pcVtype[2] = {'C','B'};

/* We have now assembled a full description of the model.
    We pass this information to LSloadLPData with the
    following call. */
      nErrorCode = LSloadLPData( pModel, nM, nN, nDir,
       dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
       pnLenCol, adA, anRowX, pdLower, pdUpper);
      APIERRORCHECK;

/* We load the variable type information*/
      nErrorCode = LSloadVarType( pModel, pcVtype);
      APIERRORCHECK;

   }


   {
/*
 * >>>> Step 3b <<< Load the nonlinear part of model by the instruction list.
 */
      int nobjs, ncons, nvars, nnums, lsize;
      int *objsense;
      char *ctype, *vtype;
      int code[100], conndx[5];
      double numval[2],*varval;
      int objs_beg[1], objs_length[1], cons_beg[5], cons_length[5];
      double *lwrbnd, *uprbnd;
      int nLinearz, nAutoDeriv;
      int ikod, iobj, icon;
      /* Number of constraints with nonlinear terms*/
      ncons = 5;
      /* Number of objectives with nonlinear terms*/
      nobjs = 1;
      /* Number of newly added variables */
      nvars = 0;
      /* Number of real number constants */
      nnums = 2;

      /* Lower bounds of variables. Since we aren't adding
      any new variables to our model, we can set this to NULL */
      lwrbnd=NULL;
      /* Upper bounds of variables. Since we aren't adding
      any new variables to our model, we can set this to NULL */
      uprbnd=NULL;
      /* Starting point of variables. Since we aren't adding
      any new variables to our model, we can set this to NULL */
      varval=NULL;
      /* Variable type. Since we aren't adding
      any new variables to our model, we can set this to NULL */
      vtype=NULL;
      /* Direction of optimization. Since we aren't changing
      the objective sense in our model, we can set this to NULL */
      objsense= NULL;
    /* Constraint type Since we aren't changing or adding
      the constraint types in our model, we can set this to NULL */
      ctype=NULL;

  /* Double Precision constants in the model */
      numval[0]=1.0;
      numval[1]=2.0;
      /* Count for instruction code */
    ikod = 0;
      /* Count for objective row */
    iobj = 0;
      /* Count for constraint row */
    icon = 0;
      /*
       *  Instruction code of nonlinear terms in objective:
       *
       *  max abs( x0 + 1) + .4 * x1;
       */

      /* Beginning position of objective */
      objs_beg[iobj]=ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    0;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_ABS;
  /* Length of objective */
      objs_length[iobj] = ikod - objs_beg[iobj];

      /*
       *  Instruction code of constraint 1:
       *
       *  x0 * x1      + x1 - 6 <= 0;
       */
      /* Constraint index with nonlinear terms*/
      conndx[icon]=1;
      /* Beginning position of constraint 1 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_MULTIPLY;
      /* Length of constraint 1 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
  icon++;
      /*
       *  Instruction code of constraint 2:
       *
       *  x0 * x1           <= 0;
       */
      /* Constraint index with nonlinear terms*/
      conndx[icon]=2;
      /* Beginning position of constraint 2 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_MULTIPLY;
  /* Length of constraint 2 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
      icon++;
      /*
       *  Instruction code of constraint 3:
     *
       *  max(x0 , x1 + 1)        >= 0;
       */
      /* Constraint index with nonlinear terms*/
      conndx[icon]=3;
      /* Beginning position of constraint 3 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    0;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_MAX;
      code[ikod++]=    2;
  /* Length of constraint 3 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
  icon++;
      /*
       *  Instruction code of constraint 4:
       *
       *  if(x1, 1, x1)        <= 0;
       */
      /* Constraint index with nonlinear terms*/
      conndx[icon]=4;
      /* Beginning position of constraint 4 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_IF;
  /* Length of constraint 4 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
  icon++;
      /*
       *  Instruction code of constraint 5:
       *
       *  (x1 * 2 * x1  -  x1) * x0      <= 0;
       */
      /* Constraint index with nonlinear terms*/
      conndx[icon]=5;
      /* Beginning position of constraint 5 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    1;
      code[ikod++]=  EP_MULTIPLY;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_MULTIPLY;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_MINUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_MULTIPLY;
  /* Length of constraint 5 */
      cons_length[icon] = ikod - cons_beg[icon];

      /* Total number of items in the instruction list */
      lsize = ikod;
      /* Set linearization level, before a call to LSloadInstruct.
       * If not specified, the solver will decide */
      nLinearz = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_LINEARZ, nLinearz);
      APIERRORCHECK;

      /* Set up automatic differentiation, before a call to
       * LSloadInstruct. If not specified, the numerical derivative
       * will be applied */
      nAutoDeriv = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_AUTODERIV, nAutoDeriv);
      APIERRORCHECK;
      /* Pass the instruction list to problem structure
       * by a call to LSloadInstruct() */
      nErrorCode = LSaddInstruct (pModel, ncons, nobjs, nvars, nnums,
                    objsense, ctype,  vtype, code, lsize, conndx,
                    numval, varval, objs_beg, objs_length, cons_beg,
                    cons_length, lwrbnd, uprbnd);
      APIERRORCHECK;
   }
/*
 * >>> Step 5 <<< Perform the optimization using the MIP solver
 */
   nErrorCode = LSsolveGOP(pModel, NULL);

   APIERRORCHECK;
   {
      int nStatus;
      double objval=0.0, primal[100];
      /* Get the optimization result */
      nErrorCode = LSgetInfo(pModel, LS_DINFO_GOP_OBJ, &objval);
      APIERRORCHECK;
      LSgetMIPPrimalSolution( pModel, primal) ;
      APIERRORCHECK;
      printf("\n\nObjective = %f \n",objval);
      printf("x[0] = %f \n",primal[0]);
      printf("x[1] = %f \n",primal[1]);
      /* Get the linearity of the solved model */
      nErrorCode = LSgetInfo (pModel, LS_IINFO_GOP_STATUS, &nStatus);
      APIERRORCHECK;
      /* Report the status of solution */
      if (nStatus==LS_STATUS_OPTIMAL || nStatus==LS_STATUS_BASIC_OPTIMAL)
      printf("\nSolution Status: Globally Optimal\n");
      else if (nStatus==LS_STATUS_LOCAL_OPTIMAL)
      printf("\nSolution Status: Locally Optimal\n\n");
      else if (nStatus==LS_STATUS_INFEASIBLE)
      printf("\nSolution Status: Infeasible\n\n");
   }
Terminate:
 /* >>> Step 7 <<< Delete the LINDO environment */
   LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();


}
