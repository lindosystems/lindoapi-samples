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

  File   : ex_nlp2.c
  Purpose: Solve a NLP using the instruction-list style interface.
  Model  : A nonlinear model with multiple local optima.

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
#include "../common/commonutils.c"
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
   nErrorCode = LSloadDefaultLicenseString(MY_LICENSE_KEY);
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
/*
 * >>>> Step 3 <<< Set up the instruction list of the model.
 */
      int nobjs, ncons, nvars, nnums, lsize;
      int objsense[1];
      char ctype[6], vtype[2];
      int code[100], varndx[2];
      double numval[8],varval[2];
      int objs_beg[1], objs_length[1], cons_beg[6], cons_length[6];
      double lwrbnd[2], uprbnd[2];
      int nLinearz, nAutoDeriv;
      int ikod, iobj, icon;
      /* Number of constraints */
      ncons = 6;
      /* Number of objectives */
      nobjs = 1;
      /* Number of variables */
      nvars = 2;
      /* Number of real number constants */
      nnums = 5;
      /* Variable index */
      varndx[0]=1;
      varndx[1]=2;
      /* Lower bounds of variables */
      lwrbnd[0]=-100.0;
      lwrbnd[1]=0.0;
      /* Upper bounds of variables */
      uprbnd[0]=100.0;
      uprbnd[1]=1.0;
      /* Starting point of variables */
      varval[0]=4.0;
      varval[1]=0.0;
      /* Variable type, C= continuous, B = binary */
      vtype[0] = 'C';
      vtype[1] = 'B';
  /* Double Precision constants in the model */
      numval[0]=1.0;
      numval[1]=0.4;
      numval[2]=6.0;
      numval[3]=4.0;
      numval[4]=2.0;
      /* Count for instruction code */
    ikod = 0;
      /* Count for objective row */
    iobj = 0;
      /* Count for constraint row */
    icon = 0;
      /*
       *  Instruction code of the objective:
       *
       *  max abs( x0 + 1) + .4 * x1;
       */
      /* Direction of optimization */
      objsense[iobj]= LS_MAX;
      /* Beginning position of objective */
      objs_beg[iobj]=ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    0;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_ABS;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    1;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]= EP_MULTIPLY;
      code[ikod++]= EP_PLUS;
  /* Length of objective */
      objs_length[iobj] = ikod - objs_beg[iobj];

      /*
       *  Instruction code of constraint 0:
       *
       *  x0  + x1 - 4 <= 0;
       */
  /* Constraint type */
      ctype[icon]= 'L';   /* less or than or equal to */
      /* Beginning position of constraint 0 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    3;
      code[ikod++]=  EP_MINUS;
  /* Length of constraint 0 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
  icon++;
      /*
       *  Instruction code of constraint 1:
       *
       *  x0 * x1      + x1 - 6 <= 0;
       */
  /* Constraint type */
      ctype[icon]= 'L';   /* less than or equal to */
      /* Beginning position of constraint 1 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_MULTIPLY;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    2;
      code[ikod++]=  EP_MINUS;
      /* Length of constraint 1 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
  icon++;
      /*
       *  Instruction code of constraint 2:
       *
       *  x0 * x1           <= 0;
       */
  /* Constraint type */
      ctype[icon]= 'L';   /* less than or equal to */
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
  /* Constraint type */
      ctype[icon]= 'G';   /* greater than or equal to */
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
  /* Constraint type */
      ctype[icon]= 'L';  /* less than or equal to */
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
  /* Constraint type */
      ctype[icon]= 'L';  /* less than or equal to */
      /* Beginning position of constraint 5 */
      cons_beg[icon]=  ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    4;
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
      nErrorCode = LSloadInstruct (pModel, ncons, nobjs, nvars, nnums,
                    objsense, ctype,  vtype, code, lsize, varndx,
                    numval, varval, objs_beg, objs_length, cons_beg,
                    cons_length, lwrbnd, uprbnd);
      APIERRORCHECK;
   }
/*
 * >>> Step 5 <<< Perform the optimization using the MIP solver
 */
   nErrorCode = LSsolveMIP(pModel, NULL);
   APIERRORCHECK;
   {
      int nLinearity;
      double objval=0.0, primal[100];
      /* Get the optimization result */
      LSgetInfo(pModel, LS_DINFO_MIP_OBJ, &objval);
      APIERRORCHECK;
      LSgetMIPPrimalSolution( pModel, primal) ;
      APIERRORCHECK;
      printf("\n\nObjective = %f \n",objval);
      printf("x[0] = %f \n",primal[0]);
      printf("x[1] = %f \n",primal[1]);
      /* Get the linearity of the solved model */
      nErrorCode = LSgetInfo (pModel, LS_IINFO_NLP_LINEARITY, &nLinearity);
      APIERRORCHECK;
      /* Report the status of solution */
      if (nLinearity)
      printf("\nModel has been completely linearized.\
              \nSolution Status: Globally Optimal\n");
      else
      printf("\nModel is nonlinear.\
              \nSolution Status: Locally Optimal\n\n");
   }
Terminate:
 /* >>> Step 7 <<< Delete the LINDO environment */
   LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();


}
