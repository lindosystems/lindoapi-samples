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

  File   : ex_nlp5.c
  Purpose: Solve a multi-extremal continous model using GOP solver.

           MINIMIZE      x * sin(x * pi)
           subject to           0 <= x <= 10
*/

#include <stdio.h>
#include <stdlib.h>
/* LINDO API header file */
#include "lindo.h"

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
   nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
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
      int objsense[10];
      char ctype[10], vtype[10];
      int code[200];
      double numval[10],varval[10];
      int objs_beg[10], objs_length[10], cons_beg[10], cons_length[10];
      double lwrbnd[10], uprbnd[10];
      int nLinearz, nAutoDeriv, nConvexRelax, nCRAlgReform;
      int ikod, iobj, icon;

      /* Number of constraints */
      ncons = 1;
      /* Number of objectives */
      nobjs = 1;
      /* Number of variables */
      nvars = 2;
      /* Number of real number constants */
      nnums = 0;

      /* Lower bounds of variables */
      lwrbnd[0]=0.0;
      lwrbnd[1]=-2.0;

      /* Upper bounds of variables */
      uprbnd[0]=10.0;
      uprbnd[1]=2.0;

      /* Starting point of variables */
      varval[0]=9.510649470091590;
      varval[1]=0.0;

      /* Variable type, C= continuous, B = binary */
      vtype[0] = 'C';
      vtype[1] = 'C';

      /* Count for instruction code */
    ikod = 0;
      /* Count for objective row */
    iobj = 0;
      /* Count for constraint row */
    icon = 0;

      /*
       *  Instruction code of the objective:
       *
       *  min  x0 * sin(x0 * pi)
       */

      /* Direction of optimization */
      objsense[iobj]= LS_MIN;
      /* Beginning position of objective */
      objs_beg[iobj]=ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PI;
      code[ikod++]=  EP_MULTIPLY;
      code[ikod++]=  EP_SIN;
      code[ikod++]=  EP_MULTIPLY;

    /* Length of objective */
      objs_length[iobj] = ikod - objs_beg[iobj];
      /* Increment the objective count */
    iobj++;


      /*
       *  Instruction code of constraint 0:
       *
       *   x1   = 0 ;  (a dummy constraint)
       */

    /* Constraint type */
      ctype[icon]= 'E';   /* less or than or equal to */
      /* Beginning position of constraint 0 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;

    /* Length of constraint 0 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
    icon++;

      /* Total number of items in the instruction list */
      lsize = ikod;

      /* Set linearization level, before a call to LSloadNLPCode.
       * If not specified, the solver will decide */
      nLinearz = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_LINEARZ, nLinearz);
      APIERRORCHECK;

    /* Select algebraic reformulation level in convex relaxation*/
      nCRAlgReform = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_CR_ALG_REFORM, nCRAlgReform);
      APIERRORCHECK;


    /* Select convex relax level */
      nConvexRelax = 0;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_CONVEXRELAX, nConvexRelax);
      APIERRORCHECK;

      /* Set up automatic differentiation, before a call to
       * LSloadNLPCode. If not specified, the numerical derivative will
       * be applied */
      nAutoDeriv = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_AUTODERIV, nAutoDeriv);
      APIERRORCHECK;

      /* Pass the instruction list to problem structure
       * by a call to LSloadNLPCode() */
      nErrorCode = LSloadInstruct (pModel, ncons, nobjs, nvars, nnums,
                    objsense, ctype,  vtype, code, lsize, NULL,
                    numval, varval, objs_beg, objs_length, cons_beg,
                    cons_length, lwrbnd, uprbnd);
      APIERRORCHECK;
   }

/*
 * >>> Step 5 <<< Perform the optimization using the MIP solver
 */
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
   APIERRORCHECK;
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_GOP_PRINTLEVEL,1);
   APIERRORCHECK;

   nErrorCode = LSsolveGOP(pModel, NULL);
   APIERRORCHECK;

   {
      int nLinearity, stat, i, nvars;
      double objval=0.0, primal[100];

      /* Get the linearity of the solved model */
      nErrorCode = LSgetInfo (pModel, LS_IINFO_NLP_LINEARITY, &nLinearity);
      APIERRORCHECK;

      nErrorCode = LSgetInfo(pModel,LS_IINFO_MODEL_STATUS,&stat);
      APIERRORCHECK;
      printf("\nSolution status = %d \n",stat);

      /* Report the status of solution */

      if (nLinearity)
    {
       printf("\nModel has been completely linearized.\n");
    }
      else
    {
       printf("\nModel is nonlinear.\n");
    }

      /* Get the optimization result */
      nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&nvars);
      APIERRORCHECK;
      nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&objval);
      APIERRORCHECK;
      nErrorCode = LSgetPrimalSolution(pModel,primal);
      APIERRORCHECK;

    if (stat==1 || stat==2 || stat==5)
    {
         printf("\nPrinting the solution ... \n");
         printf("obj = %f \n",objval);
     for (i=0;i<nvars;i++)
            printf("x[%d] = %f \n",i,primal[i]);
    }else if (stat == 3)
         printf("\n\nNo feasible solution. \n");

      /* Get the linearity of the solved model */
      nErrorCode = LSgetInfo (pModel, LS_IINFO_NLP_LINEARITY, &nLinearity);
      APIERRORCHECK;

   }
Terminate:
 /* >>> Step 7 <<< Delete the LINDO environment */
   LSdeleteEnv(&pEnv);

  /* Wait until user presses the Enter key */
   printf("\nPress <Enter> ...");
   getchar();

}

