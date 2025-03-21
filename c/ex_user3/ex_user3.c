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

  @file   : ex_user3.c

  @purpose: Set up and solve an NLP using the Grey-Box interface
            with arbitrary dimensions.

            minimize G0( -1, x0,x1,... )
                     G1(  0, x0,x1,... )  <= 0
                     G2(  1, x0,x1,... )  <= 0
                     G2(  2, x0,x1,... )  <= 0
                     ..
                     Gm(m-1, x0,x1,... )  <= 0

  The black-box functions are

    Gi(i,x)  a user defined function. i = -1,0,..m-1

  @remark : EP_USER operator is used in the instruction list to
  identify the black-box function and specify the number of
  arguments they take. Each function is defined with the following
  block

      EP_PUSH_NUM  0
      EP_PUSH_VAR  x0
      EP_PUSH_VAR  x1
      EP_PUSH_VAR  x2
      ..
      EP_PUSH_VAR  x(n-1)
      EP_USER      n+1

  @remark : LSsetUsercalc() is used to set the user-defined
  MyUserFunc() function  as the gateway to the black-box functions.

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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

/***************************************************************
 *          Set up an output log function.
 */
static void LS_CALLTYPE print_line(pLSmodel model,
    char *line, void *notting)
{
    if (line)
    {
        printf("%s",line);
    } /*if*/
} /*print_line*/



/***************************************************************
    MODEL:
    TITLE TEST;
    [OBJ] MIN= - 10.5 * X1 - 7.5 * X2 - 3.5 * X3 - 2.5 * X4 - 1.5 * X5 - 10 * X6 - 0.5
                * X1 * X1 - 0.5 * X2 * X2 - 0.5 * X3 * X3 - 0.5 * X4 * X4 - 0.5 * X5 * X5;
    [E2]  6 * X1 +  3 * X2 +  3 * X3 + 2 * X4 + X5      <= 6.5;
    [E3] 10 * X1           + 10 * X3               + X6 <= 20;
    @BND( 0, X1, 1); @BND( 0, X2, 1); @BND( 0, X3, 1);
    @BND( 0, X4, 1); @BND( 0, X5, 1);
    END
 */
int LS_CALLTYPE MyUserFunc( pLSmodel model,
    int      nargs,
    double   *argval,
    void     *UserData,
    double   *FuncVal)
{
    double f=0, X1, X2, X3, X4, X5, X6, *x=NULL;
    int n;

    x = argval+1;
    X1 = x[0];
    X2 = x[1];
    X3 = x[2];
    X4 = x[3];
    X5 = x[4];
    X6 = x[5];

    LSgetInfo(model,LS_IINFO_NUM_VARS,&n);
    if (n!=6) {
      return -1;
    }

    if (argval[0]==-1.)     // objective function [OBJ]
    {
      f = - 10.5 * X1 - 7.5 * X2 - 3.5 * X3 - 2.5 * X4 - 1.5 * X5 - 10 * X6 - 0.5
          * X1 * X1 - 0.5 * X2 * X2 - 0.5 * X3 * X3 - 0.5 * X4 * X4 - 0.5 * X5 * X5;
    }
    else if (argval[0]==0.) //constraint#1 [E2]
    {
      f = 6 * X1 + 3 * X2 + 3 * X3 + 2 * X4 + X5 - 6.5;
    }
    else if (argval[0]==1.) //constraint#2 [E3]
    {
      f = 10 * X1 + 10 * X3 + X6 - 20;
    } else {
      f = 0;  // not relevant
    }

    *FuncVal = f;

    return 0;
} /*print_line*/




/***************************************************************
 *                    Main entry point
 */
int main()

{
    APIERRORSETUP;
    pLSenv pEnv = NULL;
    pLSmodel pModel;
    char MY_LICENSE_KEY[1024];

    int ncons, nvars, objsense;
    char ctype[2], vtype[6];
    double x0[6];
    double lb[6], ub[6];

    /*
    * >>> Step 1 <<< Create a LINDO environment.
    */

    nErrorCode = LSloadDefaultLicenseString(
      MY_LICENSE_KEY);
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

    /*
    * >>> Step 2 <<< Create a model in the environment.
    */
    pModel = LScreateModel(pEnv,&nErrorCode);
    APIERRORCHECK;


    /*
     * >>>> Step 3 <<< Set up a dense NLP.
     */
    {

      /* Objective sense */
      objsense = LS_MIN;

      /* Number of constraints */
      ncons = 2;
      /* Number of variables */
      nvars = 6;

      /* Lower bounds of variables */
      lb[0]=0.0;
      lb[1]=0.0;
      lb[2]=0.0;
      lb[3]=0.0;
      lb[4]=0.0;
      lb[5]=0.0;

      /* Upper bounds of variables */
      ub[0]=1;
      ub[1]=1;
      ub[2]=1;
      ub[3]=1;
      ub[4]=1;
      ub[5]=LS_INFINITY;

      /* Starting point of variables */
      x0[0]=1.2345;
      x0[1]=1.2345;
      x0[2]=1.2345;
      x0[3]=1.2345;
      x0[4]=1.2345;
      x0[5]=1.2345;

      /* Variable type, C= continuous, B = binary */
      vtype[0] = 'C';
      vtype[1] = 'C';
      vtype[2] = 'C';
      vtype[3] = 'C';
      vtype[4] = 'C';
      vtype[5] = 'C';

      /* Constraint type */
      ctype[0]= 'L';   /* constraint #0 sense is L */
      ctype[1]= 'L';   /* constraint #1 sense is L */

      /*  Set a log function to call.  */
      nErrorCode = LSsetLogfunc(pModel,(printLOG_t) print_line,NULL);
      APIERRORCHECK;

      /* Set up MyUserFunc() as the user function */
      nErrorCode = LSsetUsercalc (pModel, (user_callback_t) MyUserFunc, NULL);
      APIERRORCHECK;

      nErrorCode = LSloadNLPDense(pModel,ncons,nvars,objsense,ctype,vtype,x0,lb,ub);
      APIERRORCHECK;
   }



    /*
    * >>> Step 5 <<< Perform the optimization
    */

    if (0>1) {
      /* set multi-start as the current NLP solver if desired */
      nErrorCode = LSsetModelIntParameter (pModel,LS_IPARAM_NLP_SOLVER, LS_NMETHOD_MSW_GRG);
      APIERRORCHECK;
    }

    nErrorCode = LSoptimize(pModel, LS_METHOD_FREE, NULL);
    APIERRORCHECK;


    /*
    * >>> Step 6 <<< Retrieve the solution
    */
    {
        int nLinearity, i, stat;
        double objval=0.0, primal[1000];

        /* Get the linearity of the solved model */
        nErrorCode = LSgetModelIntParameter (pModel, LS_IPARAM_NLP_LINEARITY, &nLinearity);
        APIERRORCHECK;

        nErrorCode = LSgetInfo(pModel,LS_IINFO_MODEL_STATUS,&stat);
        APIERRORCHECK;
        printf("\n\n\nSolution status = %d \n",stat);

        /* Report the status of solution */
        nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_VARS,&nvars);
        APIERRORCHECK;

        nErrorCode = LSgetInfo(pModel, LS_IINFO_NUM_CONS,&ncons);
        APIERRORCHECK;

        if (nLinearity)
        {
            printf("\nModel has been completely linearized.\n");
        }
        else
        {
            printf("\nModel is nonlinear. (nvars=%d, ncons=%d)\n",nvars,ncons);
        }

        nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&objval);
        APIERRORCHECK;

        nErrorCode = LSgetPrimalSolution(pModel,primal);
        APIERRORCHECK;

        if (stat==LS_STATUS_OPTIMAL || stat==LS_STATUS_BASIC_OPTIMAL ||
            stat==LS_STATUS_FEASIBLE || stat==LS_STATUS_LOCAL_OPTIMAL)
        {
            printf("\n\nPrinting the solution ... \n\n");
            printf("f(x) = %20.15f \n",objval);;
            for (i=0;i<nvars;i++)
              printf("  x[%d]  = %20.15f\n",i,primal[i]);
            printf("\n");
        }
        else if (stat == 3)
            printf("\n\nNo feasible solution. \n\n");

        /* Get the linearity of the solved model */
        nErrorCode = LSgetInfo (pModel, LS_IINFO_NLP_LINEARITY, &nLinearity);
        APIERRORCHECK;

    }
Terminate:
    /*
    * >>> Step 7 <<< Delete the LINDO environment
    */
    LSdeleteEnv(&pEnv);

    printf("\n\nPress <Enter> ...");
    getchar();

}


