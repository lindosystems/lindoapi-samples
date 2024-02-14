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

  @file   : ex_user.c

  @purpose: Solve an NLP that uses two black-box functions within
  the instruction-list interface.

            minimize f(x) * x
                G(x) <= 100
             0 <= x  <= 10

  The black-box functions are

    f(x)   the expression sin(pi*x)+cos(pi*x)
    G(x)   the integral[g(x),a,b)], where a,b constants specifying
           the limits of the integral.

  @remark : This application uses the Instruction Style Interface,
  where the instructions are imported from ex_user.mpi file.

  @remark : EP_USER operator is used in the instruction list to
  identify each black-box function and specify the number of
  arguments they take. For each function, the first argument
  is reserved to identify the function, whereas the rest are the
  actual arguments for the associated function.

  @remark : LSsetUsercalc() is used to set the user-defined
  MyUserFunc() function  as the gateway to the black-box functions.

*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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
 *         Function g(t) to integrate over [a,b] */
double gox(double x, double t)
{
    double function;
    function = exp(x*cos(t));
    return(function);
}

/***************************************************************
 *          Black-box #2 -- G(x)
 *          Calculated by Simpson's Rule.
 */
double Gox(int n          /* Maximum number of steps (even) n */,
           double x)
{
    int c,k=1;            /* Counters in the algorithm        */
    double a=0;           /* Lower limit x=0                  */
    double b=8*atan(1.0);   /* Upper limit x=2*pi               */
    double h,dsum;

    dsum=gox(x,a);        /* Initial function value */
    c=2;
    h=(b-a)/n;            /* Step size h=(b-a)/n */
    while (k <= n-1)      /* Steps through the iteration */
    {
        c=6-c;            /* gives the 4,2,4,2,... */
        dsum = dsum +
          c*gox(x,a+k*h); /* Adds on the next area */
        k++;              /* Increases k value by +1 */
    }
    return ((dsum + gox(x,b))*h/3);
}

/***************************************************************
 *          Black-box function #1 -- f(x).
 */
double fox(double a, double b)
{
  return sin(a) + cos(b);
}

/***************************************************************
 *          Grey-box interface
 */
int LS_CALLTYPE MyUserFunc( pLSmodel model,
    int      nargs,
    double   *argval,
    void     *UserData,
    double   *FuncVal)
{
    double f;
    if (argval[0]==1.) /* argval[0] is the function ID. */
    {
      double a = argval[1];
      double b = argval[2];
      f = fox(a,b);
    }
    else if (argval[0]==2.)
    {
      f = Gox((int)argval[1],argval[2]);
    }

    *FuncVal = f;

    return (0);
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

    /*
    * >>> Step 1 <<< Create a LINDO environment.
    */

    nErrorCode = LSloadLicenseString(
      "../../../license/lndapi150.lic",MY_LICENSE_KEY);
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
    * >>>> Step 3 <<< Set up the instruction list of the model.
    */
    {
        int nLinearz, nAutoDeriv, nConvexRelax, nCRAlgReform;

        /*  Set a log function to call.  */
        nErrorCode = LSsetLogfunc(pModel,(printLOG_t) print_line,NULL);
        APIERRORCHECK;

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

        /*
        * Set up automatic differentiation, before a call to LSreadMPIFile.
        * If not specified, the numerical derivative will be applied
        */
        nAutoDeriv = 0;
        nErrorCode = LSsetModelIntParameter (pModel,
            LS_IPARAM_NLP_AUTODERIV, nAutoDeriv);
        APIERRORCHECK;

        /* Set up MyUserFunc() as the user functionas */
        nErrorCode = LSsetUsercalc (pModel,
            (user_callback_t) MyUserFunc, NULL);
        APIERRORCHECK;

        /* Read instructions from an MPI-file */
        nErrorCode = LSreadMPIFile (pModel,"ex_user.mpi");
        APIERRORCHECK;
    }


    /*
    * >>> Step 5 <<< Perform the optimization using the
    *                multi-start solver
    */

    /* set multi-start as the current NLP solver */
    nErrorCode = LSsetModelIntParameter (pModel,
        LS_IPARAM_NLP_SOLVER, LS_NMETHOD_MSW_GRG);
    APIERRORCHECK;

    nErrorCode = LSoptimize(pModel, LS_METHOD_FREE, NULL);
    APIERRORCHECK;


    /*
    * >>> Step 6 <<< Retrieve the solution
    */
    {
        int nLinearity, i, stat, nvars, ncons;
        double objval=0.0, primal[1000];

        /* Get the linearity of the solved model */
        nErrorCode = LSgetInfo (pModel, LS_IINFO_NLP_LINEARITY, &nLinearity);
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
            printf("obj  = %20.15f \n",objval);
            if ( primal[0] != 0.) printf("f(x) = %20.15f \n",objval/primal[0]);
            printf("G(x) = %20.15f \n",Gox(20,primal[0]));
            for (i=0;i<nvars;i++)
              printf("  x  = %20.15f\n",primal[i]);
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


