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

  @file   : ex_user2.c

  @purpose: Solve an NLP that uses two black-box functions within
  the instruction-list interface.

            minimize F(x0,x1,...,xn)
                     x0  <= 10

  The black-box functions are

    F(x)   a user defined function which has no closed forms.

  @remark : This application uses the Instruction Style Interface,
  where the instructions are built programatically.

  @remark : EP_USER operator is used in the instruction list to
  identify the black-box function and specify the number of
  arguments they take.

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
 *          Black-box function #1 -- f(x).
 */
double fox(double x[], int n)
{
  double dobj = 0;
  int i=0;
  double u;


  u = rand()/((double)RAND_MAX);
  if (u>0.5)
  {
    for (i=0; i<n; i++) dobj += pow(x[i],i);
  }
  else
  {
    for (i=0; i<n; i++) dobj += pow(x[i],u);
  }

  return dobj;
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
    double f, *x;

    x = &argval[0];
    f = fox(x,4);
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
      "../../../license/lndapi140.lic",MY_LICENSE_KEY);
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

#if 0
        /* Read instructions from an MPI-file */
        nErrorCode = LSreadMPIFile (pModel,"ex_user2.mpi");
        APIERRORCHECK;
#endif
    }


   {
      /*
       * >>>> Step 4 <<< Set up the instruction list of the model.
       */
      int nobjs, ncons, nvars, nnums, lsize;
      int objsense[10];
      char ctype[10], vtype[10];
      int code[200];
      double numval[10],varval[10];
      int objs_beg[10], objs_length[10], cons_beg[10], cons_length[10];
      double lwrbnd[10], uprbnd[10];
      int ikod, iobj, icon;

      /* Number of constraints */
      ncons = 1;
      /* Number of objectives */
      nobjs = 1;
      /* Number of variables */
      nvars = 4;
      /* Number of real number constants */
      nnums = 1;

      /* Lower bounds of variables */
      lwrbnd[0]=0.0;
      lwrbnd[1]=0.0;
      lwrbnd[2]=0.0;
      lwrbnd[3]=0.0;

      /* Upper bounds of variables */
      uprbnd[0]=100; // dummy variable
      uprbnd[1]=10;
      uprbnd[2]=10;
      uprbnd[3]=10;

      /* Starting point of variables */
      varval[0]=1.2345;
      varval[1]=1.2345;
      varval[2]=1.2345;
      varval[3]=1.2345;

      /* Variable type, C= continuous, B = binary */
      vtype[0] = 'C';
      vtype[1] = 'C';
      vtype[2] = 'C';
      vtype[3] = 'C';

      /* Numerical values */
      numval[0] = 100;


      /* Count for instruction code */
      ikod = 0;
      /* Count for objective row */
      iobj = 0;
      /* Count for constraint row */
      icon = 0;

      /*
       *  Instruction code of the objective:
       *
       *  min  f(x0,x1,x2,x3)
       */

      /* Direction of optimization */
      objsense[iobj]= LS_MIN;
      /* Beginning position of objective */
      objs_beg[iobj]=ikod;

      /* Instruction list code to define a four variable user function*/
      code[ikod++]=  EP_PUSH_VAR; //x0
      code[ikod++]=            0; //x0
      code[ikod++]=  EP_PUSH_VAR; //x1
      code[ikod++]=            1; //x1
      code[ikod++]=  EP_PUSH_VAR; //x2
      code[ikod++]=            2; //x2
      code[ikod++]=  EP_PUSH_VAR; //x3
      code[ikod++]=            3; //x3
      code[ikod++]=  EP_USER;     //@user(x0,..,x3)
      code[ikod++]=  4;

      /* Length of objective */
      objs_length[iobj] = ikod - objs_beg[iobj];
      /* Increment the objective count */
      iobj++;


      /*
       *  Instruction code of constraint 0:
       *
       *   x0  <= 100 ;  (a dummy constraint)
       */

    /* Constraint type */
      ctype[icon]= 'L';   /* less or than or equal to */
      /* Beginning position of constraint 0 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    0;
      code[ikod++]=  EP_MINUS;


      /* Length of constraint 0 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
      icon++;

      /* Total number of items in the instruction list */
      lsize = ikod;

      /* Pass the instruction list to problem structure
       * by a call to LSloadNLPCode() */
      nErrorCode = LSloadInstruct (pModel, ncons, nobjs, nvars, nnums,
                    objsense, ctype,  vtype, code, lsize, NULL,
                    numval, varval, objs_beg, objs_length, cons_beg,
                    cons_length, lwrbnd, uprbnd);
      APIERRORCHECK;
   }


    /*
    * >>> Step 5 <<< Perform the optimization using the
    *                multi-start solver
    */

    /* set multi-start as the current NLP solver */
    nErrorCode = LSsetModelIntParameter (pModel,LS_IPARAM_NLP_SOLVER, LS_NMETHOD_MSW_GRG);
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


