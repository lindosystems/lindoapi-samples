//ylin(0)

/* ex_sdp1.c

  A C programming example for solving a mixed semidefinite and
  conic quadratic programming problem,
  where the model is described via an instruction list.

   Example model:
  *****************************************************************
  *                                                               *
  * minimize 2*(x00 + x10 + x11 + x21 + x22) + x0 ;
  * st    x00 + x11 + x22 + x0 = 1 ;
  *       x00 + x11 + x22 + 2*(x10 + x20 + x21) + x1 + x2 = 0.5 ;
  *       x0^2 >= x1^2 + x2^2 ;
  *       x0 >= 0 ;
  *       | x00 x10 x20 |
  *       | x10 x11 x21 |  is positive semidefinite
  *       | x20 x21 x22 |
  *
  *****************************************************************

  Solving such a problem with the LINDO API involves
  the following steps:

      1. Create a LINDO environment.
      2. Create a model in the environment.
      3. Set up the instruction list of the model.
      4. Load the model
      5. Perform the optimization.
      6. Retrieve the solution.
      7. Delete the LINDO environment.
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

/* Set up an outputlog function. */
static void LS_CALLTYPE print_line_log(pLSmodel pModel, char *line, void *userdata)
{
  if (line)
  {
    printf("%s",line);
  } /*if*/
} /*print_line*/

/* main entry point */
int main()

{
   APIERRORSETUP;
/* declare an instance of the LINDO environment object */
   pLSenv pEnv = NULL;
/* declare an instance of the LINDO model object */
   pLSmodel pModel, pModelR=NULL;

   char MY_LICENSE_KEY[1024];
   int n, m, nC, status ;
   double dObj;
  /****************************************************************
   * Step 1: Create a LINDO environment.
   ****************************************************************/
   nErrorCode = LSloadLicenseString("../../../license/lndapi150.lic",MY_LICENSE_KEY);
   APIERRORCHECK;
   APIVERSION;


   pEnv = LScreateEnv ( &nErrorCode, MY_LICENSE_KEY);
   if ( nErrorCode == LSERR_NO_VALID_LICENSE) {
      printf( "Invalid License Key!\n");
      exit( 1);
   }
   APIERRORCHECK;

  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   APIERRORCHECK;
   {
  /****************************************************************
   *  Step 3: Set up the instruction list of the model.
   ****************************************************************/
      int nobjs, ncons, nvars, nnums, lsize;
      int objsense[10];
      char ctype[10], vtype[10];
      int code[200];
      double numval[10],varval[10];
      int objs_beg[10], objs_length[10], cons_beg[10], cons_length[10];
      double lwrbnd[10], uprbnd[10];
      int ikod, iobj, icon;

      /* Number of constraints */
      ncons = 4;
      /* Number of objectives */
      nobjs = 1;
      /* Number of variables */
      nvars = 9;
      /* Number of real number constants */
      nnums = 4;

      /***************
      variable name vs index
       * 0   X00
       * 1   X10
       * 2   X11
       * 3   X21
       * 4   X22
       * 5   X0
       * 6   X20
       * 7   X1
       * 8   X2
      **********************/

      /* Lower bounds of variables */
      lwrbnd[0]=-1e30;
      lwrbnd[1]=-1e30;
      lwrbnd[2]=-1e30;
      lwrbnd[3]=-1e30;
      lwrbnd[4]=-1e30;
      lwrbnd[5]=0    ;
      lwrbnd[6]=-1e30;
      lwrbnd[7]=-1e30;
      lwrbnd[8]=-1e30;

      /* Upper bounds of variables */
      uprbnd[0]=1e30;
      uprbnd[1]=1e30;
      uprbnd[2]=1e30;
      uprbnd[3]=1e30;
      uprbnd[4]=1e30;
      uprbnd[5]=1e30;
      uprbnd[6]=1e30;
      uprbnd[7]=1e30;
      uprbnd[8]=1e30;

      /* Starting point of variables */
      varval[0]=0.0;
      varval[1]=0.0;
      varval[2]=0.0;
      varval[3]=0.0;
      varval[4]=0.0;
      varval[5]=0.0;
      varval[6]=0.0;
      varval[7]=0.0;
      varval[8]=0.0;

      /* Variable type, C= continuous, B = binary */
      vtype[0] = 'C';
      vtype[1] = 'C';
      vtype[2] = 'C';
      vtype[3] = 'C';
      vtype[4] = 'C';
      vtype[5] = 'C';
      vtype[6] = 'C';
      vtype[7] = 'C';
      vtype[8] = 'C';

	  /* Double Precision constants in the model */
      numval[0]=2.0;
      numval[1]=1.0;
      numval[2]=2.0;
      numval[3]=0.5;

      /* Count for instruction code */
	  ikod = 0;
      /* Count for objective row */
	  iobj = 0;
      /* Count for constraint row */
	  icon = 0;

      /*
       *  Instruction code of the objective:
       *
       *  min  2*(x00 + x10 + x11 + x21 + x22) + x0
       */

      /* Direction of optimization */
      objsense[iobj]= LS_MIN;
      /* Beginning position of objective */
      objs_beg[iobj]=ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    2;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    3;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    4;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_MULTIPLY;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    5;
      code[ikod++]=  EP_PLUS;

	  /* Length of objective */
      objs_length[iobj] = ikod - objs_beg[iobj];
      /* Increment the objective count */
	  iobj++;


      /*
       *  Instruction code of constraint 0:
       *
       *   x00 + x11 + x22 + x0 = 1 ;
       */

	  /* Constraint type */
      ctype[icon]= 'E';   /* less or than or equal to */
      /* Beginning position of constraint 0 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    2;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    4;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    5;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    1;
      code[ikod++]=  EP_MINUS;

	  /* Length of constraint 0 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
	  icon++;

      /*
       *  Instruction code of constraint 1:
       *
       *   x00 + x11 + x22 + 2*(x10 + x20 + x21) + x1 + x2 = 0.5 ;
       */

	  /* Constraint type */
      ctype[icon]= 'E';   /* less or than or equal to */
      /* Beginning position of constraint 1 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    0;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    2;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    4;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    2;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    1;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    6;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    3;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_MULTIPLY ;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    7;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    8;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_PUSH_NUM;
      code[ikod++]=    3;
      code[ikod++]=  EP_MINUS;

	  /* Length of constraint 1 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
	  icon++;

      /*
       *  Instruction code of constraint 2:
       *
       *   x0^2 >= x1^2 + x2^2 ;
       */

	  /* Constraint type */
      ctype[icon]= 'G';   /* less or than or equal to */
      /* Beginning position of constraint 2 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    5;
      code[ikod++]=  EP_SQR;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    7;
      code[ikod++]=  EP_SQR;
      code[ikod++]=  EP_PUSH_VAR;
      code[ikod++]=    8;
      code[ikod++]=  EP_SQR;
      code[ikod++]=  EP_PLUS;
      code[ikod++]=  EP_MINUS;

	  /* Length of constraint 2 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
	  icon++;

      /*
       *  Instruction code of constraint 3:
       *
       *   | x00 x10 x20 |
       *   | x10 x11 x21 |  is positive semidefinite
       *   | x20 x21 x22 |
       */

	  /* Constraint type */
      ctype[icon]= 'G';
      /* Beginning position of constraint 3 */
      cons_beg[icon]= ikod;
      /* Instruction list code */
      code[ikod++]=  EP_POSD  ;   // POSD constraint
      code[ikod++]=    3;   // dimension of matrix
      code[ikod++]=    6;   // number of matrix elements
      // 1st matrix element
      code[ikod++]=    0;  // variable index
      code[ikod++]=    0;  // row index
      code[ikod++]=    0;  // col index
      // 2nd matrix element
      code[ikod++]=    1;  // variable index
      code[ikod++]=    1;  // row index
      code[ikod++]=    0;  // col index
      // 3rd matrix element
      code[ikod++]=    6;  // variable index
      code[ikod++]=    2;  // row index
      code[ikod++]=    0;  // col index
      // 4th matrix element
      code[ikod++]=    2;  // variable index
      code[ikod++]=    1;  // row index
      code[ikod++]=    1;  // col index
      // 5th matrix element
      code[ikod++]=    3;  // variable index
      code[ikod++]=    2;  // row index
      code[ikod++]=    1;  // col index
      // 6th matrix element
      code[ikod++]=    4;  // variable index
      code[ikod++]=    2;  // row index
      code[ikod++]=    2;  // col index

	  /* Length of constraint 3 */
      cons_length[icon] = ikod - cons_beg[icon];
      /* Increment the constraint count */
	  icon++;



      /* Total number of items in the instruction list */
      lsize = ikod;

   /****************************************************************
   *  Step 4: Load the model
   ****************************************************************/
     /* Pass the instruction list to problem structure
       * by a call to LSloadNLPCode() */
      nErrorCode = LSloadInstruct (pModel, ncons, nobjs, nvars, nnums,
                    objsense, ctype,  vtype, code, lsize, NULL,
                    numval, varval, objs_beg, objs_length, cons_beg,
                    cons_length, lwrbnd, uprbnd);
      APIERRORCHECK;
   }


   /***************************************************************
    * Step 5: Optimize the model
    ***************************************************************/
   /*  Set a log function to call.  */
   nErrorCode = LSsetModelLogfunc(pModel, (printModelLOG_t) print_line_log, NULL);
   APIERRORCHECK;

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   nErrorCode += LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);
   nErrorCode += LSgetInfo(pModel,LS_IINFO_NUM_CONT,&nC);
   APIERRORCHECK;

   nErrorCode = LSoptimizeQP( pModel, &status);
   APIERRORCHECK;

   /***************************************************************
    * Step 6: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_BASIC_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE)
   {
     double *primal = NULL, *dual = NULL;
     int    j;

     primal = (double *) malloc(n*sizeof(double));
     dual   = (double *) malloc(m*sizeof(double));

     nErrorCode = LSgetPrimalSolution( pModel, primal) ;
     APIERRORCHECK;
     nErrorCode = LSgetDualSolution( pModel, dual) ;
     APIERRORCHECK;
     nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&dObj);
     APIERRORCHECK;

     printf ("\n Objective at solution = %f \n", dObj);


     // un/comment the block below if you would like the primal and dual solutions
     // to be printed on the screen.
     if (1){
       char szname[255];
       printf ("\n Primal Solution\n");
       printf("\t%8s %18s\n","VARS", "Primal");
       for (j = 0; j<n; j++)
       {
         nErrorCode = LSgetVariableNamej(pModel,j,szname);
         printf("\t%8s %18.10e\n",szname, primal[j]);
       }

       printf ("\n Dual Solution\n");
       printf("\t%8s %18s\n","CONS", "Dual");
       for (j = 0; j<m; j++)
       {
         nErrorCode = LSgetConstraintNamei(pModel,j,szname);
         printf("\t%8s %18.10e\n",szname, dual[j]);
       }
     }
     free(primal);
     free(dual);
   }
   else
   {
     char strbuf[255];
     LSgetErrorMessage(pEnv,nErrorCode,strbuf);
     printf ("\n Optimization failed. Status = %d ",status);
     //printf ("\n Error %d: %s\n",nErrorCode,strbuf);
   }

Terminate:
   /***************************************************************
    * Step 7: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);


  /* Wait until user presses the Enter key */
   printf("Press <Enter> ...");
   getchar();

}
// ylin(1)