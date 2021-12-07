/*
###################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
###################################################################

  File   : ex_nlp5.ox
  Purpose: Solve a multi-extermal continous model using GOP solver.

           MINIMIZE      x * sin(x * pi)
           subject to           0 <= x <= 10
*/

#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>

/* main entry point */
main()
{
   decl nErrorCode;
   decl pEnv;
   decl pModel;

/*
 *>>> Step 1 <<< Create a LINDO environment.
 */
   pEnv = OxLScreateEnv();

/* >>> Step 2 <<< Create a model in the environment. */
   pModel = LScreateModel(pEnv,&nErrorCode);
   LSerrorCheck(pEnv, nErrorCode);

   {
/*
 * >>>> Step 3 <<< Set up the instruction list of the model.
 */
      decl nobjs, ncons, nvars, nnums, lsize;
      decl objsense=new matrix[1];
      decl ctype=new string[2], vtype=new string[2];
      decl code=new matrix[200];
      decl numval=<>,varval=new matrix[2];
      decl objs_beg=new matrix[1], objs_length=new matrix[1], cons_beg=new matrix[1], cons_length=new matrix[1];
      decl lwrbnd=new matrix[2], uprbnd=new matrix[2];
      decl nLinearz, nAutoDeriv, nConvexRelax, nCRAlgReform;
      decl ikod, iobj, icon;

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
      LSerrorCheck(pEnv, nErrorCode);

	  /* Select algebraic reformulation level in convex relaxation*/
      nCRAlgReform = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_CR_ALG_REFORM, nCRAlgReform);
      LSerrorCheck(pEnv, nErrorCode);


	  /* Select convex relax level */
      nConvexRelax = 0;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_CONVEXRELAX, nConvexRelax);
      LSerrorCheck(pEnv, nErrorCode);

      /* Set up automatic differentiation, before a call to
       * LSloadNLPCode. If not specified, the numerical derivative will
       * be applied */
      nAutoDeriv = 1;
      nErrorCode = LSsetModelIntParameter (pModel,
                    LS_IPARAM_NLP_AUTODERIV, nAutoDeriv);
      LSerrorCheck(pEnv, nErrorCode);

      /* Pass the instruction list to problem structure
       * by a call to LSloadNLPCode() */
      nErrorCode = LSloadInstruct (pModel, ncons, nobjs, nvars, nnums,
                    objsense, ctype,  vtype, code, lsize, <>,
                    numval, varval, objs_beg, objs_length, cons_beg,
                    cons_length, lwrbnd, uprbnd);
      LSerrorCheck(pEnv, nErrorCode);
   }

/*
 * >>> Step 5 <<< Perform the optimization using the MIP solver
 */
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_LP_PRINTLEVEL,0);
   LSerrorCheck(pEnv, nErrorCode);
   nErrorCode = LSsetModelIntParameter(pModel,LS_IPARAM_NLP_PRINTLEVEL,1);
   LSerrorCheck(pEnv, nErrorCode);

   decl nStatus;
   nErrorCode = LSsolveGOP(pModel, &nStatus);
   LSerrorCheck(pEnv, nErrorCode);

   {
      decl nLinearity, stat, i, nvars;
      decl objval=0.0, primal;

      /* Get the linearity of the solved model */
      nErrorCode = LSgetInfo (pModel,  LS_IINFO_NLP_LINEARITY, &nLinearity);
      LSerrorCheck(pEnv, nErrorCode);

      nErrorCode = LSgetModelParameter(pModel,LS_IPARAM_STATUS,&stat);
      LSerrorCheck(pEnv, nErrorCode);
      println("\n\n\nSolution status = ",stat);

      /* Report the status of solution */

      if (nLinearity)
	  {
	     print("\nModel has been completely linearized.\n");
	  }
      else
	  {
	     print("\nModel is nonlinear.\n");
	  }

      /* Get the optimization result */
      nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&nvars);
      LSerrorCheck(pEnv, nErrorCode);
      nErrorCode = LSgetInfo(pModel,LS_DINFO_POBJ,&objval);
      LSerrorCheck(pEnv, nErrorCode);
      nErrorCode = LSgetPrimalSolution(pModel,&primal);
      LSerrorCheck(pEnv, nErrorCode);

	  if (stat==1 || stat==2 || stat==5)
	  {
         print("\n\n\nPrinting the solution ... \n");
         println("obj = ", "%f",objval);
		 for (i=0;i<nvars;i++)
            println("x[", i, "] = ", "%f",primal[i]);
	  }else if (stat == 3)
         print("\n\n\nNo feasible solution. \n");

      /* Get the linearity of the solved model */
      nErrorCode = LSgetInfo (pModel,  LS_IINFO_NLP_LINEARITY, &nLinearity);
      LSerrorCheck(pEnv, nErrorCode);

   }

 /* >>> Step 7 <<< Delete the LINDO environment */
   LSdeleteEnv(&pEnv);
}

