 /*
#################################################################
#                       LINDO-API
#                    Sample Programs
#                  Copyright (c) 2006
#
#         LINDO Systems, Inc.           312.988.7422
#         1415 North Dayton St.         info@lindo.com
#         Chicago, IL 60622             http://www.lindo.com
#################################################################
  File   : markow.ox
  Purpose: Solve a quadratic programming problem.
  Model  : The Markowitz Portfolio Selection Model

           MAXIMIZE  r(1)w(1) + ... +r(n)w(n)
           st.       sum_{ij} Q(i,j)w(i)w(j) <= K
                         w(1) +  ...  + w(n)  = 1
                         w(1),         ,w(n) >= 0
           where
           r(i)  : return on asset i
           Q(i,j): covariance between the returns of i^th and
                   j^th assets.
           K     : a scalar denoting the level of risk of loss.
           w(i)  : proportion of total budget invested on asset i

    Data :
  Covariance Matrix:
          w1    w2    w3    w4
     w1 [ 1.00  0.64  0.27  0.    ]
     w2 [ 0.64  1.00  0.13  0.    ]
     w3 [ 0.27  0.13  1.00  0.    ]
     w4 [ 0.    0.    0.    1.00  ]
   Returns Vector:
          w1    w2    w3    w4
  r =   [ 0.30  0.20 -0.40  0.20  ]
  Risk of Loss Factor:
  K = 0.2
*/
#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>

/* main entry point */
main()
{
   decl nErrorCode;
   decl i,len;
   decl nM = 2;      /* Number of constraints */
   decl nN = 4;      /* Number of assets */
   decl K = 0.20; /* The risk level*/
  /* declare an instance of the LINDO environment object */
   decl pEnv;
  /* declare an instance of the LINDO model object */
   decl pModel;
  /****************************************************************
   * Step 1: Create a LINDO environment. MY_LICENSE_KEY in licence.h
   * must be defined using the key shipped with your software.
   ****************************************************************/
   pEnv = OxLScreateEnv();
  /****************************************************************
   * Step 2: Create a model in the environment.
   ****************************************************************/
   pModel = LScreateModel ( pEnv, &nErrorCode);
   LSerrorCheck(pEnv, nErrorCode);
  /*****************************************************************
   * Step 3: Specify and load the LP portion of the model.
   *****************************************************************/
   {
     /* The direction of optimization */
      decl objsense = LS_MAX;
      /* The objective's constant term */
      decl objconst = 0.;
      /* The coefficients of the objective function are the expected
      returns*/
      decl reward = < .3, .2, -.4, .2>;
      /* The right-hand sides of the constraints */
      decl rhs = K ~ 1.0;
      /* The constraint types */
      decl contype = "LE";
      /* The number of nonzeros in the constraint matrix */
      decl Anz = 4;
      /* The indices of the first nonzero in each column */
      decl Abegcol = < 0, 1, 2, 3> ~ Anz;
      /* The length of each column.  Since we aren't leaving
       * any blanks in our matrix, we can set this to NULL */
      decl Alencol = <>;
      /* The nonzero coefficients */
      decl A = < 1., 1., 1., 1.>;
      /* The row indices of the nonzero coefficients */
      decl Arowndx = < 1, 1, 1, 1>;
      /* By default, all variables have a lower bound of zero
       * and an upper bound of infinity.  Therefore pass NULL
       * pointers in order to use these default values. */
      decl lb = <>, ub = <>;
  /*****************************************************************
   * Step 4: Specify and load the quadratic matrix
   *****************************************************************/
     /* The number of nonzeros in the quadratic matrix */
      decl Qnz = 7;
      /* The nonzero coefficients in the Q-matrix */
      decl Q =      < 1.00, .64, .27,
                           1.00, .13,
                                1.00,
                                      1.00> ;
      /* Specify the row indices of the nonzero coefficients in the
         Q-matrix. */
      decl Qrowndx = < 0, 0, 0, 0, 0, 0, 0>;
      /* The indices of variables in the Q-matrix */
      decl Qcolndx1 = <  0, 1, 2, 1, 2, 2, 3>;
      decl Qcolndx2 = <  0, 0, 0, 1, 1, 2, 3>;
      /* Pass the linear portion of the data to problem structure
       * by a call to LSloadLPData() */
      nErrorCode = LSloadLPData( pModel, nM, nN, objsense, objconst,
                                reward, rhs, contype,
                                Anz, Abegcol, Alencol, A, Arowndx,
                                lb, ub);
      LSerrorCheck(pEnv, nErrorCode);
     /* Pass the quadratic portion of the data to problem structure
      * by a call to LSloadQCData()  */
      nErrorCode = LSloadQCData(pModel, Qnz, Qrowndx,
                                 Qcolndx1, Qcolndx2, Q );
      LSerrorCheck(pEnv, nErrorCode);
   }
  /*****************************************************************
   * Step 5: Perform the optimization using the barrier solver
   *****************************************************************/
   decl nSolStatus;
   nErrorCode = LSoptimize( pModel, LS_METHOD_BARRIER, &nSolStatus);
   LSerrorCheck(pEnv, nErrorCode);
  /***************************************************************
   * Step 6: Retrieve the solution
   ***************************************************************/
   {
      decl i;
      decl W, dObj;
   /* Get the value of the objective */
      nErrorCode = LSgetInfo( pModel, LS_DINFO_POBJ, &dObj) ;
      LSerrorCheck(pEnv, nErrorCode);
      print( "* Objective Value = ", "%10g", dObj, "\n\n");
    /* Get the portfolio */
      nErrorCode = LSgetPrimalSolution ( pModel, &W);
      LSerrorCheck(pEnv, nErrorCode);
      print ("* Optimal Portfolio : \n");
      for (i = 0; i < nN; i++)
         println( "Invest ", "%5.2f", 100*W[i], " percent of total budget in asset ",
               i+1 );
      print("\n");
   }
  /***************************************************************
   * Step 7: Delete the LINDO environment
   *****************************************************************/
   nErrorCode = LSdeleteEnv( &pEnv);
}
