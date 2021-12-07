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

  File   : ex_nlp4.ox
  Purpose: Read a nonconvex nonlinear model from an MPI file and
           optimize with the GOP solver
*/
#include <oxstd.h>

/* LINDO API header file is located under lindoapi\ox */
#import <packages/lindoapi/ox/oxlindo>

/****************************************************************
   Standard callback function to display local and intermediate
   solutions
 ****************************************************************/
print_log(const model,const iLoc, const cbData)
{
  decl siter=0,niter=0;
  decl pobj=0.0;
  decl bestbnd;
  decl status;

  if (iLoc == LSLOC_GOP)
  {
    LSgetCallbackInfo(model,iLoc,LS_IINFO_GOP_STATUS,&status);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_GOP_SIM_ITER,&siter);
    LSgetCallbackInfo(model,iLoc,LS_IINFO_GOP_NLP_ITER,&niter);
    LSgetCallbackInfo(model,iLoc,LS_DINFO_GOP_OBJ,&pobj);
    println("Iters=", "%6d",siter+niter, " \tObj=", "%11.5e",pobj, " \tStatus=",
      status);
  }
  return 0;
} /*print_log*/

/* main entry point*/
main()
{
   decl nErrorCode;
   decl m, n; /* number of constraints and vars */
   decl dObj;
   decl    status;
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
  /****************************************************************
   * Step 3: Read the model from an MPS file and get the model size
   ****************************************************************/
   nErrorCode = LSreadMPIFile(pModel,"testgop.mpi");

   if (nErrorCode != LSERR_NO_ERROR) {
     print("\n Bad  MPI  format\n");
   } else {
     print("Reading MPI format. \n\n");
   }
   LSerrorCheck(pEnv, nErrorCode);

   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_VARS,&n);
   LSerrorCheck(pEnv, nErrorCode);
   nErrorCode = LSgetInfo(pModel,LS_IINFO_NUM_CONS,&m);   
   LSerrorCheck(pEnv, nErrorCode);
   /***************************************************************
    * Step 4: Optimize the model
    ***************************************************************/
   status = LS_STATUS_UNKNOWN;
   LSsetModelDouParameter(pModel,LS_DPARAM_CALLBACKFREQ,2.0);
   /* Install a callback function */
   LSsetCallback(pModel, print_log, 0);
   /* optimize */
   print("\tSolving for Global Solution\n\n");
   nErrorCode = LSsolveGOP( pModel, &status);
   /***************************************************************
    * Step 5: Access the final solution if optimal or feasible
    ***************************************************************/
   if (status == LS_STATUS_OPTIMAL ||
       status == LS_STATUS_LOCAL_OPTIMAL ||
       status == LS_STATUS_FEASIBLE )
   {
     decl primal, dual;
     decl    j;

     nErrorCode = LSgetInfo(pModel, LS_DINFO_MIP_OBJ, &dObj);
       LSerrorCheck(pEnv, nErrorCode);
     nErrorCode = LSgetMIPPrimalSolution( pModel,&primal);
       LSerrorCheck(pEnv, nErrorCode);
     nErrorCode = LSgetMIPDualSolution( pModel,&dual);
       LSerrorCheck(pEnv, nErrorCode);
     println ("\n Objective = ", "%f", dObj);
     print ("\n Primal Solution\n");
     for (j = 0; j<n; j++)
       println("\tprimal[", j, "] = ", "%18.10e", primal[j]);

     print ("\n Dual Solution\n");
     for (j = 0; j<m; j++)
         println("\tdual[", j, "] = ", "%18.10e", dual[j]);
   }
   /***************************************************************
    * Step 6: Terminate
    ***************************************************************/
   nErrorCode = LSdeleteModel( &pModel);
   nErrorCode = LSdeleteEnv( &pEnv);
}
