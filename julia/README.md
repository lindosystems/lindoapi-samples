﻿# Lindo API in Julia 



## Introduction

The Julia package Lindoapi.jl offers two ways to interface with the Lindo API. 
The first way to interface with the API is through the soft wrapper that allows users to directly call the functions implemented in the Lindo API. The second way to interface with the API is with the JuMP.jl package. Both ways of interfacing have their strengths calling Lindo API functions directly using the soft wrapper is more efficient with computation speed and memory use while using the JuMP interface allows for much more natural expression of a model.



## Downloading Lindo API

Before adding the Lindoapi package please [download](https://www.lindo.com/index.php/ls-downloads/try-lindo-api) the API
See the [manual](https://www.lindo.com/downloads/PDF/API.pdf) for operating system specific downloading instructions and keep as a reference for the available API functions.

## Installing Lindoapi.jl

The Julia library needs to know where the Lindo API is stored. To do so create an environment variable named. ```LINDOAPI_HOME```. 
### Using Windows
On the command line
```dos
> set LINDOAPI_HOME="c:/lindoapi"
```
Or in julia
```julia
ENV["LINDOAPI_HOME"] = "c:/lindoapi"
```

### Using Mac or Linux
On the command line
```sh
$ export LINDOAPI_HOME="/opt/lindoapi"
```
Or in julia
```julia
ENV["LINDOAPI_HOME"] = "/opt/lindoapi"
```

To add the package in Julia
```julia
using Pkg
Pkg.add(url="https://github.com/lindosystems/lindoapi.jl")
```
# Using the soft wrapper
The [manual](https://www.lindo.com/downloads/PDF/API.pdf) has documentation on every function available, and plenty of samples all of which are straight forward to convert to Julia. For a quick start guide and to see how Julia works with the API this section will go through the steps that are important to getting started with the API.

### Table of Contents
1. Creating Modeling Environment and Model
2. Loading Data
3. Calling the Optimizer
4. Query the Model
5. Freeing Model and Environment

### Creating Modeling Environment and Model 

This section goes over the creation of the pointers to the environment and a model. 

**1)**  ```LSloadLicenseString(pszFname, pachLicense)```

This function loads users license key into an array. This function comes first since the API key is needed to create API environment for models.

``pszFname``: A string that contains the full path to your Lindo API license key.


``pachLicense``: An empty vector of type unsinged Int 8 (UInt8)


``Returns``: An error code of type Int if not 0 then it has failed.

```julia
PATH = ENV["LINDOAPI_HOME"]
# undef argument for not initializing the vector
# used for performance
LicenseKey = Vector{UInt8}(undef,1024)
# joinpath is a built in function for concatenating two paths.
license_path = joinpath(PATH,"license/lndapi150.lic")     
ret = Lindoapi.LSloadLicenseString(license_path, LicenseKey)
if ret != 0
    error("Key not found check key $(license_path)")
end
```

**2)** ```LScreateEnv(pnErrorCode, LicenseKey)```

This function creates a modeling environment that one or more models can be attached to.


``pnErrorCode``: An array of Int32 initialized with one element for LScreateEnv to store the error code. If ``pnErrorCode[1] == 0`` after the function is ran, then it was successful.


``LicenseKey``: A vector of UInt8 that is initialized by `LSloadLicenseString`.


``Returns``: A pointer to the newly created instance of LSenv.

```julia
pnErrorCode = Int32[-1]
pEnv = Lindoapi.LScreateEnv(pnErrorCode, LicenseKey)
Lindoapi.check_error(pEnv,pnErrorCode[1])
```

**3)** ```check_error(pEnv, ErrorCode)```

Once the environment is created error codes can be checked and error messages can be displayed.

``pEnv``:  A pointer to an environment created by `LScreateEnv`.

``ErrorCode``: The first element of pnErrorCode.

``Returns``:  Prints an error message If ``pnErrorCode[1] != 0`` and then the program is exited.

```julia
Lindoapi.check_error(pEnv,pnErrorCode[1])
```

**4)** `LScreateModel(pEnv, pnErrorCode)` This function creates a model pointer.

``pEnv``:           A pointer to an environment created by LScreateEnv.

``pnErrorCode``:    An array of Int32 initialized with one element for `LScreateEnv` to store the error code when `pnErrorCode[1] == 0` the function is successful.

``Returns`` : A pointer to a newly created model.

```julia
pModel = Lindoapi.LScreateModel(pEnv, pnErrorCode)
Lindoapi.check_error(pEnv,pnErrorCode[1])
```
### Loading Data
Once an environment has been created and a model has been attached the data can be added to the model. This can be done in a variety of ways depending on what kind of data is being added. See the attached examples to get an idea of how different data types are loaded into a model. Also see Chapter 1 of the [manual](https://www.lindo.com/downloads/PDF/API.pdf) to learn more about array and matrix representation used by the Lindo API.

**Examples provided for:**
* `` LSloadNLPData``
* ``LSloadLPData``
* ``LSloadQCData``
* ``LSloadInstruct``

### Calling The Optimizer 

*  ``LSoptimize(pModel, nMethod, pnSolStatus)`` Use this when all variables are continuous.
* ``LSsolveMIP(pModel, pnMIPSolStatus)``   When there is one or more integer variables.
* ``LSsolveGOP(pModel, pnGOPSolStatus)``  For global optimization of nonlinear models. 

``pModel``: The pointer to the model being optimized.

``pnSolStatus``, ``pnMIPSolStatus``, ``pnGOPSolStatus``: To store the optimization status a vector of type Int32 with a single value  ``pnStatus = Int32[-1]``. The solver will replace the single value with the optimization status when finished.
For a detailed list describing all 14 different model statuses see the [manual](https://www.lindo.com/downloads/PDF/API.pdf) page 22 in the Common Parameter Macro Definitions table.

``nMethod``: Only used in  ``LSoptimize``. This is a value of type Int that corresponds to what kind of optimization method the solver will use. There are stored constants in the Lindoapi.jl for the possible values of ``nMethod``. Use ``Lindoapi.LS_METHOD_FREE`` to let the solver decide what is best.


• ``Lindoapi.LS_METHOD_FREE``      =  0
• ``Lindoapi.LS_METHOD_PSIMPLEX``  = 1
• ``Lindoapi.LS_METHOD_DSIMPLEX``  = 2
• ``Lindoapi.LS_METHOD_BARRIER``  = 3
• ``Lindoapi.LS_METHOD_NLP``      = 4

```julia 
pnStatus = Int32[-1]
errorcode = Lindoapi.LSoptimize(pModel, Lindoapi.LS_METHOD_FREE, pnStatus)
LS.check_error(pEnv, errorcode)
```

```julia 
if pnStatus[1] == Lindoapi.LS_STATUS_OPTIMAL
    println("Optimal solution found!")
end
```



### Querying the Model

``LSgetInfo(pModel, nQuery, pvResult)``  
Used to get model objective value or other information about the model. 

``pmodel``:  A pointer to the model that is being queried.

``nQuery``:  An Integer that corresponds to what is being queried Lindoapi.jl has a constant stored for each possible nQuery. For example, to get the objective value when the model has continuous variables use ``Lindoapi.LS_DINFO_POBJ`` . When there are integer variables, use ``Lindoapi.LS_IINFO_POBJ`` to get the objective value. For a detailed list of all query values see the [manual](https://www.lindo.com/downloads/PDF/API.pdf) page 139.

``pvResults``: Used to store the value returned the function takes Ptr{Cvoid} making versatile for integer values use
``pvResult = Int32[-1]`` and for floating point use ``pvResults = Cdouble[-1]``.  


```julia
#retrieve the objective value
dObj = Cdouble[-1]
errorcode = Lindoapi.LSgetInfo(pModel, Lindoapi.LS_DINFO_POBJ, dObj)
Lindoapi.check_error(pEnv, errorcode)
```


* ``LSgetPrimalSolution(pModel, padPrimal)`` 
* ``LSgetDualSolution(pModel, padDual)``
* ``LSgetMIPPrimalSolution(pModel, padPrimal)``
* ``LSgetMIPDualSolution(pModel, padDual)``

The arguments for these functions are similar use the ``MIP`` functions when the model has any integer variables.

``pmodel``:  A pointer to model that is being queried.
``padPrimal``:  A vector of Cdouble that is the length of the number of variables in the model the function will replace the values in the list with the primal values.
``padDual``: A vector of Cdouble that is the length of the number of constraints in the model.

```julia
padPrimal = Vector{Cdouble}(undef, nVars)
errorcode = Lindoapi.LSgetPrimalSolution(pModel, padPrimal)
Lindoapi.check_error(pEnv, errorcode)
```

### Freeing Model and Enviroment 

Lindo API is allocating memory that Julia does not manage. It is important to free this memory before ending the program. First free the memory allocated to each model attached to the environment then free the environment using the two functions described below.
* ``LSdeleteModel(pModel)``
* ``LSdeleteEnv(pEnv)``

``pModel``:  A pointer to a model being freed
``pEnv``:  A pointer to the environment being freed

```julia
#delete LINDO model pointer
errorcode = Lindoapi.LSdeleteModel(pModel)
Lindoapi.check_error(pEnv, errorcode)

#delete LINDO environment pointer
errorcode = Lindoapi.LSdeleteEnv(pEnv)
Lindoapi.check_error(pEnv, errorcode)
```

# Using the JuMP Interface

[JuMP](https://jump.dev/JuMP.jl/stable/) is a modeling language embedded in the Julia programming language. This section will cover the functions from JuMP that Lindoapi.jl supports, and how to use them. Samples are also provided in the folder `JuMP_interface` for more concrete examples.

### Table of Contents
1. Creating a Model
2. Making Variables
3. Constraints
4. Objectives
5. Setting Model Attributes
6. Calling the Optimizer
7. Querying the Model

### Creating a Model

Using the JuMP function `Model` will create an empty model.

```julia
model = Model(Lindoapi.Optimizer)
```

`Lindoapi.Optimizer` sets the optimizer to the Lindo API.

### Making Variables

The Lindoapi.jl supports the JuMP macros `@variable` and `@variables` to attach variables to a model.

`@variable(model, expr, args...)`

`expr`:  Includes variable or variables and bounds.

* `@variable(model, x)`   Single variable
* `@variable(model, x[1:n])`  Vector of `n`  variables.
* `@variable(model, x[1:n, 1:m])` Matrix of `n` by `m`  variables.
* `@variable(model, a <= x[1:n] <= b)`  Vector  `n`  variables bounded between `a` and `b`

`@variables(model, args...)` add multiple variables wrapped in a `begin ..end`

``` julia
@variables(model,
    begin
    2 <= x1 <= 5
    1 <= x2
         x3 <= 10
         x4
     end
)
```

`args`:  Supported `Bin` for binary and `Int` for integer variables. This argument can be used on any of the above examples.

* `@variable(model, z[1:n], Int)` Vector of `n` integer variables.

The variables in arrays can be accessed like typical julia array use `x[i]` to access ith variable in the array, and `x[i,j]` to access the i jth variable in a matrix.

### Constraints

To attach a constraint to a model, use the macro`@NLconstraint`

`@NLconstraint(model, ref, expr)`

`ref`: A constraint name for referncing. This is an optional argument and is equivalent to `ref = @NLconstraint(model, expr)`.

`expr`:  Constraint expression. Lindoapi.jl supports the constraints `<=`, `>=`  and `==`.

``` julia
@NLconstraint(model, con1, x[1]*x[2] <=  6)
```

To sum over or take the product of variables JuMP supports `sum` and `prod` operations.
```julia
@NLconstraint(model, sum(x[i] for i in 1:n) == 1)
```

To make an array of `n` constraints use `[i = 1:n]` before the constraint expression. The variable `i` can be used to index over variables as well.
```julia
@NLconstraint(
	model,
	con2[i = 1:(n - 1)],
	θ[i] - θ[i+1] <= 0
)
```

**Note:**  `@NLconstraint` does not support matrix and vector operations, and the `sum` function will have to be used instead. Here is an example of `w'Σ*w` using a double `sum`.

```julia
@NLconstraint(
	model,
	sum(w[i] * sum(Σ[i,j] * w[j]
		for j in 1:n) for i in 1:n)  <= K
)
```

###  Objectives

To attach an objective to a model, use the macro`@NLobjective`

`@NLobjective(model, sense, expr)`

`sense`: Lindoapi.jl  supports `Min` and `Max`

`expr`: The objective expression is expressed just as the constraints are, and supports `sum` and `prod` as well.

```julia
NLobjective(
	model,
	Min,
	sum(x[i,j]*exp(f[i,j]*(x[i,j]-g[i,j]))
	for i in 1:m, j in 1:n)
)
```

**Note:** JuMP currently only supports one objective function per model with `@NLobjective`.
### Setting Model Attributes

Model attributes can be set using the overloaded function `set_optimizer_attribute()`
 * `set_optimizer_attribute(model::Model, name::String, value)`
 * `set_optimizer_attribute(model::Model, name::LindoIntParam, value)`
 * `set_optimizer_attribute(model::Model, name::LindoDouParam, value)`

`set_optimizer_attribute(model::Model, name::String, value)` 

`name`:  This argument can be any of the three listed below.
* `"use_global"` toggles on/off the global solver.
* `"silent"` toggles on/off the defualt callbacks
* `"solverMethod"` Sets the solver method to `LSoptimize()`.

`value`:  Both `"use_global"` and `"silent"` only accept boolean arguments `true` or `false`.  The acceptable arguments for `"solverMethod"` are the following integers.

* 0  `LS_METHOD_FREE` 
* 1  `LS_METHOD_PSIMPLEX`
* 2  `LS_METHOD_DSIMPLEX`
* 3  `LS_METHOD_BARRIER`
* 4  `LS_METHOD_NLP`  

```julia
method = 1  # LS_METHOD_PSIMPLEX
set_optimizer_attribute(
	model,
	"solverMethod",
	 method
) 
 ```
 
 `name`:  A type LindoIntParam is unique to the Lindoapi, and is used hold a Lindo API integer parameter.  
 
 `value`: The integer value of the parameter being set.
 
 ```julia
iparam = Lindoapi.LindoIntParam(Lindoapi.LS_IPARAM_SOLVER_METHOD)
solver_method = 3     #LS_METHOD_BARRIER
JuMP.set_optimizer_attribute(
	model,
	iparam,
	solver_method
)
```

 `set_optimizer_attribute(model::Model, name::LindoDouParam, value)`  
 
 `name`:  A type LindoDouParam is also unique to the Lindoapi, and is used hold a Lindo API double parameter.  
 
 `value`: The Float value of the parameter being set.
 
 ```julia
iparam = Lindoapi.LindoDouParam(Lindoapi.LS_DPARAM_IPM_BASIS_TOL_S)
adtol = 10^(-8)     #Maximum absolute dual bound violation
JuMP.set_optimizer_attribute(
	model,
	iparam,
	adtol
)
```

See [manual](https://www.lindo.com/downloads/PDF/API.pdf) **Available Parameters** section page 64 for a detailed list of all parameters. Any parameter starting with LS_IPARAM will work with LindoIntParam and any parameter starting with LS_DPARAM will work with LindoDouParam.

### Callback Functions

Custom callback functions can be attached to the model. To attach one to the model use the overloaded `MOI.set()` function. 


`MOI.set(model, ::AbstractCallback, cbfunc)`

`::AbstractCallback` Will represent the type of callback function being set. This abstract data type has been extended by four datatypes exclusive to the Lindoapi.jl

* `LogFunction(uDict::Dict{String, Any})`
* `CallbackFunction(uDict::Dict{String, Any})`
* `GOPCallbackFunction(uDict::Dict{String, Any})`
* `MIPCallbackFunction(uDict::Dict{String, Any})`

`uDict`: A dictionary of Data with keys of type String and data of any datatype. This is pass through data to access within the callback function.

This sample  demonstrates the flexibility of uDict as passthrough data. Also the ease of writing a callback function, and initializing a `LogFunction` type. Finally how to set the callback with `MOI.set()`.  
```julia
uDict = Dict(
"Prefix"     => "Custom Callback",
"Postfix"    => "...",
"firstPrint" => true,
"model"      => model)

function logFunc(modelPtr, line, uDict)
        if uDict["firstPrint"] == true
                println(uDict["model"])
                uDict["firstPrint"] = false
        else
                @printf("%s",line)
        end
end
logCallback = LogFunction(uDict)

MOI.set(model, Lindoapi.LogFunction(uDict), logFunc)
```

Here are templates for the all the callback functions
```julia
uDict = Dict(
"Prefix"     => "Custom Callback",
"Postfix"    => "...",)

function logFunc(modelPtr, line, uDict)
  # your code here
end
MOI.set(model, Lindoapi.LogFunction(uDict), logFunc)
```
```julia
function cbFunc(pModel, loc, uDict)
	# your code here
end
MOI.set(model, Lindoapi.CallbackFunction(uDict), cbFunc)
```
```julia
function cbMIPFunc(modelPtr, uDict, objValue, pimalValues)
	# your code here
end
MOI.set(model, Lindoapi.MIPCallbackFunction(uDict), cbMIPFunc)
```
```julia
function cbGOPFunc(modelPtr, uDict, objValue, pimalValues)
	# your code here
end
MOI.set(model, Lindoapi.GOPCallbackFunction(uDict), cbGOPFunc)
```
The `uDict` is the only data that you need to provide. The rest will be passed to the functions by the Lindo API. See the [manual](https://www.lindo.com/downloads/PDF/API.pdf)  for more detail on how to use the Callbacks and the Lindo API functions such as `LSgetCallbackInfo()`. 

**Note** By `using JuMP` the Mathoptinterface.jl libray is also imported and a constant `MOI = Mathoptinterface` is set. 
 
### Calling the Optimizer

Once the variables, constraints and objective have been added to the model use the function `optimize!`.

```julia
optimize!(model)
```

###  Querying the Model

A list of supported functions supported to query the model and its variables

* `termination_status(model)`
* `raw_status(model)`
* `primal_status(model)`
* `dual_status(model)`
* `objective_value(model)`
* `dual_objective_value(model)`
* `value(x)`
* `reduced_cost(x)`
* `dual(constraint_name)`
* `get_optimizer_attribute(model,Lindoapi.Slack_or_Surplus())`
* `solution_summary(model)`
* `println(model)`

`termination_status(model)`  A list of all possible returns and what Lindo termination status maps to them. 

* OPTIMAL
	* LS_STATUS_OPTIMAL
	* LS_STATUS_BASIC_OPTIMAL
* ALMOST_OPTIMAL
	* LS_STATUS_NEAR_OPTIMAL
* LOCALLY_SOLVED
	* LS_STATUS_LOCAL_OPTIMAL
* ALMOST_LOCALLY_SOLVED
	* LS_STATUS_FEASIBLE
* INFEASIBLE
	* LS_STATUS_INFEASIBLE
* INFEASIBLE_OR_UNBOUNDED
	* LS_STATUS_INFORUNB 
	* LS_STATUS_UNBOUNDED
* LOCALLY_INFEASIBLE
	* LS_STATUS_LOCAL_INFEASIBLE
* OBJECTIVE_LIMIT
	* LS_STATUS_CUTOFF
* OPTIMIZE_NOT_CALLED
	* LS_STATUS_UNLOADED
	* LS_STATUS_LOADED
* OTHER_ERROR
	* LS_STATUS_UNKNOWN

`raw_status(model)` This function will return the Lindo termination status. For more detail on the Lindo termination status see Model Status  table on pages 22-23 in the [manual](https://www.lindo.com/downloads/PDF/API.pdf).

```julia 
julia> JuMP.raw_status(model)
"LS_STATUS_OPTIMAL"
```
`primal_status(model)`  Returns:

* `NO_SOLUTION`
* `FEASIBLE_POINT`
* `INFEASIBLE_POINT`
* `UNKNOWN_RESULT_STATUS`

`dual_status(model)`  Returns

* `NO_SOLUTION`
* `FEASIBLE_POINT`
* `INFEASIBLE_POINT`
* `UNKNOWN_RESULT_STATUS`

If the model is a MIP then `dual_status(model)` will return `NO_SOLUTION` unless the `LS_IPARAM_MIP_DUAL_SOLUTION` flag is set to `1`.

`objective_value(model)`  Returns the objective value of the model.

`dual_objective_value(model)` Returns the dual objective value of the model. Only works for continuous models.

`value(x)` This function takes single variable and returns it value.
 If `x` is an array or matrix use the `.` operator like so `value.(x)` to broadcast the function to each element.

`reduced_cost(x)` Returns the reduced cost of a single variable `x`. If `x` is an array or matrix use the `.` operator like so `reduced_cost.(x)` to broadcast the function to each element.

`dual(constraint_name)` Returns the dual price of a constraint. If the constraint is defined as an array use the `.` operator `dual.(constraint_name)`  to broadcast the function to each element.
This function only works when the model is continuous. 

`get_optimizer_attribute(model,Lindoapi.Slack_or_Surplus())` Returns an array of the slack variables for the model. The order of each element in the array is the order that the constraints are defined as in the model. This function will only work for the Lindoapi and no other solvers. 

```julia
slacks = JuMP.get_optimizer_attribute(model,
			Lindoapi.Slack_or_Surplus()
		   )
portfolio_con_dual  = JuMP.dual(portfolio_con)
return_con_dual     = JuMP.dual(return_con)
portfolio_con_slack = slacks[1]
return_con_slack    = slacks[2]
```


`solution_summary(model)` Returns a struct that can be used to print out a solution summary. 

```julia
julia> solution_summary(model)
* Solver : Lindo

* Status
  Termination status : OPTIMAL
  Primal status      : FEASIBLE_POINT
  Dual status        : FEASIBLE_POINT
  Message from the solver:
  "LS_STATUS_OPTIMAL"

* Candidate solution
  Objective value      : 0.2428012179755081
  Dual objective value : 0.24280121682384317

* Work counters
  Solve time (sec)   : 0.00000
```

`println(model)` Prints out a mathematical representation of the model.

```Julia
julia> println(model)
Max x + z
Subject to
x ≥ 0.0
z ≥ 0.0
(x ^ 2.0 + z ^ 2.0) - 4.0 = 0
```

**Note:** JuMP does not support deleting @NLconstraints or @NLobjectives currently and the model will have to be rebuilt if constraints or objectives need to be edited/deleted. After a model is ran @NLconstraints can be added to an existing model and the model can be rerun.
