"""
Utilities for debugging CCATS.

Common Debug: Summary
_____________________
This file contains functions to:

    1. Perform code analysis with pylint.
    
    2. Debug DataFrames.
    
    3. Debug Pyomo models.

    
Common Debug: Input Files
_________________________

Unique to each function.


Common Debug: Model Functions and Class Methods
_______________________________________________

* :func:`~ccats_common.common_debug.run_pylint` - Runs pylint to analyze code.
* :func:`~ccats_common.common_debug.check_nans` - Checks a DataFrame for NaNs.
* :func:`~ccats_common.common_debug.check_infvalues` - Checks a DataFrame for infinity values.
* :func:`~ccats_common.common_debug.check_index_type` - Checks two DataFrame to see if they are aligned for a .merge() or .update()
* :func:`~ccats_common.common_debug.print_table` - Prints a table for easy debugging.
* :func:`~ccats_common.common_debug.compute_infeasibility_explanation` - used to debug Pyomo models (from Pyomo development team). 


Common Debug: Output Debug Files
________________________________
None


Common Debug: Output Restart Variables
______________________________________
None


Common Debug: Code
__________________
"""


import os
import pandas as pd
import numpy as np
from pylint.lint import Run
import logging
from tabulate import tabulate

### Setup Logging
# Logging needs to be reinstantiated here because the :ref:`common` directory is not part of the :ref:`module` class structure.
for handler in logging.root.handlers[:]:
    logging.root.removeHandler(handler)
logging.basicConfig( level=logging.DEBUG,
                    format='[%(asctime)s][%(name)s]' +
                           '[%(funcName)s][%(levelname)s]  :: |%(message)s|',
                    handlers=[logging.FileHandler("ccats_debug.log"),
                              logging.StreamHandler()])

logger = logging.getLogger('common_debug.py')


def run_pylint(filelist = None, errors_only = False):
    """Runs pylint.

    Parameters
    ----------

    filelist: list
       List of files to evalute, if None, then pylint will evalute the cwd

    errors_only: bool
       Determines if we run pylint against all checks or errors only

    Returns
    -------

    bool
    """
    if filelist is None:
        file_list = [os.getcwd()]
    with open('__init__.py', 'w') as file:
        pass

        for f in file_list:
            try:
                if errors_only:
                    Run([f,'--output=pylint_analysis.txt', '--errors-only'])
                else:
                    Run([f,'--output=pylint_analysis.txt'])
            except:
                pass
    os.remove('__init__.py')


def check_nans(df, tablename='my_table'):
    """Checks table (DataFrame) for nan values.

    Parameters
    ----------
    
    df: DataFrame (2 dimensional)
       Restart variable table

    tablename: string 
        Name for the output file

    Returns
    -------

    bool
    """
    if df.isna().values.any():
        nan_cols = df.columns[df.isna().any()].values.tolist()
        nan_cols_table = df.loc[:, df.isna().any()]
        fileparts = os.path.split(tablename)
        tablename = fileparts[1]
        #print_table(nan_cols_table, tablename)
        logger.warning(f'Dataframe {tablename} has nans in columns: {nan_cols}.')
        #raise Exception(f'Dataframe {tablename} has nans in columns: {nan_cols}')
        return True
    else:
        logger.info('Dataframe has no nans')
    return False


def check_infvalues(df, tablename='my_table'):
    """Checks DataFrame for infinity values.

    Parameters
    ----------
    
    df: DataFrame (2 dimensional)
       Restart variable table.

    tablename: str 
        Name for the output file.

    Returns
    -------

    bool
    """
    if np.isinf(df).values.any():
        inf_cols = df.columns[np.isinf(df).any()].values.tolist()
        inf_cols_table = df.loc[:, np.isinf(df).any()]
        print_table(inf_cols_table, tablename)
        logger.error(f'Dataframe {tablename} has nans in columns: {inf_cols}. printing table to .txt file')
        raise Exception(f'Dataframe {tablename} has nans in columns: {inf_cols}')
    else:
        logger.info('Dataframe has no nans')


def check_index_type(df1, df2):
    """Checks two tables (DataFrames) index types to see if they are aligned for a .merge() or .update().

    * Checks if index type of df1 is equal to index type of df2
    * Logs the result

    Parameters
    ----------

    df1 : DataFrame (2 dimensional)
       Restart variable table.

    df2 : DataFrame (2 dimensional)
       Restart variable table.

    Returns
    -------

    bool
    """

    if df1.index.dtype != df2.index.dtype:
        logger.error('tables have different index types, .update() was unsuccessful')
        return False
    else:
        logger.info('.update() succeeded')
        return True


def print_table(df, outputfilename = None):
    """Prints a table for easy debugging.

    Parameters
    ----------
    
    df: DataFrame (2 dimensional)
        Restart variable table
    
    outputfilename: str
        Name of output file

    Returns
    -------

    None
    """
    if outputfilename: 
        fileparts = os.path.split(outputfilename)
        if fileparts[0]:
            outputfilename = fileparts[0] + r'\table_' + fileparts[1]
        outputfilename = outputfilename.replace('|','_')
        if '.txt' not in outputfilename:
            outputfilename = outputfilename + '.txt'
    else:
        outputfilename = 'table_mytable.txt'
    with open(outputfilename, 'a+') as f:
        if os.path.exists(outputfilename):
            f.write('\n')
        f.write(tabulate(df, headers='keys', tablefmt='psql'))

"""
Minimal Intractable System (MIS) finder
Originall written by Ben Knueven as part of the WaterTAP project:
   https://github.com/watertap-org/watertap
That's why DLW put a huge license notice at the bottom of the file.

copied by DLW 18Feb2024

See: https://www.sce.carleton.ca/faculty/chinneck/docs/CPAIOR07InfeasibilityTutorial.pdf

Edited slightly by DLW, who is on good terms with Ben Knueven,
John Chinneck, and the Regents of the University of California, so surely
none of them will mind!

"""

class _VariableBoundsAsConstraints():
    """PYOMO DEBUG FUNCTION FROM PYOMO DEVELOPMENT TEAM

    Replace all variables bounds and domain information with constraints.
    Leaves fixed Vars untouched (for now)
    """
    import pyomo.environ as pyo
    from pyomo.core.plugins.transform.add_slack_vars import AddSlackVariables
    from pyomo.core.plugins.transform.hierarchy import IsomorphicTransformation
    from pyomo.common.modeling import unique_component_name
    from pyomo.common.collections import ComponentMap, ComponentSet
    from pyomo.opt import WriterFactory
    _default_nl_writer = WriterFactory.get_class("nl")


    def _apply_to(self, instance, **kwds):
        boundconstrblockname = unique_component_name(instance, "_variable_bounds")
        instance.add_component(boundconstrblockname, pyo.Block())
        boundconstrblock = instance.component(boundconstrblockname)
        for v in instance.component_data_objects(pyo.Var, descend_into=True):
            if v.fixed:
                continue
            lb, ub = v.bounds
            if lb is None and ub is None:
                continue
            var_name = v.getname(fully_qualified=True)
            if lb is not None:
                con_name = "lb_for_" + var_name
                con = pyo.Constraint(expr=(lb, v, None))
                boundconstrblock.add_component(con_name, con)
            if ub is not None:
                con_name = "ub_for_" + var_name
                con = pyo.Constraint(expr=(None, v, ub))
                boundconstrblock.add_component(con_name, con)
            # now we deactivate the variable bounds / domain
            v.domain = pyo.Reals
            v.setlb(None)
            v.setub(None)


def compute_infeasibility_explanation(model, solver=None, tee=False, tolerance=1e-8):
    """PYOMO DEBUG FUNCTION FROM PYOMO DEVELOPMENT TEAM

    This function attempts to determine why a given model is infeasible. It deploys
    two main algorithms:

    1. Successfully relaxes the constraints of the problem, and reports to the user
       some sets of constraints and variable bounds, which when relaxed, creates a
       feasible model.
    2. Uses the information collected from (1) to attempt to compute a Minimal
       Infeasible System (MIS), which is a set of constraints and variable bounds
       which appear to be in conflict with each other. It is minimal in the sense
       that removing any single constraint or variable bound would result in a
       feasible subsystem.

    Parameters
    ----------
        model: A pyomo block
        solver (optional): A pyomo solver, a string, or None
        tee (optional):  Display intermediate solves conducted (False)
        tolerance (optional): The feasibility tolerance to use when declaring a
            constraint feasible (1e-08)
    """
    import pyomo.environ as pyo
    from pyomo.core.plugins.transform.add_slack_vars import AddSlackVariables
    from pyomo.core.plugins.transform.hierarchy import IsomorphicTransformation
    from pyomo.common.modeling import unique_component_name
    from pyomo.common.collections import ComponentMap, ComponentSet
    from pyomo.opt import WriterFactory

    _default_nl_writer = WriterFactory.get_class("nl")
    # hold the original harmless
    modified_model = model.clone()

    if solver is None:
        raise ValueError("solver needed unless IDAES is installed")
    elif isinstance(solver, str):
        solver = pyo.SolverFactory(solver)
    else:
        # assume we have a solver
        assert solver.available()

    # first, cache the values we get
    _value_cache = ComponentMap()
    for v in model.component_data_objects(pyo.Var, descend_into=True):
        _value_cache[v] = v.value

    # finding proper reference
    if model.parent_block() is None:
        common_name = ""
    else:
        common_name = model.name + "."

    _modified_model_var_to_original_model_var = ComponentMap()
    _modified_model_value_cache = ComponentMap()

    for v in model.component_data_objects(pyo.Var, descend_into=True):
        modified_model_var = modified_model.find_component(v.name[len(common_name) :])

        _modified_model_var_to_original_model_var[modified_model_var] = v
        _modified_model_value_cache[modified_model_var] = _value_cache[v]
        modified_model_var.set_value(_value_cache[v], skip_validation=True)

    _VariableBoundsAsConstraints().apply_to(modified_model)

    AddSlackVariables().apply_to(modified_model)
    slack_block = modified_model._core_add_slack_variables

    for v in slack_block.component_data_objects(pyo.Var):
        v.fix(0)
    # start with variable bounds -- these are the easist to interpret
    for c in modified_model._variable_bounds.component_data_objects(
        pyo.Constraint, descend_into=True
    ):
        plus = slack_block.component(f"_slack_plus_{c.name}")
        minus = slack_block.component(f"_slack_minus_{c.name}")
        assert not (plus is None and minus is None)
        if plus is not None:
            plus.unfix()
        if minus is not None:
            minus.unfix()

    # Phase 1 -- build the initial set of constraints, or prove feasibility
    msg = ""
    fixed_slacks = ComponentSet()
    elastic_filter = ComponentSet()

    def _constraint_loop(relaxed_things, msg):
        if msg == "":
            msg += f"Model {model.name} may be infeasible. A feasible solution was found with only the following {relaxed_things} relaxed:\n"
        else:
            msg += f"Another feasible solution was found with only the following {relaxed_things} relaxed:\n"
        while True:

            def _constraint_generator():
                elastic_filter_size_initial = len(elastic_filter)
                for v in slack_block.component_data_objects(pyo.Var):
                    if v.value > tolerance:
                        constr = _get_constraint(modified_model, v)
                        yield constr, v.value
                        v.fix(0)
                        fixed_slacks.add(v)
                        elastic_filter.add(constr)
                if len(elastic_filter) == elastic_filter_size_initial:
                    raise Exception(f"Found model {model.name} to be feasible!")

            msg = _get_results_with_value(_constraint_generator(), msg)
            for var, val in _modified_model_value_cache.items():
                var.set_value(val, skip_validation=True)
            results = solver.solve(modified_model, tee=tee)
            if pyo.check_optimal_termination(results):
                msg += f"Another feasible solution was found with only the following {relaxed_things} relaxed:\n"
            else:
                break
        return msg

    results = solver.solve(modified_model, tee=tee)
    if pyo.check_optimal_termination(results):
        msg = _constraint_loop("variable bounds", msg)

    # next, try relaxing the inequality constraints
    for v in slack_block.component_data_objects(pyo.Var):
        c = _get_constraint(modified_model, v)
        if c.equality:
            # equality constraint
            continue
        if v not in fixed_slacks:
            v.unfix()

    results = solver.solve(modified_model, tee=tee)
    if pyo.check_optimal_termination(results):
        msg = _constraint_loop("inequality constraints and/or variable bounds", msg)

    for v in slack_block.component_data_objects(pyo.Var):
        if v not in fixed_slacks:
            v.unfix()

    results = solver.solve(modified_model, tee=tee)
    if pyo.check_optimal_termination(results):
        msg = _constraint_loop(
            "inequality constraints, equality constraints, and/or variable bounds", msg
        )

    if len(elastic_filter) == 0:
        # load the feasible solution into the original model
        for modified_model_var, v in _modified_model_var_to_original_model_var.items():
            v.set_value(modified_model_var.value, skip_validation=True)
        results = solver.solve(model, tee=tee)
        if pyo.check_optimal_termination(results):
            print(f"A feasible solution was found!")
        else:
            print(
                f"Could not find a feasible solution with violated constraints or bounds. This model is likely unstable"
            )

    # Phase 2 -- deletion filter
    WriterFactory.register("nl")(WriterFactory.get_class("nl_v1"))

    # remove slacks by fixing them to 0
    for v in slack_block.component_data_objects(pyo.Var):
        v.fix(0)
    for o in modified_model.component_data_objects(pyo.Objective, descend_into=True):
        o.deactivate()

    # mark all constraints not in the filter as inactive
    for c in modified_model.component_data_objects(pyo.Constraint):
        if c in elastic_filter:
            continue
        else:
            c.deactivate()

    try:
        results = solver.solve(modified_model, tee=tee)
    except:
        results = None

    if pyo.check_optimal_termination(results):
        msg += "Could not determine Minimal Intractable System\n"
    else:
        deletion_filter = []
        guards = []
        for constr in elastic_filter:
            constr.deactivate()
            for var, val in _modified_model_value_cache.items():
                var.set_value(val, skip_validation=True)
            try:
                results = solver.solve(modified_model, tee=tee)
            except:
                math_failure = True
            else:
                math_failure = False

            if math_failure:
                constr.activate()
                guards.append(constr)
            elif pyo.check_optimal_termination(results):
                constr.activate()
                deletion_filter.append(constr)
            else:  # still infeasible without this constraint
                pass

        msg += "Computed Minimal Intractable System (MIS)!\n"
        msg += "Constraints / bounds in MIS:\n"
        msg = _get_results(deletion_filter, msg)
        msg += "Constraints / bounds in guards for stability:"
        msg = _get_results(guards, msg)

    WriterFactory.register("nl")(_default_nl_writer)

    print(msg)


def _get_results_with_value(constr_value_generator, msg=None):
    if msg is None:
        msg = ""
    for c, value in constr_value_generator:
        c_name = c.name
        if "_variable_bounds" in c_name:
            name = c.local_name
            if "lb" in name:
                msg += f"\tlb of var {name[7:]} by {value}\n"
            elif "ub" in name:
                msg += f"\tub of var {name[7:]} by {value}\n"
            else:
                raise RuntimeError("unrecongized var name")
        else:
            msg += f"\tconstraint: {c_name} by {value}\n"
    return msg


def _get_results(constr_generator, msg=None):
    if msg is None:
        msg = ""
    for c in constr_generator:
        c_name = c.name
        if "_variable_bounds" in c_name:
            name = c.local_name
            if "lb" in name:
                msg += f"\tlb of var {name[7:]}\n"
            elif "ub" in name:
                msg += f"\tub of var {name[7:]}\n"
            else:
                raise RuntimeError("unrecongized var name")
        else:
            msg += f"\tconstraint: {c_name}\n"
    return msg


def _get_constraint(modified_model, v):
    if "_slack_plus_" in v.name:
        constr = modified_model.find_component(v.local_name[len("_slack_plus_") :])
        if constr is None:
            raise RuntimeError(
                "Bad constraint name {v.local_name[len('_slack_plus_'):]}"
            )
        return constr
    elif "_slack_minus_" in v.name:
        constr = modified_model.find_component(v.local_name[len("_slack_minus_") :])
        if constr is None:
            raise RuntimeError(
                "Bad constraint name {v.local_name[len('_slack_minus_'):]}"
            )
        return constr
    else:
        raise RuntimeError("Bad var name {v.name}")

"""
WaterTAP Copyright (c) 2020-2023, The Regents of the University of California, through Lawrence Berkeley National Laboratory, Oak Ridge National Laboratory, National Renewable Energy Laboratory, and National Energy Technology Laboratory (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

    Neither the name of the University of California, Lawrence Berkeley National Laboratory, Oak Ridge National Laboratory, National Renewable Energy Laboratory, National Energy Technology Laboratory, U.S. Dept. of Energy nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the features, functionality or performance of the source code ("Enhancements") to anyone; however, if you choose to make your Enhancements available either publicly, or directly to Lawrence Berkeley National Laboratory, without imposing a separate written license agreement for such Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free perpetual license to install, use, modify, prepare derivative works, incorporate into other computer software, distribute, and sublicense such enhancements or derivative works thereof, in binary and source code form.
"""



if __name__ == '__main__':
    logger.info('Running common_debug')
    run_pylint(errors_only=False)
