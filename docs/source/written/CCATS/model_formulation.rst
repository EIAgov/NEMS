Model Formulation
=================

This section describes the CCATS optimization model. Please refer to the :ref:`Glossary <glossary>` for additional details, and to :ref:`Inputs and Methods <section-inputs-methods>` for parameter selection.

Objective Function
------------------

The core objective of the CCATS optimization model is to minimize the total cost associated with and transporting and storing CO₂ from supply sources to sequestration sites. In the model, each of these locations are represented as nodes, :math:`n`, while transportation routes or connections between two nodes are represented as arcs, :math:`a`.  

The model is also designed with multiple time periods to accommodate investment in network expansion. A time period is denoted by :math:`t`.

The objective function :eq:`e_objective`, encompasses investment including CAPEX and fixed operating and maintenance (O&M) costs :eq:`e_sum_costs_investment`, variable operating costs including variable O&M and electricity costs :eq:`e_sum_costs_variable`, and policy-related costs and incentives :eq:`e_sum_costs_policy`. 

.. transportation, :math:`\mathbf{I^T_{a,d,t}}`, and investment in
    \mathbf{I^T_{a,d,t}}, \; 

.. Math::
    \min_{\mathbf{\bar{X}_{a,t}}, \; \mathbf{X_{a,t}}, \; \mathbf{X_{a,p,t}}, \; \mathbf{Q_{a,d,t}^T}, \; \mathbf{I^S_{n,t}}} \quad 
    \sum_{t \in \mathcal{T}} \big( C^{investment}_t + C^{variable}_t + C^{policy}_t \big)
    :label: e_objective

.. Math::
    C^{investment}_t =  \psi^T_t \sum_{a \in \mathcal{A}, \; d \in \mathcal{D}} \big( \theta^T_{a,d,t} \mathbf{Q_{a,d,t}^T} \big)
                       + \psi^S_t \sum_{n \in \mathcal{N}^S} \big( \theta^S_{n,t} \mathbf{I^S_{n,t}} \big)
    :label: e_sum_costs_investment

.. Math::
    C^{variable}_t = 
                      \psi^{variable}_t
                     \bigg( \sum_{a \in \mathcal{A}}  \big( \kappa^T_{a,t} \lambda^T_{a,t} X_{a,t} \big)
                     + \sum_{n \in \mathcal{N}^S} \big( \lambda_{n,t}^S \sum_{a \in \mathcal{A}^{in}_n} X_{a,t} \big) \bigg)
    :label: e_sum_costs_variable

.. Math::
    C^{policy}_t = 
                   \psi^{policy}_t 
                  \sum_{n \in \mathcal{N}^S, \; a \in \mathcal{A}^{in}_n, \; p \in \mathcal{P}} \zeta_{n,p} \mathbf{X_{a,p,t}} 
    :label: e_sum_costs_policy

The model solves for several decision variables, both continous and binary. Decision variables are denoted in bold. First, the model determines the flow of CO₂ in arcs, split into three types for tractability: flow that uses existing capacity, :math:`\mathbf{\bar{X}_{a,t}}`, flow that uses newly constructed capacity, :math:`\mathbf{X_{a,t}}`, and flow that receives different policy incentives :math:`p`, :math:`\mathbf{X_{a,p,t}}`. Policy incentives are denoted by :math:`p`.

Second, the model determines the amount of additional pipeline capacity installed, :math:`\mathbf{Q^T_{a,d,t}}`, indexed by diameter size :math:`d`. Lastly, the model solution includes binary decision variables representing investment in saline storage, :math:`\mathbf{I^S_{n,t}}`. The superscripts :math:`T` and :math:`S` denote transportation and storage, respectively.

The parameters in the objective function represent the various costs by type of node. Parameters denoted by :math:`\psi` represent discount factors which can vary by the type of cost and over time. Parameters denoted by :math:`\theta` represent investment costs, while :math:`\lambda` denotes variable costs. Transportation by pipeline requires electricity and is denoted by electricity demand, :math:`\kappa_{a,t}^T`. Policy incentives are denoted by :math:`\zeta_{n,p}`. 



Constraints
-----------

The model has four groups of constraints: constraints on the volume of flow in each arc, constraints on the balance of CO₂ flowing in and out of the nodes; constraints on transportation investment, and constraints on saline storage investment. 

Arc Flow Constraints
~~~~~~~~~~~~~~~~~~~~

.. Math::
    \mathbf{\bar{X}_{a,t}} \le  \rho_{a,t}
    , \quad \forall \; a \in \mathcal{A}, \; t \in \mathcal{T}
    :label: c_flow_existing

.. Math::
    \mathbf{X_{a,t}} \le \sum_{d \in \mathcal{D},t^{\star}<=t-1} \mathbf{Q_{a,d,t^\star}^T}
    , \quad \forall \; a \in \mathcal{A}, \; t \in \mathcal{T}
    :label: c_flow_added

.. Math::
    \mathbf{Q_{a,d,t}^T} \le  \sigma^{max}_{a,d}
	, \quad \forall \; a \in \mathcal{A}, \; d \in \mathcal{D}, \; t \in \mathcal{T}
    :label: c_transport_capacity_added_upper_bound

.. Math::
    X_{a,t} = \mathbf{\bar{X}_{a,t}} + \mathbf{X_{a,t}}
    , \quad \forall \, a \in \mathcal{A}, \, t \in \mathcal{T}
    :label: c_flow_total

.. Math::
    X_{a,t} = \sum_{p \in \mathcal{P}} \mathbf{X_{a,p,t}}
    , \quad \forall \, a \in \mathcal{A}, \, t \in \mathcal{T}
    :label: c_flow_by_policy

In Equation :eq:`c_flow_existing`, flow that uses existing pipeline capacity as of the first time period, :math:`\mathbf{\bar{X}_{a,t}}`, is limited by the existing capacity parameter, :math:`\rho_{a,t}`. This parameter is taken from the data in the early projection years and is determined by the model in later projection years as pipeline capacity is built in NEMS endogenously. Please refer to :ref:`this section <NEMS>` for additional details. 

In Equation :eq:`c_flow_added`, flow that uses newly constructed capacity, :math:`\mathbf{X_{a,t}}`, is limited by the total capacity that has been installed in all previous time periods and is available for use in the current time period. This newly constructed capacity is a continuous decision variable that is in turn limited by the parameter :math:`\sigma^{max}_{a,d}` in Equation :eq:`c_transport_capacity_added_upper_bound`. This parameter represents the maximum volume that can be installed based on characteristics of available pipelines in the market, particularly diameter.

Constraints :eq:`c_flow_added` and :eq:`c_transport_capacity_added_upper_bound` are formulated using the big M method to limit the number of binary decision variables in the model. Otherwise, the optimization problem where a binary investment decision is multiplied by build capacity or flow, which are both decision variables themselves, is a non-linear problem. To avoid this, the investment decision variable, :math:`\mathbf{Q^T_{a,d,t}}`, is instead defined as a continuous variable. When this variable is zero, meaning no investment is undertaken by the model, the constraints force both capacity and flow to be zero as well.

Equation :eq:`c_flow_total` defines the secondary decision variable, :math:`X_{a,t}`, as the sum of the two flow types for ease of notation. Equation :eq:`c_flow_by_policy` constrains this total to be the sum of CO₂ flow across policies.


Node Flow Balance Constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next set of constraints define the net supply or demand of CO₂ at nodes and ensure that the sum of flows into a node and the sum of flows out of the node must balance out. The subsets of arcs with flow going into and out of a node is denoted by :math:`\mathcal{A}_{n}^{in}` and :math:`\mathcal{A}_{n}^{out}`, respectively. 

.. Math::
    \sum_{a \in \mathcal{A}_{n}^{in}}  X_{a,p,t} - \sum_{a \in \mathcal{A}_{n}^{out}} X_{a,p,t} = -\phi_{n,p,t}^C  
    , \quad \forall \; n \in \mathcal{N}^C, \; t \in \mathcal{T}, \; p \in \mathcal{P}
    :label: c_flow_balance_supply

.. Math::
    \sum_{a \in \mathcal{A}_{n}^{in}}  X_{a,p,t} - \sum_{a \in \mathcal{A}_{n}^{out}} X_{a,p,t} = 0
    , \quad \forall \; n \in \mathcal{N}^{TS}, \; t \in \mathcal{T}, \; p \in \mathcal{P}
    :label: c_flow_balance_transshipment

.. Math::
    \sum_{a \in \mathcal{A}_{n}^{in}}  X_{a,t} - \sum_{a \in \mathcal{A}_{n}^{out}} X_{a,t} \le J_{n,t}
    , \quad \forall \; n \in \mathcal{N}^{S,saline}, \; t \in \mathcal{T}
    :label: c_flow_balance_demand_storage

.. Math::
    \sum_{a \in \mathcal{A}_{n}^{in}}  X_{a,t} - \sum_{a \in \mathcal{A}_{n}^{out}} X_{a,t} \le \phi_{n,t}^{S,EOR}
    , \quad \forall \; n \in \mathcal{N}^{S,EOR}, \; t \in \mathcal{T}
    :label: c_flow_balance_demand_eor

Equation :eq:`c_flow_balance_supply` sets the CO₂ flow balance for capture nodes equal to :math:`-\phi^C_{n,p,t}`. This parameter represents the supply of captured CO₂ and is determined by other NEMS modules. Note that this constraint is an equality, so that CCATS must allocate and transport all captured CO₂ received from other NEMS modules.

Equation :eq:`c_flow_balance_transshipment` sets the balance at transshipment points to zero, so that these nodes cannot be used as temporary storage locations. 

Equation :eq:`c_flow_balance_demand_storage` limits the flow balance in saline storage nodes by the injection parameter :math:`J_{n,t}`. It is defined as an inequality because the storage capacity in geologic formations in the U.S. is an order of magnitude higher than the CO₂ emissions being produced in the data. This parameter determined by the model in later projection years as storage capacity is built in NEMS endogenously. It is further discussed in the section on :ref:`section_saline_storage`. 

Equation :eq:`c_flow_balance_demand_eor` limits the flow balance to the EOR parameter :math:`\phi_{n,t}^{S,EOR}`. This parameter is determined by HSM. Similar to the constraint on saline storage, it is also defined as an inequality, so the model does not force captured CO₂ to be directed to EOR unless it is economical. As such, there may be EOR demand that is unfulfilled in the model.

Supply of CO₂ from NEMS modules will be specified by policy elgibility, thus the need to index by :math:`p` in Equations :eq:`c_flow_balance_supply`. Consequently, the flow balance in the transshipment network also needs to be tracked by policy in Equation :eq:`c_flow_balance_transshipment`. However, storage and EOR nodes can accept flow from any policy, thus Equations :eq:`c_flow_balance_demand_storage` and :eq:`c_flow_balance_demand_eor` are not indexed by :math:`p`.


.. _section-saline-storage:

Saline Storage Investment Constraints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CCATS represents two aspects of storage: the speed at which CO₂ flow can be stored and the total volume of CO₂ that can be stored.

.. Math::
    J_{n,t} = \alpha_n + \beta_{n} \sum_{0 \le t^\star \le t-1} \mathbf{I^S_{n,t^\star}}
    , \quad \forall \; n \in {\mathcal{N}^{S,saline}}, \; t \in \mathcal{T}
    :label: c_storage_injectivity_rule

.. Math::
    \sum_{a \in \mathcal{A}^{in}_n,0 \le t^\star \le t} \tau_t^\star \; X_{a,t^\star}  \le \gamma_n + \delta_n \sum_{0 \le t^\star \le t-1} \mathbf{I^S_{n,t^\star}} 
    , \quad \forall \; n \in \mathcal{N}^{S,saline} , \; t \in \mathcal{T} 
    :label: c_storage_cumulative_injection

.. Math::
    \sum_{t \in \mathcal{T}} \mathbf{I^S_{n,t}} \le \epsilon_n
    , \quad \forall \; n \in \mathcal{N}^{S,saline} 
    :label: c_storage_aors_available

Injectivity, or the rate at which CO₂ can be injected, is defined in Equation :eq:`c_storage_injectivity_rule` and is based on the investment decision in a saline storage node. The parameters :math:`\alpha_n` and :math:`\beta_n` are based on external studies and data.  

Cumulative injection, or the total amount of CO₂ that can be injected over all time periods, is defined in Equation :eq:`c_storage_cumulative_injection`. The parameters :math:`\gamma_n` and :math:`\delta_n` are also based on external studies and data. 

Lastly, a storage node is broken down into multiple Areas of Review (AOR) to represent incremental development of a storage location. Equation :eq:`c_storage_aors_available` limits the total number of AORs that can be built at a given node.



Non-Linear MILP Model
---------------------

The default specification of CCATS is a linear program to save on runtime. However, the model can also be run as a mixed-integer linear program (MILP). In the MILP specification, investment in pipeline capacity is a piecewise-linear function instead of a simple linear function. The following equations are different from the ones described above.

.. Math::
    \min_{\mathbf{\bar{X}_{a,t}}, \; \mathbf{X_{a,t}}, \; \mathbf{X_{a,p,t}}, \; \mathbf{Q_{a,d,t}^T}, \; \mathbf{I^T_{a,d,t}} \; \mathbf{I^S_{n,t}}} \quad 
    \sum_{t \in \mathcal{T}} \big( C^{investment}_t + C^{variable}_t + C^{policy}_t \big)
    :label: e_objective_nonlinear

.. Math::
    C^{investment}_t =  \psi^T_t \sum_{a \in \mathcal{A}, \; d \in \mathcal{D}} \big(  \eta^T_{a,d,t} \mathbf{I^T_{a,d,t}} + \theta^T_{a,d,t} \mathbf{Q_{a,d,t}^T} \big)
                        + \psi^S_t \sum_{n \in \mathcal{N}^S} \big( \theta^S_{n,t} \mathbf{I^S_{n,t}} \big)
    :label: e_sum_costs_investment_nonlinear

.. Math::
    \sum_{d \in \mathcal{D}} \mathbf{I^T_{a,d,t}} \le 1
    , \quad
    \forall \; a \in \mathcal{A}, \; t \in \mathcal{T}
    :label: c_transport_selection_nonlinear

.. Math::
    \sigma^{min}_{a,d} \mathbf{I^T_{a,d,t}} \le \mathbf{Q_{a,d,t}^T}
    , \quad
    \forall \; a \in \mathcal{A}, \; d \in \mathcal{D}, \; t \in \mathcal{T}
    :label: c_transport_capacity_added_lower_bound_nonlinear

.. Math::
    \mathbf{Q_{a,d,t}^T} \le  \sigma^{max}_{a,d} \mathbf{I^T_{a,d,t}}
    , \quad
    \forall \; a \in \mathcal{A}, \; d \in \mathcal{D}, \; t \in \mathcal{T}
    :label: c_transport_capacity_added_upper_bound_nonlinear

In the objective function, Equation :eq:`e_objective` becomes Equation :eq:`e_objective_nonlinear` with an additional binary decision variable :math:`\mathbf{I^T_{a,d,t}}`. 

Equation :eq:`e_sum_costs_investment_nonlinear` replaces the calculation for investment costs in Equation :eq:`e_sum_costs_investment` and includes the piecewise linear function. The coefficient :math:`\eta^T_{a,d,t}` is as an intercept parameter for each of the segments of the piecewise linear function, while :math:`\theta^T_{a,d,t}` is a slope parameter for each additional unit of capacity invested in within that segment. 

Equation :eq:`c_transport_selection_nonlinear` ensures that each arc can only build once per time period, and must be in one of the piecewise linear segments. Equation :eq:`c_transport_capacity_added_lower_bound_nonlinear` and Equation :eq:`c_transport_capacity_added_upper_bound_nonlinear` enforce the lower and upper bounds per segment of the pipeline piecewise linear function.
    



Price of Carbon Dioxide
------------------------

One of the main results of the CCATS optimization model is the price CO₂. Price is an important driver used by other NEMS modules to determine their decision to capture CO₂ and whether to install capture technology. 

There are two types of flows in CCATS: flows that are 45Q eligible and flows that are not 45Q eligible. CCATS distinguishes between the two types to enable the model to return two separate prices, one for each 45Q eligibility. This is important, because policy incentives can be a big driver of the decision to provide captured CO₂ to the model. 

The price CO₂ comes the duals from the constraints in Equation :eq:`c_flow_balance_supply`. These equations represent the point at which CCATS receives CO₂ from other NEMS modules, and equates supply to demand. In the optimization, the dual variables reflect the change in the objective function for each unit change in the constraint. Economically, it reflects the present value of the marginal cost to transport, store, and receive policy incentives for an additional unit of captured CO₂.

CCATS uses a highly detailed representation of CO₂ supply facilities, which is more granular than what can be used by other NEMS modules. The constraints need to be aggregated to a census region or census division level to be compatible with NEMS. As such, CCATS calculates the volume-weighted average of the duals from the constraints in a post-processor to aggregate at both the census region and census division levels . In addition, CCATS returns the CO₂ price for the first year of the optimization which aligns with the year that the NEMS model is being run for a particular cycle or iteration. 


.. _NEMS:

Relationship between CCATS and NEMS
-----------------------------------

Within a NEMS run, the CCATS model is executed each model year, iteration, and cycle. For instance, when NEMS runs for the 2025 model year, CCATS optimizes with 2025 as the initial period.

To reduce runtime, CCATS simplifies the temporal resolution of the investment horizon and limits the number of time periods to three. The first time period represents the current NEMS year, the second time period the following year, and the third time represents a longer time horizon to inform long-term decisions. Capacity can be added during any period, but will not available for use until the following time period. CCATS projects operation and capacity expansion of carbon transport and storage over this time horizon.

.. csv-table:: CCATS Time Periods and Capacity Expansion Assumptions
   :header: "Time Period", "Duration (years)", "Capacity Expansion"
   :widths: 8, 8, 8
   :name: table-model-assumptions-time-period 

   "1","1","Yes"
   "2","1","Yes"
   "3","18","Yes"

.. rubric:: Source: U.S. Energy Information Administration.

Although the model produces solutions for three time periods, only the solution for the first time period is returned to NEMS. For example, a NEMS run for the model year 2025 uses output from the first time period of a CCATS model. Subsequently, when NEMS advances to the 2026 model year, CCATS re-optimizes, now with 2026 as the initial period. 

In addition, model parameters are derived from two sources: outputs from other NEMS modules, enabling feedback effects, and expert studies for calibrating remaining parameters. For example, the model also uses NEMS results from 2025 as input parameters for 2026 to allow the model to build capacity over time. 


Modeling in Pyomo
-----------------

The formulation is implemented in Pyomo using the :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_objective` and :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_objective_across_blocks` methods within the OptimizationModel class.

Constraints are set in the optimization program by :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_constraints` and :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_constraints_across_blocks`.

Additional documentation for the source code of the CCATS module can be found found in the Model API Reference Section.

.. _glossary:

Glossary
--------


Superscripts
~~~~~~~~~~~~

Superscripts identify the main concepts within CCATS, indicating the relevance of sets, parameters, and variables to those concepts. For clarity and tractability, the number of superscripts is limited. Superscripts are displayed in regular font and listed in :numref:`Table %s <table-superscripts>`.

.. _table-superscripts:

.. csv-table:: Superscripts.
   :header: "Superscript", "Short Description"
   :widths: 8, 30
   :name: csv-table-superscripts

   ":math:`C`","Capture"
   ":math:`A`","Allocation"
   ":math:`T`","Transport"
   ":math:`TS`","Transshipment"
   ":math:`S`","Storage"
   ":math:`S,EOR`","Storage - Enhanced Oil Recovery"
   ":math:`S,saline`","Storage - Saline aquifer"
   ":math:`in`","In"
   ":math:`out`","Out"


Sets and subsets
~~~~~~~~~~~~~~~~

Sets and subsets group model objects with shared characteristics, such as nodes and arcs, enabling efficient application of consistent calculations. Parameters and variables are indexed by these sets and subsets and are displayed in calligraphic font.

Sets and subsets are listed in :numref:`Table %s <table-sets-subsets>`. Subsets, denoted by a superscript on the parent set, are listed below their corresponding set. For example, :math:`t` represents a time period within the set :math:`\mathcal{T}`.

In the code, sets for the optimization program are defined using the :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_sets` method within the OptimizationModel class.

.. _table-sets-subsets:

.. csv-table:: Sets and subsets.
   :header: "Set", "Subset", "Element", "CCATS Name", "Short Description", "Detailed Description"
   :widths: 8, 8, 8, 28, 50, 14
   :name: csv-table-sets-subsets

   ":math:`\mathcal{A}`","",":math:`a`",":code:`s_arcs`","Arcs, connections between nodes",":ref:`Details<s_arcs>`"
   "",":math:`\mathcal{A}^{in}`",":math:`a`",":code:`arcs_in`","Arcs transporting flow into a node",":ref:`Details<arcs_in>`"
   "",":math:`\mathcal{A}^{out}`",":math:`a`",":code:`arcs_out`","Arcs transporting flow away from a node",":ref:`Details<arcs_out>`"
   ":math:`\mathcal{D}`","",":math:`d`",":code:`s_transport_options`","Pipeline segment options",":ref:`Details<s_transport_options>`"
   ":math:`\mathcal{N}`","",":math:`n`",":code:`s_nodes`","Nodes",":ref:`Details<s_nodes>`"
   "",":math:`\mathcal{N}^C`",":math:`n`",":code:`s_nodes_supply`","Capture nodes",":ref:`Details<s_nodes_supply>`"
   "",":math:`\mathcal{N}^{TS}`",":math:`n`",":code:`s_nodes_transshipment`","Transport (transshipment) nodes",":ref:`Details<s_nodes_transshipment>`"
   "",":math:`\mathcal{N}^S`",":math:`n`",":code:`s_nodes_demand`","Saline storage or EOR nodes",":ref:`Details<s_nodes_demand>`"
   "",":math:`\mathcal{N}^{S,saline}`",":math:`n`",":code:`s_nodes_demand_storage`","Saline storage nodes",":ref:`Details<s_nodes_demand_storage>`"
   "",":math:`\mathcal{N}^{S,EOR}`",":math:`n`",":code:`s_nodes_demand_eor`","EOR nodes",":ref:`Details<s_nodes_demand_eor>`"
   ":math:`\mathcal{P}`","",":math:`p`",":code:`s_policy_options`","CO₂ Policies (for example, 45Q)",":ref:`Details<s_policy_options>`"
   ":math:`\mathcal{T}`","",":math:`t`",":code:`s_time`","Time",":ref:`Details<s_time>`"

Data Source: U.S. Energy Information Administration


.. _s_arcs:

Arcs
""""

:math:`\mathcal{A}` is the set of all arcs :math:`a`.
Arcs represent connections between nodes.
Arcs have directionality, therefore a single arc allows flow in one direction.
Supply nodes only have the option to build arcs that transport flow away from the supply.
Saline storage and EOR nodes only have the option to build arcs that transport flow to storage/EOR.
Transshipment nodes have the option to build flow in either direction between nodes.
Arcs options are defined in the offline preprocessor.
In the optimization model, arcs are indexed by 1) the name (string) of the starting node, and 2) the name (string) of the ending node.


.. _arcs_in:

Arcs in
"""""""

:math:`\mathcal{A}^{in}` is a subset of :math:`\mathcal{A}`.
:math:`\mathcal{A}^{in}` represents the arcs with flow coming into a node.
This subset is used to support the balance of flows coming into a node against the flows leaving a node.
:math:`\mathcal{A}^{in}` is determined in the online preprocessor.


.. _arcs_out:

Arcs out
""""""""

:math:`\mathcal{A}^{out}` is a subset of :math:`\mathcal{A}`.
:math:`\mathcal{A}^{out}` represents the arcs with flow exiting a node.
This subset is used to support the balance of flows coming into a node against the flows leaving a node.
:math:`\mathcal{A}^{out}` is determined in the online preprocessor.


.. _s_transport_options:

Pipeline options
""""""""""""""""

:math:`\mathcal{D}` is the set of all pipeline options :math:`d`.
When CCATS is run as a linear program, then each arc only has one pipeline option.
When CCATS is run as a mixed integer linear program, it uses a piecewise linear function to select between different pipeline options.
The different pipeline options are designed to represent possible combinations of pipeline diameters and number of pumps.
Each segment of the piecewise linear is a pipeline option :math:`d`.
:math:`\mathcal{D}` is determined in the offline preprocessor.
Pipeline options are indexed by their name (string).


.. _s_nodes:

Nodes
"""""

:math:`\mathcal{N}` is the set of nodes :math:`n`. 
Nodes represent locations where pipelines (arcs) either start or end.
Nodes include key locations including suppliers of CO₂, EOR demand sites, and saline storage.
Nodes are indexed by their name (string).


.. _s_nodes_supply:

Supply nodes
""""""""""""

:math:`\mathcal{N}^C` is the subset of nodes where CO₂ capture occurs, thus supplying CO₂ to the network.


.. _s_nodes_transshipment:

Transshipment nodes
"""""""""""""""""""

:math:`\mathcal{N}^{TS}` is the subset of nodes where two pipelines join, known as a transshipment node.


.. _s_nodes_demand:

Storage nodes
"""""""""""""

:math:`\mathcal{N}^{S}` is the subset of nodes where CO₂ is stored either in EOR or saline storage.


.. _s_nodes_demand_storage:

Saline storage nodes
""""""""""""""""""""

:math:`\mathcal{N}^{S,saline}` is the subset of nodes :math:`\mathcal{N}^{S}` where CO₂ is stored in saline storage.


.. _s_nodes_demand_eor:

EOR nodes
"""""""""

:math:`\mathcal{N}^{S,EOR}` is the subset of nodes :math:`\mathcal{N}^{S}` where CO₂ used for Enhanced Oil Recovery (EOR).


.. _s_policy_options:

Policy options
""""""""""""""

:math:`\mathcal{P}` is the set of policy options :math:`p`. 
This includes flow that is not eligible for policy incentives and flow that is eligible for 45Q.
CCATS does not determine the 45Q eligibility of flow, rather that is provided as an input by NEMS modules supplying CO₂.
Policy options are indexed by their name (string).


.. _s_time:

Time periods
""""""""""""

:math:`\mathcal{T}` is the set of time periods :math:`t`. 
CCATS operates with three time periods, which are implemented in Pyomo using a block structure.
The first time period represents the current year being analyzed by NEMS.
The second and third time period are used to represent the future to support investment decisions.
Time periods are indexed by their number (integer).


Parameters
~~~~~~~~~~

Parameters serve as inputs to the CCATS optimization program and are generally represented using lowercase Greek letters. These parameters are sourced either endogenously from other NEMS modules or exogenously from the offline CCATS preprocessor. We use the notation :math:`\mathbb{R}` to denote the set of real numbers and :math:`\mathbb{R}^+_0` to denote the set of non-negative real numbers. Parameters are defined for the optimization program using the :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_parameters` method within the OptimizationModel class. A complete list of parameters is provided in :numref:`Table %s <table-parameters>`.

.. csv-table:: Parameters.
   :header: "Parameter", "CCATS Name", "Data Type", "Short Description", "Source", "Units", "Detailed Description"
   :widths: 8, 28, 14, 50, 14, 8, 14
   :name: table-parameters

   "**Storage**"
   ":math:`\alpha_n`",":code:`p_co2_demand_storage`",":math:`\mathbb{R}^+_0`","Existing injectivity", "Exogenous", ":math:`t\,CO_2/year`",":ref:`Details<p_co2_demand_storage>`"
   ":math:`\beta_n`",":code:`p_storage_inj_capacity_adder`",":math:`\mathbb{R}^+_0`","Additional injectivity per AoR", "Exogenous", ":math:`t\,CO_2/year`",":ref:`Details<p_storage_inj_capacity_adder>`"
   ":math:`\gamma_n`",":code:`p_storage_injection_net_remaining`",":math:`\mathbb{R}^+_0`","Existing net capacity", "Exogenous", ":math:`t\,CO_2`",":ref:`Details<p_storage_injection_net_remaining>`"
   ":math:`\delta_n`",":code:`p_storage_injection_adder`",":math:`\mathbb{R}^+_0`","Additional capacity per AoR", "Exogenous", ":math:`t\,CO_2`",":ref:`Details<p_storage_injection_adder>`"
   ":math:`\epsilon_n`",":code:`p_storage_aors_available`",":math:`\mathbb{R}^+_0`","Number of AoRs available to open", "Exogenous", ":math:`t\,CO_2`",":ref:`Details<p_storage_aors_available>`"
   "**Policy**"
   ":math:`\zeta_{n,p,t}`",":code:`p_policy_45Q`",":math:`\mathbb{R}`","Policy cost (+) or incentive (-)", "Exogenous", ":math:`$/t\,CO_2`",":ref:`Details<p_policy_45Q>`"
   "**Costs**"
   ":math:`\eta^T_{a,d,t}`",":code:`p_capex_transport_0`",":math:`\mathbb{R}^+_0`","Transport CAPEX - Intercept", "Exogenous", ":math:`$`",":ref:`Details<p_capex_transport_0>`"
   ":math:`\theta^T_{a,d,t}`",":code:`p_capex_transport_slope`",":math:`\mathbb{R}^+_0`","Transport CAPEX", "Exogenous", ":math:`$/t\,CO_2`",":ref:`Details<p_capex_transport_slope>`"
   ":math:`\theta^S_{n,t}`",":code:`p_capex_storage`",":math:`\mathbb{R}^+_0`","Storage CAPEX", "Exogenous", ":math:`$/t\,CO_2`",":ref:`Details<p_capex_storage>`"
   ":math:`\kappa^T_{a,t}`",":code:`p_electricity_demand`",":math:`\mathbb{R}^+_0`","Transport electricity consumption", "Exogenous", ":math:`MWh/t\,CO_2`",":ref:`Details<p_electricity_demand>`"
   ":math:`\lambda^T_{a,t}`",":code:`p_opex_transport`",":math:`\mathbb{R}^+_0`","Transport electricity cost", "Exogenous", ":math:`$/MWh`",":ref:`Details<p_opex_transport>`"
   ":math:`\lambda^S_{n,t}`",":code:`p_opex_storage`",":math:`\mathbb{R}^+_0`","Storage OPEX", "Exogenous", ":math:`$/t\,CO_2`",":ref:`Details<p_opex_storage>`"
   "**Transport**"
   ":math:`\rho_a`",":code:`p_transport_capacity_existing`",":math:`\mathbb{R}^+_0`","Exisiting capacity", "Exogenous", ":math:`t\,CO_2/year`",":ref:`Details<p_transport_capacity_existing>`"
   ":math:`\sigma_{a,d}`",":code:`p_transport_capacity_adder`",":math:`\mathbb{R}^+_0`","Maximum capacity per new build", "Exogenous", ":math:`t\,CO_2/year`",":ref:`Details<p_transport_capacity_adder>`"
   "**Net Supply**"
   ":math:`\phi^C_{n,p}`",":code:`p_co2_supply`",":math:`\mathbb{R}^+_0`","CO₂ captured at source", "Endogenous", ":math:`t\,CO_2/year`",":ref:`Details<p_co2_supply>`"
   ":math:`\phi^{S,EOR}_n`",":code:`p_co2_demand_eor`",":math:`\mathbb{R}^+_0`","CO₂ demand for EOR", "Endogenous", ":math:`t\,CO_2/year`",":ref:`Details<p_co2_demand_eor>`"
   "**Financing/discounting and time**"
   ":math:`\tau_t`",":code:`p_duration`",":math:`\mathbb{R}^+_0`","Duration of time period", "Exogenous", ":math:`years`",":ref:`Details<p_duration>`"
   ":math:`\psi^{T}_t`",":code:`p_discount_invest_storage`",":math:`\mathbb{R}^+_0`","Discount factor for storage investment", "Endogenous", ":math:`-`",":ref:`Details<p_discount_invest_storage>`"
   ":math:`\psi^{S}_t`",":code:`p_discount_invest_transport`",":math:`\mathbb{R}^+_0`","Discount factor for transport investment", "Endogenous", ":math:`-`",":ref:`Details<p_discount_invest_transport>`"
   ":math:`\psi^{variable}_t`",":code:`p_discount_variable`",":math:`\mathbb{R}^+_0`","Discount factor for variable costs", "Endogenous", ":math:`-`",":ref:`Details<p_discount_variable>`"
   ":math:`\psi^{policy}_t`",":code:`p_discount_policy`",":math:`\mathbb{R}^+_0`","Discount factor for policy costs", "Endogenous", ":math:`-`",":ref:`Details<p_discount_policy>`"


.. _p_co2_demand_storage:

Existing injectivity
""""""""""""""""""""

:math:`\alpha_n` is the injectivity (amount of CO₂ that can be injected per year) at node :math:`n` before the current model year.


.. _p_storage_inj_capacity_adder:

Additional injectivity
""""""""""""""""""""""

:math:`\beta_n` is the additional injectivity that is added to node :math:`n` per additional AoR opened.


.. _p_storage_injection_net_remaining:

Existing net capacity
"""""""""""""""""""""

:math:`\gamma_n` is the injection capacity remaining at node :math:`n` before the current model year.


.. _p_storage_injection_adder:

Additional capacity
"""""""""""""""""""

:math:`\delta_n` is the additional injection capaicty added to node :math:`n` per additional AoR opened.


.. _p_storage_aors_available:

Available AoRs
""""""""""""""

:math:`\epsilon_n` is the number of AoRs available to open at node :math:`n`.


.. _p_policy_45Q:

Policy cost
"""""""""""

:math:`\zeta_{n,p,t}` is the cost of policy :math:`p`. Incentives such as 45Q are provided as a negative cost.


.. _p_capex_transport_0:

Transport investment - intercept
""""""""""""""""""""""""""""""""

:math:`\eta^T_{a,d,t}` is the intercept for transport investment costs.
This is only used in the non-linear version of CCATS.


.. _p_capex_transport_slope:

Transport investment - slope
""""""""""""""""""""""""""""

:math:`\theta^T_{a,d,t}` is the slope of transport investment costs.


.. _p_capex_storage:

Storage investment
""""""""""""""""""

:math:`\theta^S_{n,t}` is the investment cost of storage.


.. _p_electricity_demand:

Transport electricity consumption
"""""""""""""""""""""""""""""""""

:math:`\kappa^T_{a,t}` is the amount of electricity consumed to transport a tonne of CO₂.


.. _p_opex_transport:

Transport electricity cost
""""""""""""""""""""""""""

:math:`\lambda^T_{a,t}` is the cost per unit of electricity consumed for arc :math:`a`.


.. _p_opex_storage:

Storage variable cost
"""""""""""""""""""""

:math:`\lambda^S_{n,t}` is the variable cost of storage.


.. _p_transport_capacity_existing:

Existing transport capacity
"""""""""""""""""""""""""""

:math:`\rho_a` is the capacity of transport arcs before the current model year.


.. _p_transport_capacity_adder:

Transport capacity adder
""""""""""""""""""""""""

:math:`\sigma_{a,d}` is the limit for adding capacity to an arc for the current pipeline option :math:`d`.


.. _p_co2_supply:

CO₂ supply
"""""""""""""""""""

:math:`\phi^C_{n,p}` is the amount of CO₂ supplied at node :math:`n` and of policy :math:`p`.


.. _p_co2_demand_eor:

EOR demand
""""""""""

:math:`\phi^{S,EOR}_n` is the maximum CO₂ demand for EOR at node :math:`n`.


.. _p_duration:

Duration
""""""""

:math:`\tau_t` is the duration of time period :math:`t`.


.. _p_discount_invest_storage:

Transport discount factor
"""""""""""""""""""""""""

:math:`\psi^{T}_t` is a multiplier to finance and discount payments for transportation investments made in time period :math:`t`. It is calculated by :meth:`~ccats_financial.CCATS_Finance.calculate_discount_investment`.

.. Math::
    \psi^T_t = \bigg(\frac{RINT_t(1+RINT_t)^{n}}{(1+RINT_t)^{n}-1} * \sum_{y=y_t+1}^{y_t+1+n} DIS_{y_0,y} \bigg) + \bigg( FOM * \sum_{y=y_t+1}^{y_t+1+n} INF_{y_t,y} * DIS_{y_0,y} \bigg)

with the discount factor, :math:`DIS`, and inflation factor, :math:`INF`, from year :math:`y_0` to :math:`y_1` defined as:

.. Math::
    DIS_{y_0,y_1} =\begin{cases}
                   1 && \forall \; y_1-y_0=0 \\
                   \prod_{y^{\star}=y_0}^{y_1} \frac{1}{(1+RDIS_{y^{\star}})} && \forall \; y_1-y_0>0
                   \end{cases}

.. Math::
    INF_{y_0,y_1} = \begin{cases}
                    1 && \forall \; y_1-y_0=0 \\
                    \prod_{y^{\star}=y_0}^{y_1} ( 1+RINF_{y^{\star}} ) && \forall \; y_1-y_0>0
                    \end{cases}


The discount and inflation factor is summed starting at :math:`y_t+1` because the first payment is assumed to occur one year after the investment decision.

These equations rely on eight inputs:

* time inputs:

  * :math:`t` is the time period of the investment decision,
  * :math:`y_0` is the NEMS year that time period 0 begins,
  * :math:`y_t` is the NEMS year that time period :math:`t` begins,
  * :math:`n` are the number of years that CAPEX is financed for transport and storage, respectively,

* financing inputs:

  * :math:`FOM` is the fraction of CAPEX paid each year as Fixed O&M for transport and storage, respectively,
  * :math:`RINF_y` is the inflation rate of year y,
  * :math:`RDIS_y` is the discount rate of year y equal to the real rate + inflation rate,
  * :math:`RINT_t` is the interest rate used for borrowing in time period :math:`t`, equal to the real rate + inflation rate + risk premia.


.. _p_discount_invest_transport:

Storage discount factor
"""""""""""""""""""""""

:math:`\psi^{S}_t` is a multiplier to finance and discount payments for storage investments made in time period :math:`t`. It is calculated by :meth:`~ccats_financial.CCATS_Finance.calculate_discount_investment`.

.. Math::
    \psi^S_t = \bigg(\frac{RINT_t(1+RINT_t)^{n}}{(1+RINT_t)^{n}-1} * \sum_{y=y_t+1}^{y_t+1+n} DIS_{y_0,y} \bigg) + \bigg( FOM * \sum_{y=y_t+1}^{y_t+1+n} INF_{y_t,y} * DIS_{y_0,y} \bigg)


.. _p_discount_variable:

Variable cost discount factor
"""""""""""""""""""""""""""""

:math:`\psi^{variable}_t` is a multiplier to discount variable costs occurring in time period :math:`t`. Variable costs are input for a single year, so :math:`\psi^{variable}_t` also accounts for repeated costs in multi year time periods. It is calculated by :meth:`~ccats_financial.CCATS_Finance.calculate_discount_variable_policy`.

.. Math::
    \psi^{variable}_t = \sum_{y=y_t}^{y_t+\Delta_t} \bigg( DIS_{y_0,y}\bigg)

where :math:`\Delta_t` is the duration of the time period :math:`t`.


.. _p_discount_policy:

Policy cost discount factor
"""""""""""""""""""""""""""

:math:`\psi^{policy}_t` is a multiplier to discount policy costs occurring in time period :math:`t`.
Policy costs are input for a single year, so :math:`\psi^{policy}_t` also accounts for repeated costs in multi year time periods. It is calculated by :meth:`~ccats_financial.CCATS_Finance.calculate_discount_variable_policy`.

.. Math::
    \psi^{policy}_t = \sum_{y=y_t}^{y_t+\Delta_t} \bigg( DIS_{y_0,y}\bigg)


Variables
~~~~~~~~~

Unknowns (decisions) to be solved by the mathematical program. 
They are split into primary and secondary decision variables.
All variables use the uppercase Roman alphabet, with subscripts in lowercase.
Primary decision variables are shown in **bold**.
Secondary decisions variables are dependent on primary decision variables.
Variables are shown in the order they are declared in the code.
We use the symbol :math:`\mathbb{R}` to denote the set of real numbers, :math:`\mathbb{R}^+_0` to denote non-negative real numbers, and :math:`\mathbb{B}` to denote binary variables.
Variables are set-up for the optimization program by :meth:`~opmodels.ccats_optimization.OptimizationModel.declare_variables`.
Variables are listed in :numref:`Table %s <table-variables>`.

.. csv-table:: Variables.
    :header: "Variable", "CCATS Name", "Data Type", "Short Description", "Units", "Detailed Description"
    :widths: 8, 28, 14, 50, 8, 14
    :name: table-variables

    "","**Costs**"
    ":math:`C^{investment}_t`",":code:`e_sum_costs_investment`",":math:`\mathbb{R}^+_0`","Investment costs", "$", ":ref:`Details<v_sum_costs_investment>`"
    ":math:`C^{policy}_t`",":code:`e_sum_costs_policy`",":math:`\mathbb{R}`","Policy costs", "$", ":ref:`Details<v_sum_costs_policy>`"
    ":math:`C^{variable}_t`",":code:`e_sum_costs_variable`",":math:`\mathbb{R}^+_0`","Variable costs", "$", ":ref:`Details<v_sum_costs_variable>`"
    "","**Investment**"
    ":math:`\mathbf{I^T_{a,d,t}}`",":code:`vb_transport_investment`",":math:`\mathbb{B}`","Transport investment decision", "0 or 1", ":ref:`Details<vb_transport_investment>`"
    ":math:`\mathbf{I^S_{n,t}}`",":code:`v_storage_investment`",":math:`\mathbb{R}^+_0`","Storage investment decision ", "# of AoRs", ":ref:`Details<v_storage_investment>`"
    "","**Storage - injectivity**"
    ":math:`J_{n,t}`",":code:`v_storage_inj_capacity`",":math:`\mathbb{R}^+_0`","Storage injection capacity", ":math:`t\,CO_2/year`", ":ref:`Details<v_storage_inj_capacity>`"
    "","**Pipeline Capacity**"
    ":math:`\mathbf{Q_{a,d,t}}`",":code:`v_transport_capacity_added`",":math:`\mathbb{R}^+_0`","Transport capacity constructed in the current time step", ":math:`t\,CO_2/year`", ":ref:`Details<v_transport_capacity_added>`"
    "","**Pipeline Flow**"
    ":math:`X_{a,t}`",":code:`v_flow`",":math:`\mathbb{R}^+_0`","Flow of arc", ":math:`t\,CO_2/year`",":ref:`Details<v_flow>`"
    ":math:`\mathbf{\bar{X}_{a,t}}`",":code:`v_flow_base`",":math:`\mathbb{R}^+_0`","Flow using existing capacity (built before CCATS run)", ":math:`t\,CO_2/year`", ":ref:`Details<v_flow_base>`"
    ":math:`\mathbf{X_{a,t}}`",":code:`v_flow_add`",":math:`\mathbb{R}^+_0`","Flow using new capacity (built during CCATS run)", ":math:`t\,CO_2/year`", ":ref:`Details<v_flow_add>`"
    ":math:`\mathbf{X_{a,p,t}}`",":code:`v_flow_by_policy`",":math:`\mathbb{R}^+_0`","Flow by policy", ":math:`t\,CO_2/year`", ":ref:`Details<v_flow_by_policy>`"


Variables: Glossary
^^^^^^^^^^^^^^^^^^^

.. _v_sum_costs_investment:

Investment costs
""""""""""""""""
:math:`C^{investment}_t` is the sum of investment costs committed in time period :math:`t`.


.. _v_sum_costs_policy:

Policy costs
""""""""""""

:math:`C^{policy}_t` is the sum of policy costs occurring during time period :math:`t`.


.. _v_sum_costs_variable:

Variable costs
""""""""""""""

:math:`C^{variable}_t` is the sum of variable costs occurring during time period :math:`t`.


.. _vb_transport_investment:

Transport investment
""""""""""""""""""""

:math:`\mathbf{I^T_{a,d,t}}` is the transport investment decision at node :math:`n` in time period :math:`t`.
This is only used in the non-linear (MILP) version of CCATS.
This variable is in bold to indicate that it is a primary decision variable.


.. _v_storage_investment:

Storage investment
""""""""""""""""""
:math:`\mathbf{I^S_{n,t}}` is the storage investment decision at node :math:`n` in time period :math:`t`. This variable is in bold to indicate that it is a primary decision variable.


.. _v_storage_inj_capacity:

Injectivity
"""""""""""

:math:`J_{n,t}` is the available injectivity of node :math:`n` in time period :math:`t`.


.. _v_transport_capacity_added:

Transport capacity added
""""""""""""""""""""""""

:math:`\mathbf{Q_{a,d,t}}` is the amount of capacity added to arc :math:`a` of type :math:`d` in time period :math:`t`.
This variable is in bold to indicate that it is a primary decision variable.


.. _v_flow:

Total flow
""""""""""

:math:`X_{a,t}` is the total flow moving through arc :math:`a` in time period :math:`t`.


.. _v_flow_base:

Existing capacity flow
""""""""""""""""""""""

:math:`\mathbf{\bar{X}_{a,t}}` is the flow using existing transport capacity through arc :math:`a` in time period :math:`t`.
This variable is in bold to indicate that it is a primary decision variable.


.. _v_flow_add:

New capacity flow
"""""""""""""""""

:math:`\mathbf{X_{a,t}}` is the flow using new transport capacity through arc :math:`a` in time period :math:`t`.
This variable is in bold to indicate that it is a primary decision variable.


.. _v_flow_by_policy:

Flow by policy
""""""""""""""
:math:`\mathbf{X_{a,p,t}}` is the flow through arc :math:`a` in time period :math:`t` and indexed by policy :math:`p`.
This variable is in bold to indicate that it is a primary decision variable.



