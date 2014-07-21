*=============================================================================
* File      : assign2.gms
* Author    : Mihaly Himics
* Version   : 1.0
* Date      : 16/07/2014
* Remarks   :
$ontext
             Solution for problem set 2, applied trade theory and policy
             (course by Christine Wieck, SS2014, Uni Bonn)

             Consider a country which produces two goods X and Y using the single production factor
             labour. The labour requirements per unit of the goods are 2 and 5 units, respectively. The
             endowment of labour is 50 units. The social utility function is given by U = Y^0.5*X^0.5.

             Solve the following problems analytically and with Excel or GAMS or ….

             a) Provide a graphical sketch of the problem.
             b) What is the opportunity cost of producing good X in terms of Y? Write out the full
             employment condition.
             c) What are the autarky (no-trade) quantities produced and consumed that maximise
             social utility? What is the utility level obtained?
             Hint for the analytical solution: write out the maximization problem (objective
             function and constraint) as e.g. provided in KK, p. 23.
             d) Suppose that the country can trade good X at the price PX = 1 and PY = 3. What are
             the production and consumption quantities with trade? How large is social utility
             now?


$offtext
*=============================================================================


set products /x, y/;

positive variable Q(products)    'optimal quantities produced';
parameter lab_req(products)      'labour requirement in labour units';



*
*   ---  We assume constant opportunity costs, so the labor req. coeffs are constant
*        Opp. cost of x = 2/5
*        Opp. cost of y = 5/2
*
lab_req('x') = 2;
lab_req('y') = 5;

scalar lab_end 'labour endowment' /50/;

variable   U          'utility';

equations
         full_employment          "full employment on the labor market"
         SIC                      "social indifference curve"
;

full_employment .. lab_end =e= sum(products, lab_req(products) * Q(products));

SIC .. U =e= prod(products, sqrt(Q(products)));




*
*   ---  The autarky model maximizes social utility subject to the PPF
*        The PPF in this numerical solution is represented by the full employment condition.
*        Graphically we would find the optimum where the PPF is tangent to the SIC curve.
*
model autarky /full_employment, SIC/;

solve autarky using NLP maximizing U;


$ontext

At the optimum 12.5 units of good x and 5 units of good y are produced.
Attained social utility is around ~7.9

---- VAR Q  optimal quantities produced

         LOWER          LEVEL          UPPER         MARGINAL

x          .            12.5000        +INF             .
y          .             5.0000        +INF      -1.662352E-8

                           LOWER          LEVEL          UPPER         MARGINAL

---- VAR U                 -INF            7.9057        +INF             .


$offtext





*
*   ---  Now open up for trade!
*

parameter pw(products) "world prices" ;
pw("x") = 1;
pw("y") = 3;



*
*   ---  With world prices available we can now define
*        the production value in the economy
*        (valued at world prices).
*
variable   V      'production value';
equation   GNP    "gross national product";

GNP .. V =e= sum(products, pw(products) * Q(products));



*
*   ---  The production and consumption bundle will be different, i.e.
*        the consumption equilibrium and the production equilibrium
*        will be different.
*        As a result, we define the SIC over the consumption bundle.
*
positive variable         C(products)        "consumption quantities";
equation                  SIC2               "social indifference curve over the consumption bundle";
equation                  income_line        "available income for consumption";


SIC2 .. U =e= prod(products, sqrt(C(products)));

income_line .. V =e= sum(products, pw(products) * C(products));


*
*   ---  Solution in two steps.
*        Step 1: define the production equlibrium,
*                (the country will specialize in producing exactly one commodity;
*                 which one depends on the world price ration and the on the opportunity costs.)
*

model prod_equilibrium /full_employment, GNP/;

solve prod_equilibrium using NLP maximizing V;


*
*   --- Step 2: define optimal consumption
*       We fix income at the production value attained in step 1.
*       Graphically, the two maximization problem shares the same income line.
*

model cons_equilibrium /income_line, SIC2/;

V.fx = V.l;

solve cons_equilibrium using NLP maximizing U;


*
*   ---  Trade is the difference between the consumption and production bundle
*

parameter net_trade(products);

net_trade(products)   =  Q.l(products) - C.l(products);

display net_trade;




*
*   ---  Solution in one step by taking advantage of the
*        first order conditions (FOC)
*



*
*   ---  Marshallian demand functions for Cobb-Douglas preferences
*        Optimal demand depends on relative prices and the budget constraint
*
equation demand(products)  "Marshallian demand functions";

demand(products) ..  C(products) =e= 1/2 * V / pw(products);


model one_go "solving the problem in one optimizaton step"  /demand, GNP, full_employment/;
solve one_go using NLP maximizing V;


*
*   --- FOC for the production maximization problem: max V=px*x+py*y   s.t.  ax*x+ay*y=L
*       Tricky, because the solution is always in a corner solution, i.e. full specialization.
*       In which product the country will specialize depends on
*       the opportunity cost vs. relative world prices:
*       At the optimum:    x=L/ax, if ax/px < ay/py
*                          y=L/ay, if ay/py < ax/px
*
*       Basically we don't need to derive the optimal production with the solver,
*       it can be calculated easily with the above formula.


alias(products, products1);
Q.fx(products) = 0;
Q.fx(products) $ [ (lab_req(products)/pw(products)) lt sum(products1 $ (not sameas(products1,products)), lab_req(products1)/pw(products1)) ] = lab_end / lab_req(products);

V.fx = sum(products, pw(products) * Q.l(products));

model FOC "formulation without direct objective, based on the FOCs" /demand/;

solve FOC using CNS;

display Q.l, V.l;



*
*   ---  We can also formulate the model as a
*        Mixed Complementarity Problem (MCP)
*
*        In this case the supply function is missing
*        (fixed Q derived by the simple condition above).
*        But still, this is a valid formulation that is more common
*        in large-scale multi-commodity models
*

V.lo   =  0;
V.up   =  +inf;

model trade_MCP /demand.C, income_line.V/;

solve trade_MCP using MCP;




*
*   --- Building on the above MCP formulation
*       we can substitute the budget constraint
*       with an explicit trade balance condition
*

equation trade_balance   "trade balance condition (ID=ES)";



*
*   --- (Q-C) is the net trade;
*       we could have defined a variable for that,
*       but in this simple case it was easier to do it directly
*
trade_balance ..   sum(products,  (Q(products) - C(products)) * pw(products)) =e= 0;

model trade_MCP2 "formulation with explicite trade balance condition" /demand.C, trade_balance.V/;

solve trade_MCP2 using MCP;

*============================   End Of File   ================================
