*=============================================================================
* File      : assignment5.gms
* Author    : Mihaly Himics
* Version   : 1.0
* Date      : 16/07/2014
* Remarks   :
$ontext
             Solution for problem set 5, applied trade theory and policy
             (course by Christine Wieck, SS2014, Uni Bonn)

             Problem 5: Trade in a Heckscher-Ohlin setting

             This problem starts from the same technology definition as problem 3 in the last problem set,
             but we now consider two countries with different capital and labour endowments. Both
             countries A and B produce two goods, potatoes (P) and cars (C) using Capital (K) and Labour
             (L).
             The technologies for both countries are given by P = 10*K_p^0.3*L_p^0.7 and  C = 10*K_p^0.7*L_p^0.3
             respectively.
             Available endowments in country A are K^A=5 and L^A=10, for country B we have K^B=10 and
             L^B=5. Social utility in both countries is given by the utility function U = P^0.5*C^0.5.


            a) Derive the pre-trade production (= consumption) quantities of potatoes and cars for
            country A and country B (separately) either numerically in Excel or analytically. This
            is not trivial. The key is to set up the optimisation problem right. Remember that the
            (closed economy) equilibrium point is determined by maximising utility subject to the
            technologies and resource endowments of the country. One way is to choose allocated
            quantities of factors to production activities as decision variables and substitute the
            production functions for the production quantities in the utility function. Then
            maximise this expression subject to the endowment restrictions. The file
            3_problem.xls contains already a maximisation set up that is close to what you are
            looking for.

            b) What are the pre-trade opportunity costs of producing potatoes in both countries? Hint:
            In an equilibrium situation, the MRT is equal to the slope of the PPF and equal to the
            marginal rate of substitution (MRS) on the consumer side, which can be derived from
            the given utility function.

            c) Which production activity is labour intensive in the autarky situation in country A and
            country B?

            d) Which country will export cars and which one will export potatoes when borders are
            opened for trade and why? Is this consistent with the Heckscher-Ohlin theorem?

            e) Change the optimisation setup in 3_problem set.xls such that you find the open
            economy equilibrium for a country (i.e. production, consumption, and trade quantities)
            under given international market prices P_P = P_C = 3.

$offtext
*=============================================================================
$offlisting


*
*   ---  That's the typical 2x2 Heckscher-Ohlin-Samuelson model of int. trade
*        The two countries have the exact same production technologies and
*        only differ in factor endowments.
*        PRoduction factors are fully mobile between industries within a country
*        but not mobile at all between countries.

*        In the H-O model, the reason for trade is the difference in endowments and
*        not the difference in prod. technologies (as it was with the comparative advantage argumentation).
*


sets
      goods             /P "potatoes", C "cars"/
      factors           /K "capital", L "labor"/
      countries         /A, B/
;

alias (goods,goods1);
alias (factors,factors1);

positive variable FUSE(goods, factors, countries)  "factor use in production";
positive variable Q(goods, countries)              "quantities produced";

table a(goods, *)             "technology parameters (same for both countries)"
        K           L       const
P      .3          .7        10
C      .7          .3        10
;

table endowments(factors, countries) "factor endowments"
      A       B
K     5      10
L    10       5
;

variable OUTPUT(countries);

equation
        prod_func(goods, countries)            "production function"
        resource_const(factors, countries)     "resource constraints"
        tot_output(countries)                  "total output of the economy"
        SIC(countries)                         "SIC or social utilty functions"
        tot_utility                            "sum of utilities in country A and B"
;

variable
            OUTPUT(countries)      "total output"
            U(countries)           "utility"
            totU                   "total utility in the world"
;

prod_func(goods, countries)  ..
                     Q(goods, countries) =e= a(goods, "const") * prod(factors, FUSE(goods, factors, countries)**a(goods, factors));

resource_const(factors, countries) ..
                     sum(goods, FUSE(goods, factors, countries)) =e= endowments(factors, countries);

tot_output(countries) ..
                     OUTPUT(countries)  =e= sum(goods, Q(goods, countries));

SIC(countries) ..
                     U(countries)       =e= prod(goods, Q(goods, countries) ** 0.5);

tot_utility ..
                     totU               =e= sum(countries, U(countries));



*
*   ---   The autarky models of the two economies (A and B)
*         are totally independent and could be solved in two
*         independent optimization model.
*         Here we do it in one go.
*
model autarky /prod_func, SIC, resource_const, tot_utility/;


solve autarky using NLP maximizing totU;



$ontext

Autaryk equilibria are completely symmetric in the two countries,
as a direct consequent of the following:
- technologies are the same
- SICs are the same
- factor endowments are symmetric

Consumption and production equilibria in autarky coincide (see below).


---- VAR FUSE  factor use in production

             LOWER          LEVEL          UPPER         MARGINAL

P.K.A          .             1.5000        +INF            EPS
P.K.B          .             3.0000        +INF            EPS
P.L.A          .             7.0000        +INF             .
P.L.B          .             3.5000        +INF             .
C.K.A          .             3.5000        +INF             .
C.K.B          .             7.0000        +INF             .
C.L.A          .             3.0000        +INF            EPS
C.L.B          .             1.5000        +INF            EPS

---- VAR Q  quantities produced

           LOWER          LEVEL          UPPER         MARGINAL

P.A          .            44.0957        +INF             .
P.B          .            33.4183        +INF             .
C.A          .            33.4183        +INF             .
C.B          .            44.0957        +INF             .

---- VAR U  utility

         LOWER          LEVEL          UPPER         MARGINAL

A        -INF           38.3875        +INF             .
B        -INF           38.3875        +INF             .

                           LOWER          LEVEL          UPPER         MARGINAL

---- VAR totU              -INF           76.7750        +INF             .

  totU  total utility in the world

$offtext



*
*   --- Exercise b)
*       Opportunity cost in autarky equals to the tangent to the SIC (or MRS).
*       MRS = (dU/dC)/(dU/dP) = P/C, i.e. the marginal rate of substitution
*       in the equlibrium is the ration of the optimal consumption quantities:
*

parameter opp_cost(*, goods, countries) "opportunity cost";

opp_cost("autarky", goods, countries) = sum(goods1 $ (not sameas(goods,goods1)), Q.l(goods1, countries)) / Q.l(goods, countries);

display opp_cost;




*
*   --- Exercise c) factor intensities
*

parameter f_intensities(goods, factors, countries) "factor intensities (ratios)";
f_intensities(goods, factors, countries) =  FUSE.l(goods, factors, countries) / sum(factors1 $ (not sameas(factors,factors1)), FUSE.l(goods, factors1, countries));

parameter f_intensive(goods, factors, countries) "identifies the factor that is used intensively in producing the particular good";
f_intensive(goods, factors, countries)
  = (f_intensities(goods, factors, countries) gt sum(factors1 $ (not sameas(factors,factors1)), f_intensities(goods, factors1, countries)));

display f_intensive;


$ontext

Potatoe is the labor intensive, and car is the capital intensive industry in both countries.
(Remember, prod. technologies are the same)

----    223 PARAMETER f_intensive  identifies the factor that is used intensively in producing the particular good

              A           B

P.L       1.000       1.000
C.K       1.000       1.000

$offtext




*
*   ---  Exercise d)
*        Remember the H-O theorem:
*        "If a country is abundant with respect to a certain
*         factor than it will tend to specialise in the production
*         of the commodity that uses this factor intensively."
*
*        So we need to check first factor abundancy
*        Two alternative ways to do that: 1) by relative factor endowments and 2) by relative factor prices
*        {the factor prices are the shadow prices attached to the resource constraints}

parameter f_abundancy(factors, countries) "factor abundancy";


*  1) on the ground of endowments
f_abundancy(factors, countries) $ [(endowments(factors, countries) / sum(factors1 $ (not sameas(factors,factors1)), endowments(factors1, countries))) gt 1] = 1;

display f_abundancy;

* 2) on the ground of factor prices
option kill=f_abundancy;
f_abundancy(factors, countries) $ (resource_const.m(factors, countries) lt sum(factors1 $ (not sameas(factors,factors1)), resource_const.m(factors1, countries))) = 1;

display f_abundancy;

$ontext
Country A is labour-abundant. As potatoes are labour intensive, country A will export potatoes according to the H-O theorem.
Similarly, country B will specialize in the produciton of cars.

----    248 PARAMETER f_abundancy  factor abundancy

            A           B

K                   1.000
L       1.000
$offtext




*
*   ---  Exercise e) Let's set up the open economy model
*                    and verify the H-O theorem
*


*
*   ---  Optimal consumption bundle departs from optimal production set
*        => introduce consumption-related variables/equations
*

positive variable C(goods,countries) "consumption";
equation SIC2(countries);

SIC2(countries) ..
                     U(countries)       =e= prod(goods, C(goods, countries) ** 0.5);



*
*   --- Let's add budget constraints under fix world prices
*

parameter pw(goods)  "world prices";
pw(goods) = 3;

equation budget_const(countries)  "budget constraints";
equation resource_const2(factors, countries);

budget_const(countries) ..
                     sum(goods, Q(goods, countries) * pw(goods)) =e= sum(goods, C(goods, countries) * pw(goods));

resource_const2(factors, countries) ..
                     sum(goods, FUSE(goods, factors, countries)) =l= endowments(factors, countries);



model free_trade /SIC2, tot_utility, budget_const, prod_func, resource_const2/;

C.l(goods,countries)=1;

solve free_trade using NLP maximizing totU;

parameter net_trade(goods, countries) "net trade";

net_trade(goods,countries) = Q.l(goods, countries) - C.l(goods, countries);

display net_trade;




$ontext

Country A will really export potatoes, while country B cars.

----    320 PARAMETER net_trade  net trade

            A           B

P      33.930     -33.930
C     -33.930      33.930


Note that factor prices (equation marginals) are identical in the two countries.
That's the factor price equalization theorem in action:

---- EQU resource_const2

           LOWER          LEVEL          UPPER         MARGINAL

K.A        -INF            5.0000         5.0000         2.7144
K.B        -INF           10.0000        10.0000         2.7144
L.A        -INF           10.0000        10.0000         2.7144
L.B        -INF            5.0000         5.0000         2.7144

That's a very important result indeed. Remember that factors were not mobile
across countries, only between industries. Nevertheless, free trade equalized
wages and capital earnings. It's sometimes mentioned as free trade 'integrates' economies.



$offtext







*============================   End Of File   ================================




