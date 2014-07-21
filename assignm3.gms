*=============================================================================
* File      : assignment3.gms
* Author    : Mihaly Himics
* Version   : 1.0
* Date      : 16/07/2014
* Remarks   :
$ontext
             Solution for problem set 3, applied trade theory and policy
             (course by Christine Wieck, SS2014, Uni Bonn)

             Problem 3: Derivation of PPF with Excel
             Country A produces two goods, potatoes (P) and cars (C) using Capital (K) and Labour (L).
             The technologies are given by P= 10*K_p^0.3*L_p^0.7,  C= 10*K_c^0.7*L_c^0.3, respectively.
             Available endowments are K=5 and L=10.

             a) Numerically derive the production possibility frontier (PPF). For this use the solver in
             Excel. The general optimisation set-up is given in the 3_problem.xlsx. Set up a table
             of values for P and C. Then draw the PPF afterwards.
             b) Repeat the exercise in a) for K=10 and L=5. What is the difference to the PPF in a)
             and why?
             c) Repeat the exercise in a) for the case that the technologies of both products are equal
             with production elasticities of capital = 0.7 and labour =0.3. What is the difference to
             the PPF in a) and why?


$offtext
*=============================================================================


*
*   --- Set the directory path to a GNUPLOT installation
*       (for drawing the PPF curve)
*
* sets and parameters for gnuplotxyz
$setlocal gnuplot_path 'S:\util\gnuplot\bin\'



sets
      goods             /P "potatoes", C "cars"/
      factors           /K "capital", L "labor"/
;

positive variable FUSE(goods, factors)  "factor use in production";
positive variable Q(goods)              "quantities produced";

table a(goods, *)             "technology parameters"
        K           L       const
P      .3          .7        10
C      .7          .3        10
;

parameter endowments(factors);

endowments("K") = 5;
endowments("L") = 10;


*
*   --- 1) Draw a PPF
*

*  Find the max. attainable production when allocating
*  all production factors to producing one single good.
parameter qmax(goods) "max. attainable production";

qmax(goods) =  a(goods, "const") * prod(factors, endowments(factors) ** a(goods, factors));

display qmax;


set step /1*100/;
parameter ppf(step, goods) "PPF curve";



*
*   --- steps equally distanced on the "P" axis
*
ppf(step, "P") = 0 + (ord(step) ) * (qmax("P") / (card(step)) );

equation
        prod_func(goods)            "production function"
        resource_const(factors)     "resource constraints"
        tot_output                  "total output of the economy"
;

variable OUTPUT "total output";

prod_func(goods)  ..
                     Q(goods) =e= A(goods, "const") * prod(factors, FUSE(goods, factors)**a(goods, factors));

resource_const(factors) ..
                     sum(goods, FUSE(goods, factors)) =e= endowments(factors);
tot_output ..
                     OUTPUT =e= sum(goods, Q(goods));



*
*   ---  Calculate PPF (constrained on the production of potatoes)
*
model calc_ppf "calculate PPF" /prod_func, resource_const, tot_output/;

Q.l(goods) = 1;



parameter
          ps_Q_up(step, goods)     "upper bounds for the scenario dictionary below"
          ps_Q_lo(step, goods)     "lower bounds for the scenario dictionary below"
          ps_Q   (step, goods)     "results for output"
;

ps_Q_up(step, "C")    = +inf;
ps_Q_lo(step, "C")    = eps;
ps_Q_up(step, "P")    = ppf(step, "P");
ps_Q_lo(step, "P")    = ppf(step, "P");


set scen_dict "scenario dictionary (for GUSS solver option in GAMS)" /
  step.              scenario.          ''
  Q   .              lower.             ps_Q_lo
  Q   .              upper.             ps_Q_up
  Q   .              level.             ps_Q

/;

solve calc_ppf using nlp scenario scen_dict maximizing OUTPUT;

ppf(step, goods) = ps_Q(step, goods);

display ppf;



*
*   --- Prepare plot with GNUPLOT
*
*
*   --- Put data points in a .dat file
*
file datafile /PPF.dat/;
put datafile;
loop(step,
   put ppf(step, "P"):10:2;
   put ' ',ppf(step, "C"):10:2
   put /;
);
putclose;


*
*   --- Prepare GNUPLOT script
*
file pltfile /ppf.plt/;
put pltfile;
putclose
   'set xlabel "potatoes"'/
   'set ylabel "cars"'/
   'set xrange [0:90]'/
   'set yrange [0:90]'/
   'set title "Production Possibilities Frontier"'/
   'set key off'/
   'set term png font arial 13'/
   'set output "ppf.png"'/

   'plot "ppf.dat" using 1:2 with lines'
;



* Use Gnuplot to generate picture
execute 'call %gnuplot_path%gnuplot ppf.plt';

* Use mspaint(Windows) to open image file
execute 'mspaint ppf.png';




*
*   --- Now with different endowments  (Exercise b.)
*       Having more capital endowments than labor endowments
*       will increase the possible output of the capital intensive product (cars).
*       The PPF will be symmetric to the previous one.
*
endowments("K") = 10;
endowments("L") = 5;

option kill=qmax;
qmax(goods) =  a(goods, "const") * prod(factors, endowments(factors) ** a(goods, factors));

option kill=ppf;
ppf(step, "P") = 0 + (ord(step) ) * (qmax("P") / (card(step)) );

ps_Q_up(step, "C")    = +inf;
ps_Q_lo(step, "C")    = eps;
ps_Q_up(step, "P")    = ppf(step, "P");
ps_Q_lo(step, "P")    = ppf(step, "P");

Q.l(goods) = 1;

solve calc_ppf using nlp scenario scen_dict maximizing OUTPUT;

ppf(step, goods) = ps_Q(step, goods);

display ppf;


*
*   --- Prepare plot with GNUPLOT
*
*
*   --- Put data points in a .dat file
*
file datafile2 /PPF2.dat/;
put datafile2;
loop(step,
   put ppf(step, "P"):10:2;
   put ' ',ppf(step, "C"):10:2
   put /;
);
putclose;


*
*   --- Prepare GNUPLOT script
*
file pltfile2 /ppf2.plt/;
put pltfile2;
putclose
   'set xlabel "potatoes"'/
   'set ylabel "cars"'/
   'set xrange [0:90]'/
   'set yrange [0:90]'/
   'set title "Production Possibilities Frontier"'/
   'set key off'/
   'set term png font arial 13'/
   'set output "ppf2.png"'/

   'plot "ppf.dat" using 1:2 with lines, "ppf2.dat" using 1:2 with lines'
;



* Use Gnuplot to generate picture
execute 'call %gnuplot_path%gnuplot ppf2.plt';

* Use mspaint(Windows) to open image file
execute 'mspaint ppf2.png';




*
*   --- Now with uniform technology coeffs. (Exercise c.)
*       The PPF will become linear.
*       We need exactly the same combination of primary factors
*       to produce one unit of output of both cars and potatoes.
*       Factor endowments and technology coeffs. define where the linear PPF will be placed.
*       The slope of the linear PPF is always -1.
*
endowments("K") = 5;
endowments("L") = 10;
a(goods, "K") = .7;
a(goods, "L") = .3;


option kill=qmax;
qmax(goods) =  a(goods, "const") * prod(factors, endowments(factors) ** a(goods, factors));

option kill=ppf;
ppf(step, "P") = 0 + (ord(step) ) * (qmax("P") / (card(step)) );

ps_Q_up(step, "C")    = +inf;
ps_Q_lo(step, "C")    = eps;
ps_Q_up(step, "P")    = ppf(step, "P");
ps_Q_lo(step, "P")    = ppf(step, "P");

Q.l(goods) = 1;

solve calc_ppf using nlp scenario scen_dict maximizing OUTPUT;

ppf(step, goods) = ps_Q(step, goods);

display ppf;


*
*   --- Prepare plot with GNUPLOT
*
*
*   --- Put data points in a .dat file
*
file datafile3 /PPF3.dat/;
put datafile3;
loop(step,
   put ppf(step, "P"):10:2;
   put ' ',ppf(step, "C"):10:2
   put /;
);
putclose;


*
*   --- Prepare GNUPLOT script
*
file pltfile3 /ppf3.plt/;
put pltfile3;
putclose
   'set xlabel "potatoes"'/
   'set ylabel "cars"'/
   'set xrange [0:90]'/
   'set yrange [0:90]'/
   'set title "Production Possibilities Frontier"'/
   'set key off'/
   'set term png font arial 13'/
   'set output "ppf3.png"'/

   'plot "ppf.dat" using 1:2 with lines, "ppf2.dat" using 1:2 with lines, "ppf3.dat" using 1:2 with lines'
;



* Use Gnuplot to generate picture
execute 'call %gnuplot_path%gnuplot ppf3.plt';

* Use mspaint(Windows) to open image file
execute 'mspaint ppf3.png';



*============================   End Of File   ================================


