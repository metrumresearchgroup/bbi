## omega block

* for omega block structure, if no block:

0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:   2

0INITIAL ESTIMATE OF OMEGA:
 0.5000E-01
 0.0000E+00   0.2000E+00

 * if has one block:

0OMEGA HAS BLOCK FORM:
  1
  1  1

 0INITIAL ESTIMATE OF OMEGA:
  BLOCK SET NO.   BLOCK                                                                    FIXED
         1                                                                                   NO
                   0.2000E+00
                   0.1000E+00   0.2000E+00

* if has at least 1 block

0OMEGA HAS BLOCK FORM:
  1
  0  2
  0  2  2
  0  0  0  3

 0INITIAL ESTIMATE OF OMEGA:
  BLOCK SET NO.   BLOCK                                                                    FIXED
         1                                                                                   NO
                   0.2000E+00
         2                                                                                   NO
                   0.9000E-01
                   0.3000E-01   0.9000E-01
         3                                                                                   NO
                   0.1000E-01


## SGE notes

* submission script should look something like: 

`qsub -V -r y -N wexample10.ctl -j y -cwd -b y -q main.q /opt/nm73/run/nmfe73 <runName>.mod <runName>.lst -background -prdefault`

