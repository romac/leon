#!/bin/bash
log=repairs.last
fullLog=repairs.log

echo -n "" > $log;


echo "################################" >> $fullLog
echo "#" `hostname` >> $fullLog
echo "#" `date +"%d.%m.%Y %T"` >> $fullLog
echo "#" `git log -1 --pretty=format:"%h - %cd"` >> $fullLog
echo "################################" >> $fullLog
echo "#           Category,                 File,             function,   S, f.S,   Tms,   Fms,   Rms, verif?" >> $fullLog

#All benchmarks:
./leon --repair --solvers=fairz3:enum --functions=desugar testcases/repair/Desugar/Desugar1.scala
./leon --repair --solvers=fairz3:enum --functions=desugar testcases/repair/Desugar/Desugar2.scala
./leon --repair --solvers=fairz3:enum --functions=desugar testcases/repair/Desugar/Desugar3.scala
./leon --repair --solvers=fairz3:enum --functions=desugar testcases/repair/Desugar/Desugar4.scala

./leon --repair --solvers=fairz3:enum --functions=merge   testcases/repair/HeapSort/HeapSort3.scala
./leon --repair --solvers=fairz3:enum --functions=merge   testcases/repair/HeapSort/HeapSort4.scala
./leon --repair --solvers=fairz3:enum --functions=merge   testcases/repair/HeapSort/HeapSort8.scala
./leon --repair --solvers=fairz3:enum --functions=merge   testcases/repair/HeapSort/HeapSort6.scala
./leon --repair --solvers=fairz3:enum --functions=merge   testcases/repair/HeapSort/HeapSort7.scala
./leon --repair --solvers=fairz3:enum --functions=insert  testcases/repair/HeapSort/HeapSort5.scala
./leon --repair --solvers=fairz3:enum --functions=makeN   testcases/repair/HeapSort/HeapSort9.scala

./leon --repair --solvers=fairz3:enum --functions=nnf     testcases/repair/PropLogic/PropLogic1.scala 
./leon --repair --solvers=fairz3:enum --functions=nnf     testcases/repair/PropLogic/PropLogic2.scala 
./leon --repair --solvers=fairz3:enum --functions=nnf     testcases/repair/PropLogic/PropLogic3.scala 
./leon --repair --solvers=fairz3:enum --functions=nnf     testcases/repair/PropLogic/PropLogic4.scala 
./leon --repair --solvers=fairz3:enum --functions=nnf     testcases/repair/PropLogic/PropLogic5.scala 

# Average results
cat $log >> $fullLog
awk '{ total1 += $6; total2 += $7; total3 += $8; count++ } END { printf "#%69s Avg: %5d, %5d, %5d\n\n", "", total1/count, total2/count, total3/count }' $log >> $fullLog
