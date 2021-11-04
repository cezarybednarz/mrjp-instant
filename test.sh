
make gen_test;


for i in `seq 10000 10001` 
do
  echo $i
  ./my_test/gen_test $i > my_test/test.in
  time ./insc_llvm my_test/test.in
  time ./insc_jvm my_test/test.in

done