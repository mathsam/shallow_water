#/bin/csh

@ i = 3 
while ( $i < 21)
  echo $i
  cd Oct17_amp$i
  msub ./runscript
  cd ..
  @ i++
end
