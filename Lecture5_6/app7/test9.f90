program test9
implicit none
integer n, maxim, minim, sum, tmp
open(10,file='data.txt',form='formatted',action='read')
sum = 0
n = 0
input : do
 read(10,*,end=99) tmp
 n = n + 1
 sum = sum + tmp
 if(n > 1) then
  maxim = max(maxim, tmp)
  minim = min(minim, tmp)
 elseif(n == 1) then
  maxim = tmp
  minim = tmp
 endif
enddo input
99 close(10)
write(*,'(A,2(I,1X),ES13.6E2)') &
 'saidai, heikin, saisho = ', maxim, real(sum) / n, minim
end program
