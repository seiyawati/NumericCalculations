      program exercise0102
      implicit none
      integer :: gender, men, women
      real :: sum1, sum2, height

      open(10,file='height.dat',form='formatted')

      sum1 = 0.0d0 ; sum2 = 0.0d0 
      men = 0 ; women = 0

      input : do
       read(10,*,end=999) gender, height

       if(gender == 1) then
        sum1 = sum1 + height ; men = men + 1

       elseif(gender == 2) then
        sum2 = sum2 + height ; women = women + 1

       else
        write(*,*) &
         'gender is not 1 nor 2 in ', men + women + 1, '-th line'

       endif

      enddo input

  999 close(10)


      write(*,'(A,2(1X,I3))') &
       'Numbers of men/women are', men, women 

      write(*,'(A,2(ES9.3E1,A))') &
       'Average height is ' &
        , sum1 / men ,   ' m for men and ' &
        , sum2 / women , ' m for women.'

      end program
