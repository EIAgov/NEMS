c	pgrogram deter

	implicit real*8 (a-h,o-z)

	dimension dt(2,2)
	dimension tr(2,2)

	nsize=2
	zero=0.d0
	dt(1,1)=2.d0
	dt(1,2)=1.d0
	dt(2,1)=1.d0
	dt(2,2)=-3.d0

	do n2=1,nsize
	   do n1=n2+1,nsize
	      if (dt(n1,n2).ne.zero) then
		 do i1=1,nsize
	            do i2=1,nsize
	               tr(i1,i2)=dt(i1,i2)
	            end do
	         end do
	         do m2=1,nsize
	            dt(n1,m2)=dt(n1,m2)-tr(n1,n2)*tr(n2,m2)/tr(n2,n2)
                 end do
	       end if
            end do
	end do

	answer=1.d0
	do n2=1,nsize
	   answer=answer*dt(n2,n2)
	end do

	write (6,*) answer
	stop
	end
