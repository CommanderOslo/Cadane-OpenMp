module Homework
implicit none
contains
subroutine FindMaxCoordinates(A, x1, y1, x2, y2)
include "mpif.h"
real(8), intent(in), dimension(:,:) :: A
real(8), dimension(size(A(:,1))) :: B
integer(4), intent(out) :: x1, y1, x2, y2
integer(4) MatrixHeight, MatrixLength, i, j, k, bottom_border, upper_border, bottom, up
integer(4) mpiErr, mpiSize, mpiRank, ProcwithmaxSumm
real(8) previousSumm, Summ, maxSumm
real(8), dimension(:), allocatable :: ALLMaxSums
x1=1
x2=1
y1=1
y2=1
maxSumm=A(1,1)
MatrixLength=size(A(:,1))
MatrixHeight=size(A(1,:))
call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)
allocate(ALLMaxSums(mpiSize))
do i=(mpiRank+1),MatrixHeight,mpiSize

 do k=1,MatrixLength
  B(k)=0
 enddo

 do j=i,MatrixHeight
  B=B+A(:,j)
  previousSumm=B(1); bottom=1; up=1
  Summ=previousSumm; bottom_border=bottom; upper_border=up
  do k=2,MatrixLength
   if (B(k)>(B(k)+previousSumm)) then
    bottom=k
    up=k
    previousSumm=B(k)
   else
    up=k
    previousSumm=B(k)+previousSumm
   endif
   if (previousSumm>Summ) then
    Summ=previousSumm
    bottom_border=bottom
    upper_border=up
   endif
  enddo
  if (Summ>maxSumm) then
   maxSumm=Summ
   x1=bottom_border
   x2=upper_border
   y1=i
   y2=j
  endif
 enddo
enddo

call mpi_gather(maxSumm, 1, MPI_REAL8, ALLMaxSums, 1, MPI_REAL8, 0, MPI_COMM_WORLD, mpiErr)
if (mpiRank==0) then
 ProcwithmaxSumm=maxloc(ALLMaxSums, dim=1)
 ProcwithmaxSumm=ProcwithmaxSumm-1
endif
call mpi_bcast(ProcwithmaxSumm, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)

call mpi_bcast(x1, 1, MPI_INTEGER4, ProcwithmaxSumm, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(x2, 1, MPI_INTEGER4, ProcwithmaxSumm, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(y1, 1, MPI_INTEGER4, ProcwithmaxSumm, MPI_COMM_WORLD, mpiErr)
call mpi_bcast(y2, 1, MPI_INTEGER4, ProcwithmaxSumm, MPI_COMM_WORLD, mpiErr)

end subroutine
end module
