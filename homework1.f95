module homework
implicit none
contains
subroutine FindMaxCoordinates(a, x1, y1, x2, y2)
use :: omp_lib
real(8), intent(in), dimension(:,:) :: a
integer(4), intent(out) :: x1, y1, x2, y2
integer(4) :: MoveUp, MoveDown, MoveLeft, MoveRight, M, N, j
real(8), dimension(:), allocatable :: p
real(8) :: max, summ
real(8), dimension(:), allocatable :: smaximum
integer(4), dimension(:), allocatable :: leftmaximum, rightmaximum, downmaximum
M = size(a, dim = 1)
N = size(a, dim = 2)
allocate(leftmaximum(M))
allocate(rightmaximum(M))
allocate(downmaximum(M))
allocate(smaximum(M))
!$omp parallel shared(a, M, N, smaximum, leftmaximum, rightmaximum, downmaximum) private(MoveUp, MoveDown, MoveLeft, MoveRight, summ, p) default(none)
allocate(p(N))
!$omp do schedule(dinamic)
do MoveUp = 1, M
  p = 0
  do MoveDown = MoveUp, M
    do j = 1, N
      p(j) = p(j) + a(MoveDown, j)
    enddo
    call SumArray(p, MoveLeft, MoveRight, summ)
    if (summ > smaximum(MoveUp) .or. MoveUp == MoveDown) then
      smaximum(MoveUp) = summ
      leftmaximum(MoveUp) = MoveLeft
      rightmaximum(MoveUp) = MoveRight
      downmaximum(MoveUp) = MoveDown
    endif
  enddo
enddo
!$omp end do
deallocate(p)
!$omp end parallel
max = smaximum(1); x1 = 1; y1 = leftmaximum(1); x2 = downmaximum(1); y2 = rightmaximum(1);
do j=2,M
  if (smaximum(j) > max) then
    max = smaximum(j); x1 = j; y1 = leftmaximum(j); x2 = downmaximum(j); y2 = rightmaximum(j);
  endif
enddo
deallocate(smaximum)
deallocate(leftmaximum)
deallocate(rightmaximum)
deallocate(downmaximum)
end subroutine

subroutine SumArray(a, x1, x2, sum)
real(8), intent(in), dimension(:) :: a
integer(4), intent(out) :: x1, x2
real(8), intent(out) :: sum
integer(4) :: i, l, N
real(8) :: maximum, sum1, sum2
N = size(a)
sum = a(1); x1 = 1; x2 = 1
maximum=a(1); l=1
do i=2, N
  sum1 = a(i)
  sum2 = maximum + a(i)
  if (sum1 > sum2) then
    maximum = sum1
    l = i
  else
    maximum = sum2
  endif
  if (maximum > sum) then
    sum = maximum
    x1 = l
    x2 = i
  endif
enddo
end subroutine
end module

