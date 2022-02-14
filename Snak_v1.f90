program snak
    implicit none
    integer :: size=0, board(100,100)=0,x,y,dirint=0,score=1,ok=1
    character :: dirch
    logical :: oldfile=.false.
    print *, "Welcome to Snak(My shittty cut down wersion of Snake)"
    print *, "Controls:"
    print *, " Up : w/numpad8"
    print *, "Down: s/numpad2"
    print *, "Left: a/numpad4"
    print *, "Right:d/numpad6"
    print *, "Exit :`/~/Q"
    write (*, '(A23)',advance='no') "Load level.dat ?(y/n): "
    read * ,dirch
    if ( dirch=='y' ) call LoadDat(board,size,x,y,score,oldfile)
    if (oldfile) ok=3
    if(.not.oldfile) then
        do while(size>100 .or. size<10)
            write (*, '(A31)',advance='no') "Set board size(10<=size<=100): "
            read * , size
        end do
        x=size/2
        y=size/2
        call Move(board,x,y,dirint,size,ok)
        call Apple(board,size,score)
    end if

    !Main Loop
    do while(dirch /= '~' .and. dirch /= '`'.and. dirch /= 'Q'.and. dirch /= 'q')
        call CH_to_INT(dirch,dirint)
        if(ok<3) call Move(board,x,y,dirint,size,ok)
        if(ok<2) call Update(board,size)
        if(ok==0)then
            write (*, '(A8/A6)',advance='no') "GameOver","Score:"
            print *,score
            if(score==(size*size)) print *,"You Won!"
            exit
        end if
        if(ok==2) then
            score=score+1
            call Apple(board,size,score)
        end if
        call SetPos(board,score,x,y)
        call PrintBoard(board,size,score)
        if(ok==3) ok=1
        read * ,dirch
    end do
    if (dirch /= '~' .or. dirch /= '`'.and. dirch /= 'Q'.and. dirch /= 'q') then
        if(ok/=0) then
            write(*,'(A28)',advance='no') "Save board into file?(y/n): "
            read * ,dirch
            if ( dirch=='y' ) call SaveDat(board,size,x,y,score) 
        end if
    end if
    read *
end program snak

subroutine SaveDat(board, size, x, y, score)
implicit none
integer,dimension(100,100) ,intent(in) :: board
integer,intent(in) :: size, x, y,score
integer :: i,j
open(1, file = 'level.dat')
write(1,*) size, x, y, score
do i=1,size
    do j=1,size
        write(1,*) board(i,j)
    end do
end do
close(1)
end subroutine SaveDat

subroutine LoadDat(board, size, x, y, score,oldfile)
    implicit none
    integer,dimension(100,100) ,intent(out) :: board
    integer,intent(out) :: size, x, y,score
    integer :: i,j
    logical,intent(out) :: oldfile
    inquire(file = 'level.dat',exist=oldfile)
    if(oldfile)then
        open(2, file = 'level.dat',status='old')
        read(2,*) size, x, y, score
        do i=1,size
            do j=1,size
                read(2,*) board(i,j)
            end do
        end do
        close(2)
    else
        print *,"level.dat do not exist"
    end if
end subroutine LoadDat

subroutine Apple(board,  size,score)
implicit none
integer,intent(inout) :: board(100,100)
integer,intent(in) ::  size,score
real :: rand
integer :: intrand,i,j
call random_number(rand)
intrand = 1 + floor((size*size-score)*rand)
do i=1,size
    do j=1,size
        if(board(i,j)==0)then
            intrand=intrand-1
            if(intrand==0) board(i,j)=-1
        end if
    end do
end do

end subroutine Apple

subroutine Move(board,x, y, dirint, size, valid)
implicit none
integer,dimension(100,100),intent(in) :: board
integer,intent(inout) :: x,y
integer,intent(in) :: dirint,size
integer,intent(out) :: valid
valid=1
if (dirint==0) return
if ( mod(dirint,2)==1 ) then
    if ( dirint<2 ) then
        if(y-1==0 .or. board(y-1,x)>1) then
            valid=0
        else 
            if(board(y-1,x)<0) valid=2
            y=y-1
        end if
    else
        if(y==size .or. board(y+1,x)>1) then
            valid=0
        else 
            if(board(y+1,x)<0) valid=2
            y=y+1
        end if
    end if
end if
if ( mod(dirint,2)==0 ) then
    if ( dirint<3 ) then
        if(x-1==0 .or. board(y,x-1)>1) then
            valid=0
        else 
            if(board(y,x-1)<0) valid=2
            x=x-1
        end if
    else
        if(x==size .or. board(y,x+1)>1) then
            valid=0
        else 
            if(board(y,x+1)<0) valid=2
            x=x+1
        end if
    end if
end if
end subroutine Move

subroutine SetPos(board,score,x,y)
implicit none
integer,dimension(100,100),intent(inout) :: board
integer,intent(in) ::  score,x,y
board(y,x)=score
return
end subroutine SetPos

subroutine CH_to_INT(dirch,  dirint)
implicit none
character,intent(in) :: dirch
integer,intent(inout) ::  dirint
select case(dirch)
case("w","8")
    dirint=1
case("a","4")
    dirint=2
case("s","2")
    dirint=3
case("d","6")
    dirint=4
case default
    dirint=0
    return
end select
end subroutine CH_to_INT

subroutine Update(board,size)
    implicit none
    integer, dimension(100,100),intent(inout) :: board
    integer,intent(in) :: size
    integer :: i,j
    do i=1,size
        do j=1,size
            if ( board(i,j)>0 ) then
                board(i,j)= board(i,j)-1
            end if
        end do
    end do

end subroutine Update

subroutine PrintBoard(board,size,score)
    implicit none
    integer, dimension(100,100),intent(inout) :: board
    integer,intent(in) ::  size,score
    integer :: i,j
    do i=1,size
        write(*, '(A2)',advance='no') "##"
    end do
    write(*, '(A2)',advance='yes') "##"
    do i=1,size
        write(*, '(A1)',advance='no') "#"
        do j=1,size
            !write(*, '(I2)',advance='no') board(i,j)
            if(board(i,j)==-1) write(*, '(A2)',advance='no') "@)"
            if(board(i,j)>0 .and. board(i,j)<score) write(*, '(A2)',advance='no') "<>"
            if(board(i,j)==score) write(*, '(A2)',advance='no') "GG"
            if(board(i,j)==0) write(*, '(A2)',advance='no') "  "
        end do
        write(*, '(A1)',advance='no') "#"
        write(*,*)
    end do
    do i=1,size
        write(*, '(A2)',advance='no') "##"
    end do
    write(*, '(A2)',advance='yes') "##"
end subroutine PrintBoard