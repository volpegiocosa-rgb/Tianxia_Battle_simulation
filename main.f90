PROGRAM main
!===============================================================================
! Battle Simulation (Tianxia)
! Author: VolpeGiocosa
! Date: 20/10/2025
!
! WARNING: This file is shared as "READ-ONLY".
! Do not modify it directly. If you want to suggest changes, please fork the
! repository or contact the author.
!
! Recommended license: CC BY-NC-ND (Attribution - Non-Commercial - No Derivatives)
!===============================================================================

    implicit none
    integer :: sims = 1000000
    integer :: j
    integer, dimension(1:16, 1:4) :: marcia, spade
    
    ! Inizializza il generatore di numeri casuali
    call random_seed()    
    
    open(30,file='dist_forze.txt')
    
    open(unit=10, file='red.txt', status='old', action='read')
    do j=1,16
        read(10,*) marcia(j,:), spade(j,:)
    end do
    close(10)
    open(20,file='result_red.txt')
    call simula(sims, marcia, spade)
    close(20)
    
    open(unit=10, file='white.txt', status='old', action='read')
    do j=1,16
        read(10,*) marcia(j,:), spade(j,:)
    end do
    close(10)
    open(20,file='result_white.txt')
    call simula(sims, marcia, spade)    
    close(20)
    
    close(30)

end program  
    
subroutine simula(sims, marcia, spade)
    implicit none
    integer, parameter :: max_forza = 15
    integer, intent(in) :: sims
    integer, dimension(1:16, 1:4), intent(in) :: marcia, spade
    !
    integer, dimension(1:16) :: mazzo 
    integer, dimension(1:4) :: pos, forza
    integer :: j,k,r,f
    !
    integer, allocatable :: battle(:,:), attacco(:,:)
    integer, dimension(1:5) :: forza0
    !
    real*8, dimension(1:4) :: media_battle, media_attacco
    real*8, dimension(1:max_forza) :: reg_forza
    
    allocate(battle(1:sims, 1:4))
    allocate(attacco(1:sims, 1:4))
    !
    forza0 = [5, 7, 9, 10, 0]
    reg_forza = 0
    do j=1, sims
            call shuffle16(mazzo) 
            pos(:) = 0
            forza(:) = forza0(1)
            !
            attacco(j,:) = 0
            battle(j,:) = 0
            do r=1,4
                pos = pos + marcia(mazzo(r),:)
                if (r.eq.4) then
                    pos = pos + 2
                end if
                forza = forza + spade(mazzo(r), :)
                do f=1,4
                    if (forza(f).gt.max_forza) then
                        forza(f) = max_forza
                    end if
                end do
                do k=1,4
                    if (pos(k).ge.6) then ! battaglia
                        reg_forza(forza(k)) = reg_forza(forza(k)) +1
                        battle(j,k) = battle(j,k) + 1
                        pos(k) = 0
                        if (forza(k).gt.attacco(j,k)) then
                            attacco(j,k) =  forza(k)
                        end if
                        forza(k) = forza0(r+1)
                    end if
                end do
            end do
            write(20,20) battle(j,:), attacco(j,:)
    end do
    !
    do j=1,4
        media_battle(j) = dble(sum(battle(:,j))) / dble(sims)
        media_attacco(j) = dble(sum(attacco(:,j))) / dble(sims)
    end do
    !
    write(20,10) media_battle, media_attacco
    write(*,10) media_battle
    !
    write(*,*)
    write(*,*) 'Disposizione forza'
    write(*,9) dble(reg_forza(4:max_forza)) / dble(sum(reg_forza)) * 100.d0
    write(30,9) dble(reg_forza) / dble(sum(reg_forza)) * 100.d0
    !
    deallocate(battle)
    deallocate(attacco)
    
9   FORMAT(100F10.2)    
10  FORMAT(10F10.2) 
20  FORMAT(10I3)    
    return
end subroutine
            
subroutine shuffle16(arr)
    implicit none
    integer, intent(out) :: arr(16)
    integer :: i, j, temp
    real :: r

    ! Inizializza l'array con i numeri da 1 a 16
    do i = 1, 16
        arr(i) = i
    end do

    ! Algoritmo di Fisher–Yates shuffle
    do i = 16, 2, -1
        call random_number(r)
        j = int(r * i) + 1
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
    end do
end subroutine shuffle16            