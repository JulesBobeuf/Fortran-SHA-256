module sigmas
    implicit none
    integer :: sigma_incr

    contains

    function sigma_0(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        character(len=1) :: tmp_char
        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, bit3

        ! step 1

        ! Convert the bit string to an integer
        read(word, '(B32)') tmp_integer_value
            ! Perform a right shift cicular of 7 bits
        tmp_integer_value = ishftc(tmp_integer_value, -7)
        ! Convert the integer back to a bit string
        write(step1, '(B32.32)') tmp_integer_value

        ! step 2
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishftc(tmp_integer_value, -18)

        write(step2, '(B32.32)') tmp_integer_value

        ! step 3
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishft(tmp_integer_value, -3)

        write(step3, '(B32.32)') tmp_integer_value

        ! print *, word
        ! print *, step1
        ! print *, step2
        ! print *, step3

        res = ""
        do sigma_incr=1, 32
            read(step1(sigma_incr:sigma_incr), '(B32)') bit1
            read(step2(sigma_incr:sigma_incr), '(B32)') bit2
            read(step3(sigma_incr:sigma_incr), '(B32)') bit3

            tmp_integer_value = ieor(ieor(bit3, bit2), bit1)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do
        ! print *, ieor(ieor(ishftc(tmp_integer_value, -7), ishftc(tmp_integer_value, -8)), ishft(tmp_integer_value, -3))


    end function sigma_0


    function sigma_1(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        character(len=1) :: tmp_char
        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, bit3

        ! step 1

        ! Convert the bit string to an integer
        read(word, '(B32)') tmp_integer_value
        ! Perform a right shift cicular of 17 bits
        tmp_integer_value = ishftc(tmp_integer_value, -17)
        ! Convert the integer back to a bit string
        write(step1, '(B32.32)') tmp_integer_value

        ! step 2
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishftc(tmp_integer_value, -19)

        write(step2, '(B32.32)') tmp_integer_value

        ! step 3
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishft(tmp_integer_value, -10)

        write(step3, '(B32.32)') tmp_integer_value

        ! print *, word
        ! print *, step1
        ! print *, step2
        ! print *, step3

        res = ""
        do sigma_incr=1, 32
            read(step1(sigma_incr:sigma_incr), '(B32)') bit1
            read(step2(sigma_incr:sigma_incr), '(B32)') bit2
            read(step3(sigma_incr:sigma_incr), '(B32)') bit3

            tmp_integer_value = ieor(ieor(bit3, bit2), bit1)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do

    end function sigma_1



    function cap_sigma_0(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        character(len=1) :: tmp_char
        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, bit3

        ! step 1

        ! Convert the bit string to an integer
        read(word, '(B32)') tmp_integer_value
        ! Perform a right shift cicular of -2 bits
        tmp_integer_value = ishftc(tmp_integer_value, -2)
        ! Convert the integer back to a bit string
        write(step1, '(B32.32)') tmp_integer_value

        ! step 2
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishftc(tmp_integer_value, -13)

        write(step2, '(B32.32)') tmp_integer_value

        ! step 3
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishftc(tmp_integer_value, -22)

        write(step3, '(B32.32)') tmp_integer_value

        res = ""
        do sigma_incr=1, 32
            read(step1(sigma_incr:sigma_incr), '(B32)') bit1
            read(step2(sigma_incr:sigma_incr), '(B32)') bit2
            read(step3(sigma_incr:sigma_incr), '(B32)') bit3

            tmp_integer_value = ieor(ieor(bit3, bit2), bit1)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do
        ! print *, "cap sigma 0 : ", res

    end function cap_sigma_0

    function cap_sigma_1(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        character(len=1) :: tmp_char
        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, bit3

        ! step 1

        ! Convert the bit string to an integer
        read(word, '(B32)') tmp_integer_value
        ! Perform a right shift cicular of 6 bits
        tmp_integer_value = ishftc(tmp_integer_value, -6)
        ! Convert the integer back to a bit string
        write(step1, '(B32.32)') tmp_integer_value

        ! step 2
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishftc(tmp_integer_value, -11)

        write(step2, '(B32.32)') tmp_integer_value

        ! step 3
        read(word, '(B32)') tmp_integer_value

        tmp_integer_value = ishftc(tmp_integer_value, -25)

        write(step3, '(B32.32)') tmp_integer_value

        res = ""
        do sigma_incr=1, 32
            read(step1(sigma_incr:sigma_incr), '(B32)') bit1
            read(step2(sigma_incr:sigma_incr), '(B32)') bit2
            read(step3(sigma_incr:sigma_incr), '(B32)') bit3

            tmp_integer_value = ieor(ieor(bit3, bit2), bit1)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do

        ! print *, "cap sigma 1 : ", res
    end function cap_sigma_1

end module sigmas