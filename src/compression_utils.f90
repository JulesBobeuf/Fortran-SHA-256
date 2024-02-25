module compression_utils
    implicit none

    contains

    function ch(e,f,g) result(res)
        character(len=32), intent(in) :: e, f, g
        character(len=32) :: res
        integer :: incr = 1

        res = ""
        do incr=1, 32
            if (e(incr:incr) .eq. '0') then
                res = res(1: len(trim(res)))//g(incr:incr)
            else
                res = res(1: len(trim(res)))//f(incr:incr)
            end if
        end do

        ! print *,"ch : ", res
    end function ch

    function maj(a, b, c) result(res)
        character(len=32) :: a, b, c, res
        integer :: num1, num2, num3, i
    
        res = ""
        do i = 1, 32
            read(a(i:i), '(B1)') num1
            read(b(i:i), '(B1)') num2
            read(c(i:i), '(B1)') num3

            if (num1 + num2 + num3 > 1) then
                res = res(1: len(trim(res)))//'1'
            else
                res = res(1: len(trim(res)))//'0'
            end if
        end do

        ! print *,"maj : ", res
    end function maj


    function t1(h, cz, ch, k, w) result(res)
        implicit none

        character(len=32), intent(in) :: h, cz, ch, w
        integer, intent(in) :: k

        character(len=32) :: res, k_string
        character(len=1) :: tmp_char
        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, bit3, bit4, bit5, incr


        ! Convert the integer back to a bit string
        write(k_string, '(B32.32)') k

        res = ""
        do incr=1, 32
            read(h(incr:incr), '(B32)') bit1
            read(cz(incr:incr), '(B32)') bit2
            read(ch(incr:incr), '(B32)') bit3
            read(k_string(incr:incr), '(B32)') bit4
            read(w(incr:incr), '(B32)') bit5

            tmp_integer_value = mod(bit1 + bit2 + bit3 + bit4 + bit5, 2)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do
        ! print *, "t1 : ", res

    end function t1


    function sum2words(w1, w2) result(res)
        implicit none

        character(len=32), intent(in) :: w1, w2
        character(len=32) :: res
        character(len=1) :: tmp_char

        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, incr

        res = ""
        do incr=1, 32
            read(w1(incr:incr), '(B32)') bit1
            read(w2(incr:incr), '(B32)') bit2

            tmp_integer_value = ieor(bit1, bit2)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do

    end function sum2words
end module compression_utils