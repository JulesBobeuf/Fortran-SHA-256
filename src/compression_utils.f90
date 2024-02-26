!> \file compression_utils.f90
!> \brief Module containing utility functions for SHA-256 compression.
module compression_utils
    implicit none

    contains

    !> \brief Ch function for SHA-256 algorithm.
    !!
    !! This function implements the Ch function used in the SHA-256 algorithm. It evaluates each bit position in the
    !! sequences `e`, `f`, and `g` and constructs a new sequence based on the condition specified in the SHA-256 algorithm.
    !! That is : if `e[incr]` = `0`, take `g[incr]`. else `f[incr]`
    !!
    !! \param[in] e The 32-bit binary sequence 'e' (character(len=32)).
    !! \param[in] f The 32-bit binary sequence 'f' (character(len=32)).
    !! \param[in] g The 32-bit binary sequence 'g' (character(len=32)).
    !!
    !! \return The result of the Ch function as a 32-bit binary sequence (character(len=32)).
    !!
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

    end function ch

    !> \brief Majority function for three 32-bit binary sequences.
    !!
    !! This function calculates the majority function for three 32-bit binary sequences (`w1`, `w2`, `w3`). It checks each
    !! bit position and sets the result bit to '1' if the majority of the input bits are '1', otherwise, it sets the result
    !! bit to '0'.
    !!
    !! \param[in] w1 The 32-bit binary sequence 'w1' (character(len=32)).
    !! \param[in] w2 The 32-bit binary sequence 'w2' (character(len=32)).
    !! \param[in] w3 The 32-bit binary sequence 'w3' (character(len=32)).
    !!
    !! \return The result of the majority function as a 32-bit binary sequence (character(len=32)).
    !!
    function maj(w1, w2, w3) result(res)
        character(len=32) :: w1, w2, w3, res
        integer :: bit1, bit2, bit3, incr
    
        res = ""
        do incr = 1, 32
            read(w1(incr:incr), '(B1)') bit1
            read(w2(incr:incr), '(B1)') bit2
            read(w3(incr:incr), '(B1)') bit3

            if (bit1 + bit2 + bit3 > 1) then
                res = res(1: len(trim(res)))//'1'
            else
                res = res(1: len(trim(res)))//'0'
            end if
        end do

        ! print *,"maj : ", res
    end function maj


    !> \brief Calculate the t1 function for SHA-256 algorithm.
    !!
    !! This function calculates the t1 function used in the SHA-256 algorithm. It performs a bitwise addition of five
    !! 32-bit binary sequences (`h`, `cz`, `ch`, `k`, `w`) and applies modular arithmetic to obtain the result.
    !!
    !! \param[in] h  The 32-bit binary sequence 'h' (character(len=32)).
    !! \param[in] cz The 32-bit binary sequence 'cz' (character(len=32)).
    !! \param[in] ch The 32-bit binary sequence 'ch' (character(len=32)).
    !! \param[in] k  The integer constant 'k'.
    !! \param[in] w  The 32-bit binary sequence 'w' (character(len=32)).
    !!
    !! \return The result of the t1 function as a 32-bit binary sequence (character(len=32)).
    !!
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

    end function t1

    !> \brief Perform bitwise addition of two 32-bit binary sequences.
    !! 
    !! Given two 32-bit binary sequences `w1` and `w2`, this function performs a bitwise addition.
    !!
    !! \param[in] w1 The first 32-bit binary sequence (character(len=32)).
    !! \param[in] w2 The second 32-bit binary sequence (character(len=32)).
    !!
    !! \return The result of the bitwise addition as a 32-bit binary sequence (character(len=32)).
    !!
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


    !> \brief Perform bitwise addition of three 32-bit binary sequences.
    !! 
    !! Given three 32-bit binary sequences `w1`, `w2`, and `w3`, this function performs a bitwise addition.
    !!
    !! \param[in] w1 The first 32-bit binary sequence (character(len=32)).
    !! \param[in] w2 The second 32-bit binary sequence (character(len=32)).
    !! \param[in] w3 The third 32-bit binary sequence (character(len=32)).
    !!
    !! \return The result of the bitwise addition as a 32-bit binary sequence (character(len=32)).
    !!
    function sum3words(w1, w2, w3) result(res)
        implicit none

        character(len=32), intent(in) :: w1, w2, w3
        character(len=32) :: res
        character(len=1) :: tmp_char

        integer :: tmp_integer_value = 0
        integer :: bit1, bit2, bit3, incr

        res = ""
        do incr=1, 32
            read(w1(incr:incr), '(B32)') bit1
            read(w2(incr:incr), '(B32)') bit2
            read(w3(incr:incr), '(B32)') bit3

            tmp_integer_value = ieor(ieor(bit3, bit2), bit1)
            write(tmp_char, '(B1)') tmp_integer_value
            res = res(1: len(trim(res)))//tmp_char

        end do

    end function sum3words
end module compression_utils