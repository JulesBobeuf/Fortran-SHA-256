!> \file sigmas.f90
!> \brief Module containing functions for SHA-256 algorithm's sigmas.
module sigmas
    use compression_utils
    implicit none

    contains

    !> \brief Calculate the sigma_0 function for SHA-256 algorithm.
    !! 
    !! This function calculates the sigma_0 function used in the SHA-256 algorithm. It involves three steps:
    !! - step1: Circular right shift of 17 bits
    !! - step2: Circular right shift of 19 bits
    !! - step3: Right shift of 10 bits
    !! - res: Bitwise addition of Steps 1, 2, and 3
    !!
    !! \param[in] word The 32-bit binary sequence 'word' (character(len=32)).
    !!
    !! \return The result of the sigma_0 function as a 32-bit binary sequence (character(len=32)).
    !!
    function sigma_0(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        integer :: tmp_integer_value = 0

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

        res = sum3words(step1, step2, step3)

    end function sigma_0


    !> \brief Calculate the sigma_1 function for SHA-256 algorithm.
    !! 
    !! This function calculates the sigma_1 function used in the SHA-256 algorithm. It involves three steps:
    !! - step1: Circular right shift of 17 bits
    !! - step2: Circular right shift of 19 bits
    !! - step3: Right shift of 10 bits
    !! - res: Bitwise addition of Steps 1, 2, and 3
    !!
    !! \param[in] word The 32-bit binary sequence 'word' (character(len=32)).
    !!
    !! \return The result of the sigma_1 function as a 32-bit binary sequence (character(len=32)).
    !!
    function sigma_1(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        integer :: tmp_integer_value = 0

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

        res = sum3words(step1, step2, step3)

    end function sigma_1


    !> \brief Calculate the capitalized sigma_0 function for SHA-256 algorithm.
    !! 
    !! This function calculates the Capital Sigma_0 function used in the SHA-256 algorithm. It involves three steps:
    !! - step1: Circular right shift of 2 bits
    !! - step2: Circular right shift of 13 bits
    !! - step3: Circular right shift of 22 bits
    !! - res: Bitwise addition of Steps 1, 2, and 3
    !!
    !! \param[in] word The 32-bit binary sequence 'word' (character(len=32)).
    !!
    !! \return The result of the capitalized sigma_0 function as a 32-bit binary sequence (character(len=32)).
    !!
    function cap_sigma_0(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        integer :: tmp_integer_value = 0

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

        res = sum3words(step1, step2, step3)


    end function cap_sigma_0

    !> \brief Calculate the cap_sigma_1 function for SHA-256 algorithm.
    !! 
    !! This function calculates the Capital Sigma_1 function used in the SHA-256 algorithm. It involves three steps:
    !! - step1: Circular right shift of 6 bits
    !! - step2: Circular right shift of 11 bits
    !! - step3: Circular right shift of 25 bits
    !! - res: Bitwise addition of Steps 1, 2, and 3
    !!
    !! \param[in] word The 32-bit binary sequence 'word' (character(len=32)).
    !!
    !! \return The result of the cap_sigma_1 function as a 32-bit binary sequence (character(len=32)).
    !!
    function cap_sigma_1(word) result(res)
        implicit none

        character(len=32), intent(in) :: word
        character(len=32) :: step1, step2, step3, res
        integer :: tmp_integer_value = 0

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

        res = sum3words(step1, step2, step3)

    end function cap_sigma_1
end module sigmas