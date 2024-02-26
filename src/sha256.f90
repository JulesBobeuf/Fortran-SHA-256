!> \mainpage
!! # SHA-256 Algorithm Documentation
!!
!! This documentation provides an overview of my SHA-256 algorithm implementation in Fortran.
!! 
!! ## Overview
!! The SHA-256 algorithm is a widely used cryptographic hash function that produces a 256-bit hash value.
!! This implementation includes the core SHA-256 algorithm and necessary utility functions.
!!
!! ## Usage
!! - The main program `SHA256` demonstrates the application of the SHA-256 algorithm to hash a password.
!! - The `compression_utils` module contains utility functions used in the SHA-256 algorithm.
!! - The `sigmas` module includes functions representing various operations used in SHA-256.
!!
!! ## Compilation
!! To compile the program, use the provided Makefile. The `make` command will create the executable `SHA-256`.
!!
!! ## Example
!! - Run the compiled executable with the command: `./SHA-256`
!! - Input a password when prompted, and the program will output the SHA-256 hash.
!!
!! ## Author
!! - Jules BOBEUF
!!
!! ## License
!! This code is distributed under the MIT License.
!!
!! ## Disclaimer
!! This code is provided as-is and without any warranty. Use it at your own risk.
!!
!! ## References
!! - [SHA-256 Wikipedia](https://en.wikipedia.org/wiki/SHA-2)

!> \file sha256.f90
!> \brief This program implements the SHA-256 algorithm.
program sha256
    use sigmas
    use compression_utils
    implicit none

    ! Constants for the initial hash values (H) and the constants (K) used in the SHA-256 algorithm
    integer, parameter :: H(8) = [ &
    int(Z'6a09e667'), int(Z'bb67ae85'), &
    int(Z'3c6ef372'), int(Z'a54ff53a'), &
    int(Z'510e527f'), int(Z'9b05688c'), &
    int(Z'1f83d9ab'), int(Z'5be0cd19') ]


    integer, parameter :: K(64) = [ &
    int(Z'428a2f98'), int(Z'71374491'), &
    int(Z'b5c0fbcf'), int(Z'e9b5dba5'), &
    int(Z'3956c25b'), int(Z'59f111f1'), &
    int(Z'923f82a4'), int(Z'ab1c5ed5'), &
    int(Z'd807aa98'), int(Z'12835b01'), &
    int(Z'243185be'), int(Z'550c7dc3'), &
    int(Z'72be5d74'), int(Z'80deb1fe'), &
    int(Z'9bdc06a7'), int(Z'c19bf174'), &
    int(Z'e49b69c1'), int(Z'efbe4786'), &
    int(Z'0fc19dc6'), int(Z'240ca1cc'), &
    int(Z'2de92c6f'), int(Z'4a7484aa'), &
    int(Z'5cb0a9dc'), int(Z'76f988da'), &
    int(Z'983e5152'), int(Z'a831c66d'), &
    int(Z'b00327c8'), int(Z'bf597fc7'), &
    int(Z'c6e00bf3'), int(Z'd5a79147'), &
    int(Z'06ca6351'), int(Z'14292967'), &
    int(Z'27b70a85'), int(Z'2e1b2138'), &
    int(Z'4d2c6dfc'), int(Z'53380d13'), &
    int(Z'650a7354'), int(Z'766a0abb'), &
    int(Z'81c2c92e'), int(Z'92722c85'), &
    int(Z'a2bfe8a1'), int(Z'a81a664b'), &
    int(Z'c24b8b70'), int(Z'c76c51a3'), &
    int(Z'd192e819'), int(Z'd6990624'), &
    int(Z'f40e3585'), int(Z'106aa070'), &
    int(Z'19a4c116'), int(Z'1e376c08'), &
    int(Z'2748774c'), int(Z'34b0bcb5'), &
    int(Z'391c0cb3'), int(Z'4ed8aa4a'), &
    int(Z'5b9cca4f'), int(Z'682e6ff3'), &
    int(Z'748f82ee'), int(Z'78a5636f'), &
    int(Z'84c87814'), int(Z'8cc70208'), &
    int(Z'90befffa'), int(Z'a4506ceb'), &
    int(Z'bef9a3f7'), int(Z'c67178f2') ]

    ! Variables
    character(len=32), dimension(64) :: W
    
    character(len=16) :: password = "ThisIsAPasswd102"
    character(len=512) :: padding = ""
    character(len=64) :: password_length_64_bits, encoded_password
    character(len=32) :: a, b, c, d, e, f, g, hh, y, z

    integer :: char_value, incr
    integer :: password_length = len(password)
    integer :: nb_bit_in_password = LEN(password) * 8 

    ! Create the padding 
    do incr=1, password_length
        char_value = ichar(password(incr:incr))
        padding = padding(1:len( trim(padding) ))//decimal_to_binary(char_value)
    end do

    ! add 1 (SHA-256 Algorithm requires it)
    padding = padding(1:len( trim(padding) ))//'1'

    padding(nb_bit_in_password+2:) = REPEAT('0', (len(padding) - (nb_bit_in_password+1)) - 64)

    write(password_length_64_bits, '(B64.64)') nb_bit_in_password
    
    padding = padding(1:len( trim(padding) ))//password_length_64_bits
    !print *, padding

    W = create_message_schedule(padding)
    !print *, W

    write(a, '(B32.32)') H(1)
    write(b, '(B32.32)') H(2)
    write(c, '(B32.32)') H(3)
    write(d, '(B32.32)') H(4)
    write(e, '(B32.32)') H(5)
    write(f, '(B32.32)') H(6)
    write(g, '(B32.32)') H(7)
    write(hh, '(B32.32)') H(8)


    do incr=1, 64
        y = t1(hh, cap_sigma_1(e), ch(e,f,g), K(incr), W(incr))
        z = sum2words(cap_sigma_0(a), maj(a,b,c))
        hh = g
        g = f
        f = e
        e = sum2words(d, y)
        d = c
        c = b
        b = a
        a = sum2words(y, z)
    end do

    ! print *, "a ", a
    ! print *, "b ", b
    ! print *, "c ", c
    ! print *, "d ", d
    ! print *, "e ", e
    ! print *, "f ", f
    ! print *, "g ", g
    ! print *, "h ", hh

    encoded_password = final_concatenation(H, a, b, c, d, e, f, g, hh)
    print *, encoded_password

    contains

    !> \brief Convert a decimal value to its binary representation.
    !!
    !! This function takes a decimal value and converts it into a binary representation.
    !!
    !! \param[in] decimal The decimal value to be converted (integer).
    !!
    !! \return The binary representation as a character(len=8) sequence.
    !!
    function decimal_to_binary(decimal) result(binary_representation)
        integer, intent(in) :: decimal
        integer :: temp, remainder, i
        integer, parameter :: binary_digits = 8
        character(len=binary_digits) :: binary_representation
    
        temp = decimal
        binary_representation = repeat('0', binary_digits)
    
        do i = binary_digits, 1, -1
            remainder = mod(temp, 2)
            binary_representation(i:i) = char(48 + remainder) ! 48 == ASCII code for '0'
            temp = temp / 2
        end do
    
        ! Trim the leading zeros
        binary_representation = adjustl(binary_representation)
    
    end function decimal_to_binary
    
    !> \brief Create the message schedule for SHA-256 algorithm.
    !!
    !! This function creates the message schedule 'W' used in the SHA-256 algorithm from the given padding.
    !!
    !! \param[in] padding The padded message as a character(len=512) sequence.
    !!
    !! \return The message schedule 'W' as a character(len=32) array.
    !!
    function create_message_schedule(padding) result(W)
        character(len=512), intent(in) :: padding
        character(len=32), dimension(64) :: W
        character(len=32) :: w1, w2, w3, w4, res
        character(len=1) :: tmp_char
        integer :: tmp_integer_value, incr2, bit1, bit2, bit3, bit4


        do incr = 1, 16
            if (incr > 1) then
                ! we want 4 words of 8 bytes, so 32 bits
                W(incr) = padding(((incr-1) * 8 * 4) + 1:incr * 8 * 4)
            else
                W(incr) = padding(1:incr * 8 * 4)
            end if
            ! print *, incr, W(incr)
        end do

        do incr = 17, 64

            w1 = sigma_1(W(incr-2))
            w2 = W(incr-7)
            w3 = sigma_0(W(incr-15))
            w4 = W(incr-16)

            ! print *, "w1 ", w1
            ! print *, "w2 ", w2
            ! print *, "w3 ", w3
            ! print *, "w4 ", w4

            res = ""
            do incr2=1, 32
                read(w1(incr2:incr2), '(B32)') bit1
                read(w2(incr2:incr2), '(B32)') bit2
                read(w3(incr2:incr2), '(B32)') bit3
                read(w4(incr2:incr2), '(B32)') bit4

                tmp_integer_value = ieor(ieor(bit4, bit3), ieor(bit2, bit1))
                write(tmp_char, '(B1)') tmp_integer_value
                res = res(1: len(trim(res)))//tmp_char

            end do

            W(incr) =  res
            ! print *, "W ", incr, " ", W(incr)
        end do

    end function create_message_schedule

    !> \brief Perform the final concatenation for SHA-256 algorithm.
    !!
    !! This function performs the final concatenation of hash values according to the SHA-256 algorithm.
    !!
    !! \param[in] H Hash values as a 8-element integer array.
    !! \param[in] a, b, c, d, e, f, g, hh Intermediate hash values as character(len=32) sequences.
    !!
    !! \return The final concatenated hash value (hexadecimal) as a character(len=64) sequence.
    !!
    function final_concatenation(H, a, b, c, d, e, f, g, hh) result(res)
        integer, dimension(8), intent(in) :: H
        character(len=32), intent(in) :: a, b, c, d, e, f, g, hh
        character(len=64) :: res
        character(len=8) :: tmp_hex
        integer :: tmp_integer_value

        res = ""

        read(a, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(1)
        write(res, '(Z8)') tmp_integer_value

        read(b, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(2)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

        read(c, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(3)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

        read(d, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(4)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

        read(e, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(5)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

        read(f, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(6)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

        read(g, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(7)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

        read(hh, '(B32)') tmp_integer_value
        tmp_integer_value = tmp_integer_value + H(8)
        write(tmp_hex, '(Z8)') tmp_integer_value
        res = res(1: len(trim(res)))//tmp_hex

    end function final_concatenation


end program sha256