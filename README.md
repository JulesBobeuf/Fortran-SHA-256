# Fortran SHA-256

This is a simple SHA-256 implementation in Fortran.

## IMPORTANT 

Please do not use this in your professional projects. There will eventually be a release whenever the algorithm will be verified.

## TO DO

- Handle longer messages (440+ bits)
- Improve the code

## My configuration

- Windows 11
- gfortran and mingw32-make

## Launch the app

    make (or mingw32-make)
    ./SHA-256.exe ThisIsAPasswd102
    ./SHA-256.exe "A cool sentence"

## Generate Documentation

    doxygen Doxyfile

### Notes

This is my very first time programming in Fortran. The code will be improved in the future, whenever I will feel more comfortable with the language.

### Resources

- [Wikipedia](https://en.wikipedia.org/wiki/SHA-2)
- [Youtube : RedBlockBlue](https://youtu.be/orIgy2MjqrA?si=PqFuDR1SEsldb1oc)
