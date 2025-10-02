# Fortran SHA-256

**Fortran SHA-256** is a simple SHA-256 implementation written in Fortran. It is intended for **educational purposes** and helped me to learn how the algorithm works using Fortran code.

## About The Project

This project was created to explore and implement the SHA-256 hashing algorithm in Fortran. It supports messages up to 440 bits and is not optimized for production use. Future improvements could include handling longer messages and optimizing performance.

## Built With

[![Fortran](https://img.shields.io/badge/Fortran-0071C5?style=for-the-badge&logo=fortran&logoColor=white)](https://fortran-lang.org/)  
[![Make](https://img.shields.io/badge/Make-000000?style=for-the-badge&logo=gnu-make&logoColor=white)](https://www.gnu.org/software/make/)

## Getting Started

### Folder Structure

```markdown
Fortran-SHA-256/
├── 📁 src/          # All Fortran source code files
├── 📄 Makefile      # Build instructions
├── 📄 Doxyfile      # Doxygen configuration for docs
└── 📄 README.md     # Project documentation 
```

### Prerequisites

Ensure you have the following installed:

```sh
gfortran
make (or mingw32-make on Windows)
```

### Installation & Build

1. Clone the repository:

```sh
git clone https://github.com/JulesBobeuf/Fortran-SHA-256.git
cd Fortran-SHA-256
```

2. Build the executable:

```sh
make          # or mingw32-make on Windows
```

### Tests / Usage

Run the executable to hash messages:

```sh
./SHA-256.exe ThisIsAPasswd102
./SHA-256.exe "A cool sentence"
```

### Documentation

Generate documentation using Doxygen:

```sh
doxygen Doxyfile
```

## My Configuration

- Windows 11  
- gfortran and mingw32-make

## Resources

- [Wikipedia](https://en.wikipedia.org/wiki/SHA-2)  
- [RedBlockBlue](https://www.youtube.com/watch?v=orIgy2MjqrA&themeRefresh=1)

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact

Jules Bobeuf  
[LinkedIn](https://www.linkedin.com/in/bobeuf-jules/)  
bobeuf.jules@gmail.com

