# Makefile

# Compiler and flags for gfortran
FC = gfortran
FFLAGS = -Wall

# Source files in the src folder
SRC_DIR = src
SIGMAS_SRC = $(SRC_DIR)/sigmas.f90
COMPRESSION_UTILS_SRC = $(SRC_DIR)/compression_utils.f90
MAIN_SRC = $(SRC_DIR)/sha256.f90

# Object files
COMPRESSION_UTILS_OBJ = $(COMPRESSION_UTILS_SRC:$(SRC_DIR)/%.f90=%.o)
SIGMAS_OBJ = $(SIGMAS_SRC:$(SRC_DIR)/%.f90=%.o)
MAIN_OBJ = $(MAIN_SRC:$(SRC_DIR)/%.f90=%.o)

# Executable
EXECUTABLE = SHA-256

# Targets
all: $(EXECUTABLE)

$(EXECUTABLE): $(COMPRESSION_UTILS_OBJ) $(SIGMAS_OBJ) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

$(COMPRESSION_UTILS_OBJ): $(COMPRESSION_UTILS_SRC)
	$(FC) $(FFLAGS) -c $< -o $@

$(SIGMAS_OBJ): $(SIGMAS_SRC)
	$(FC) $(FFLAGS) -c $< -o $@

$(MAIN_OBJ): $(MAIN_SRC)
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	del *.exe *.mod *.o
