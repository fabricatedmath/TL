# Gencode arguments
SMS ?= 35 37 50 52 60 61 70 75 80 86

$(foreach sm,$(SMS),$(eval GENCODE_FLAGS += -gencode arch=compute_$(sm),code=sm_$(sm)))

all: kernels/sha256_iter.cu
	nvcc -Xptxas=-v --fatbin $(GENCODE_FLAGS) kernels/sha256_iter.cu -o ${SOURCE_DIR}/sha256_iter.fatbin;

clean:
	rm -f sha256_iter.fatbin