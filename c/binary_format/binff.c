
// fortran interface for binary_format

#include <stdio.h>
#include <string.h>
#include <fcntl.h>   // open
#include <unistd.h>  // read 
#include <complex.h> // complex numbers
#include <endian.h>

#include "binf.h"

// uni io uses open/read/write
int binff_uni_open(int length, char *fn, char mode) {
    char c_fn[length+1];                    // convert to c_char* with \0 ending 
    strncpy(c_fn, fn, length);
    c_fn[length]=0;
    int fh=-1;
    printf("len:%d fn:%s mode:%c\n", length, c_fn, mode);

    if(mode=='w') {
        fh = open(c_fn,O_WRONLY|O_CREAT|O_TRUNC,0644);
    }
    else if (mode=='r') {
        fh = open(c_fn,O_RDONLY);
    }
    return fh;
}

// fill bio with data
void binff_uni_init(BinF_IO *bio, int *fh) {
    binf_uni_init(bio,fh);
}

// close the file
int binff_uni_close(int fh) {
    int ret =  close(fh);
    // printf("closing fh:%d ret: %d\n",fh,ret);
    return ret;
}

// ----------------------------------------------------------------------
// std io uses fopen/fread/fwrite
FILE *binff_std_open(int length, char *fn, char mode) {
    char c_fn[length+1];                    // convert to c_char* with \0 ending 
    strncpy(c_fn, fn, length);
    c_fn[length]=0;
    FILE *fp=0;

    if(mode=='w') {
        fp = fopen(c_fn,"wb");
    }
    else if (mode=='r') {
        fp = fopen(c_fn,"rb");
    }
    // printf("len:%d fn:%s mode:%c fp:%p\n", length, c_fn, mode, fp);
    return fp;
}

// fill bio with data
void binff_std_init(BinF_IO *bio, FILE *fp) {
    // printf("bio:%p fp:%p\n", bio, fp);
    binf_std_init(bio,fp);
}

// close the file
int binff_std_close(FILE *fp) {
    int ret =  fclose(fp);
    // printf("closing fp:%p ret: %d\n",fp,ret);
    return ret;
}

// ----------------------------------------------------------------------
// zip io uses popen/fread/fwrite/pclose
FILE *binff_zip_open(int length, char *fn, char mode) {
    char c_fn[length+1];                    // convert to c_char* with \0 ending 
    strncpy(c_fn, fn, length);
    c_fn[length]=0;
    FILE *fp=0;

    if(mode=='w') {
        fp = popen(c_fn,"w");
    }
    else if (mode=='r') {
        fp = popen(c_fn,"r");
    }
    printf("len:%d fn:%s mode:%c fp:%p\n", length, c_fn, mode, fp);
    return fp;
}

// binff_zip_init = binff_std_init

// close the file
int binff_zip_close(FILE *fp) {
    int ret =  pclose(fp);
    // printf("closing fp:%p ret: %d\n",fp,ret);
    return ret;
}


// ---------------------------------------------------------------------

int binff_write_item(BinF_IO* bio, uint8_t** bin_data, uint8_t *object_type, uint16_t *object_size, BINF_INT *array_length) {
    // printf("ptr:%p type:%c size:%d length:%ld\n",*bin_data,*object_type,*object_size,*array_length);
    return binf_write_item(bio, *bin_data, *object_type, *object_size, *array_length);
}

int binff_write_shape(BinF_IO* bio, BINF_INT** shape, uint16_t *shape_length) {
    // printf("ptr: %p length:%d\n", *shape, *shape_length);
    // int i;
    // for(i=0;i<*shape_length;i++) printf("%ld,",(*shape)[i]); 
    // printf("\n");
    return binf_write_shape(bio, *shape, *shape_length);
}

// ---------------------------------------------------------------------

// scalar values are already stored in BinF struct
int binff_read_char(BinF *b, char *val) {
    *val = *(char*)b->val;
    return 0;
}


// host and data LE (little endian)
// - use leXXtoh() form endian.h XX=64,32,16
int binff_read_uint(BinF *b, uint64_t *val) {
    int ret=0;
    switch (b->object_size) {
        case 1: *val = *(uint8_t* )b->val; break;
        case 2: *val = *(uint16_t*)b->val; break;
        case 4: *val = *(uint32_t*)b->val; break;
        case 8: *val = *(uint64_t*)b->val; break;
        default: ret=-1;
    }
    return ret;
}

int binff_read_int(BinF *b, int64_t *val) {
    int ret=0;
    switch (b->object_size) {
        case 1: *val = *(int8_t* )b->val; break;
        case 2: *val = *(int16_t*)b->val; break;
        case 4: *val = *(int32_t*)b->val; break;
        case 8: *val = *(int64_t*)b->val; break;
        default: ret=-1;
    }
    return ret;
}

int binff_read_double(BinF *b, double *val) {
    int ret=0;
    switch (b->object_size) {
        case 4: *val = *(float* )b->val; break; 
        case 8: *val = *(double*)b->val; break;
        default: ret=-1;
    }
    return ret;
}

int binff_read_double_complex(BinF *b, double complex *val) {
    int ret=0;
    switch (b->object_size) {
        case  8: *val = *(float complex* )b->val; break; 
        case 16: *val = *(double complex*)b->val; break;
        default: ret=-1;
    }
    return ret;
}

// read n-char string
int binff_read_nchar(BinF_IO* bio, BinF *b, char *buf) {
    return BINF_READ(bio,buf,b->total_size);
}

// ------------------------------------------------------------------------------
// arrays rules (gfortran 9,10)
// - read binary stream into memory, number of bytes given by total_size
// - fortran array is sent as **ptr and real memory space starts at *ptr
int binff_read_nuint8(BinF_IO* bio, BinF *b, uint8_t **ary) {
    int ret=0;
    // printf("binff_read_nuint8: ary:%p *ary:%p\n", ary, *ary);
    ret = BINF_READ(bio , *ary, b->total_size);
    return ret;
}