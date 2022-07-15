#ifndef _BINARY_FORMAT_H_
#define _BINARY_FORMAT_H_

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "binf.hpp"    // constant definitions

#ifdef __cplusplus
extern "C" {
#endif

// #define FIELD_SIZEOF(t, f) (sizeof(((t*)0)->f))

// for different os you can specify max size for uint
// - suggestion: define signed size BINF_SSIZE16|BINF_SSIZE32 only in your mbed part if 64bit size is not supported or reasonable
// - signed size is required for read/write where -1 means error operation
#if defined(BINF_SSIZE16)
    #define BINF_INT  int16_t
#elif defined(BINF_SSIZE32)
    #define BINF_INT  int32_t
// #elif defined(BINF_SSIZE128)
//     #define BINF_INT  int128_t
#else
    #define BINF_SSIZE64
    #define BINF_INT  int64_t
#endif


// basic info for each read object
struct BinF {
    uint8_t   object_type;           // type one of "suifc #PVD"
    uint8_t   isarray;               // flag for array=1 scalar=0
    uint16_t  object_size;           // size of one element up to 32 B 
    BINF_INT length;                 // length of array - count of elements
    BINF_INT total_size;             // = length * object_size in bytes for arrays
    uint8_t   val[BINF_VAL_LENGTH];  // buffer for scalar values
};
typedef struct BinF BinF;           // enable BinF instead of struct BinF


// extra info for data V-name D-dimension
// some restrition of implementation
//   - max length of variable name id 256 
//   - max dimension count is 64
struct BinF_VD {
    uint16_t  name_length;                        // length of var name, 0 means name is not used
    uint8_t   name[BINF_VD_MAX_NAME_LENGTH];      // variable name 'V'
    uint16_t  shape_length;                       // count of dimension 0 means shape is not used
    BINF_INT shape[BINF_VD_MAX_SHAPE_COUNT];     // shape
};
typedef struct BinF_VD BinF_VD;                  // enable BinF_VD instead of struct BinF_VD

// IO interface, we need two functions to read/write and some extra data to send to this functions
// - user defined I/O function must of type int(*)(void*,void*,size_t) and returns read/writen bytes >=0 or -1 on error
struct BinF_IO {
    void *data;
    ssize_t (*writer)(void *data, const void *buf, size_t count);
    ssize_t (*reader)(void *data, void *buf, size_t count);
};
typedef struct BinF_IO BinF_IO;                  // enable BinF_IO instead of struct BinF_IO

// low level write, for file io it is just write(file_handle,buffer,count)
#ifndef BINF_WRITE
    #define BINF_WRITE(bio,buf,count) bio->writer(bio->data,buf,count)
#endif

// low level read, for file io it is just read(file_handle,buffer,count)
#ifndef BINF_READ
    #define BINF_READ(bio,buf,count) bio->reader(bio->data,buf,count)
#endif

// assign read/write to bio and int filehandle fh
void binf_uni_init(BinF_IO *bio, int *fh);
// assign fread/fwrite to bio and FILE* filehandle FH
void binf_std_init(BinF_IO *bio, FILE *FH);

//----------------------------------------------------------------------------------------------

int binf_int_size();
int binf_write_header(BinF_IO* bio);
int binf_write(BinF_IO* bio, uint8_t* bin_data, uint8_t object_type, uint16_t object_size, BINF_INT array_length, 
                uint8_t *name, uint16_t name_length, BINF_INT* shape, uint16_t shape_length);
int binf_write_item(BinF_IO* bio, uint8_t* bin_data, uint8_t object_type, uint16_t object_size, BINF_INT array_length);

int binf_write_shape(BinF_IO* bio, BINF_INT* shape, uint16_t shape_length);
int binf_write_note(BinF_IO* bio, uint8_t *note, uint16_t note_length);
int binf_write_varname(BinF_IO* bio, uint8_t *var_name, uint16_t var_name_length);
int binf_write_param(BinF_IO* bio, uint8_t *param, uint16_t param_length);

int binf_read_header(BinF_IO* bio);
int binf_read(BinF_IO* bio, BinF *b, BinF_VD *vd);

// param P
#define BINFW_PARAM(bio,x)   binf_write_param(bio,(uint8_t*)x,strlen(x));
// note #
#define BINFW_NOTE(bio,x)    binf_write_note (bio,(uint8_t*)x,strlen(x));
// varname 'V'
#define BINFW_VARNAME(bio,x) binf_write_varname (bio,(uint8_t*)x,strlen(x));

// scalar
#define BINFW_SCALAR(bio,x,type)       binf_write(bio,(uint8_t*)(&x),type,sizeof(x),0,0,0,0,0)
// scalar with name
#define BINFW_SCALARN(bio,x,type,name) binf_write(bio,(uint8_t*)(&x),type,sizeof(x),0,(uint8_t*)name,strlen(name),0,0)

// array
#define BINFW_ARRAY(bio,x,type)       binf_write(bio,(uint8_t*)(x),type,sizeof(*(x)),sizeof(x)/sizeof(*(x)),0,0,0,0)
// array with name
#define BINFW_ARRAYN(bio,x,type,name) binf_write(bio,(uint8_t*)(x),type,sizeof(*(x)),sizeof(x)/sizeof(*(x)),(uint8_t*)name,strlen(name),0,0)
// array with name and shape
#define BINFW_ARRAYND(bio,x,type,name,shape,ndim) \
    binf_write(bio,(uint8_t*)(x),type,sizeof(*(x)),sizeof(x)/sizeof(*(x)),(uint8_t*)name,strlen(name),shape,ndim)


#ifdef __cplusplus
}  // extern C
#endif


#endif  // _BINARY_FORMAT_H_
