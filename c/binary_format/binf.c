
// c interface for binary_format

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <float.h>
#include <math.h>
#include <unistd.h>  // read write - basic reader/write
#include <complex.h>

#include "binf.h"

// C90 standard

// to disable fprintf
#ifdef BINF_NOFPRINTF
    #define fprintf(x,fmt,...) (0)
#endif


// We define two basic IO:
//  - binf_uni_io    - tied with open read write
//  - binf_std_io    - tied with fopen fread fwrite


// binf_uni_io
// ==============
// binf_uni_io uses functions:
//  - int open(const char *pathname, int flags, mode_t mode); - returns handler or -1 on error
//  - int close(int fd);                                      - returns if ok 0 or -1 on error
//  - ssize_t write(int fd, const void *buf, size_t count);   - returns number of written bytes or -1 on error
//  - ssize_t read(int fd, void *buf, size_t count);          - returns number of read bytes or -1 is error

ssize_t binf_uni_writer(void *data, const void *buf, size_t count) {
    int fh = *(int*)data;
    // printf("binf_uni_writer data: %p  fh: %d\n", data, fh);
    return write(fh,buf,count);
}

ssize_t binf_uni_reader(void *data, void *buf, size_t count) {
    int fh = *(int*)data;
    // printf("binf_uni_reader data: %p  fh: %d\n", data, fh);
    return read(fh,buf,count);
}

// exmaple how use data pointer to store extra info
void binf_uni_init(BinF_IO *bio, int *fh) { 
    bio->data = fh;
    bio->reader=binf_uni_reader; 
    bio->writer=binf_uni_writer; 
}

// binf_std_io
// ==============
// binf_std_io uses functions:
//  - FILE *fopen(const char *restrict pathname, const char *restrict mode); - returns file pointer of NULL on error
//  - int fclose(FILE *stream)                                               - returns 0 or EOF(-1) on error
//  - size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)- returns total number of nmebs
//  - size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream)       - returns total number of nmebs

//  - int fopen(const char *pathname, int flags, mode_t mode); - returns handler or -1 on error
//  - ssize_t write(int fd, const void *buf, size_t count);    - returns number of written bytes >=0 or -1 on error
//  - ssize_t read(int fd, void *buf, size_t count);           - returns number of read bytes >=0, s EOF or -1 is error

ssize_t binf_std_writer(void *data, const void *buf, size_t count) {
    FILE *FH = (FILE*)data;
    // printf("binf_std_writer data: %p  FH: %d\n", data, FH);
    return fwrite(buf,1,count,FH);
}

ssize_t binf_std_reader(void *data, void *buf, size_t count) {
    FILE *FH = (FILE*)data;
    // printf("binf_std_reader data: %p  FH: %d\n", data, FH);
    return fread(buf,1,count,FH);
}

// exmaple how use data pointer to store extra info
void binf_std_init(BinF_IO *bio, FILE *FH) { 
    bio->data = FH;
    bio->reader=binf_std_reader; 
    bio->writer=binf_std_writer; 
}



// ------------------------------------------------------------------------------
// - only for private logging
// void binf_printf(BinF *b) {
//     printf("object_type:%c isarray:%d object_size:%d length:%ld total_size:%ld\n",
//         b->object_type,b->isarray,b->object_size,b->length,b->total_size);
// }

// binf optional header, bi-endian
int binf_write_header(BinF_IO* bio) {
    if(BINF_WRITE(bio,"binF2022",8) != 8) return -1;
    return 0;
}

// binf uinif size in bits
// - check in your project if max allowed size is as you expect
// - returns 64 | 32 | 16
int binf_int_size() {
    return sizeof(BINF_INT)*8;
}

// endianess for control fields, that are always in LE
#if defined(BINF_SSIZE16)
    #define HTOLE  htole16
    #define LETOH  le16toh
#elif defined(BINF_SSIZE32)
    #define HTOLE  htole32
    #define LETOH  le32toh
#elif defined(BINF_SSIZE64)
    #define HTOLE  htole64
    #define LETOH  le64toh
#else
    #error "undefined macro BINF_USIZEXX, specify as one of BINF_SSIZE16 | BINF_SSIZE32 | BINF_SSIZE64 "
#endif


// binary log2 - highest bit
// how to get log2:
// c       log(x) { log2 = 0; while (x >>= 1) ++log2; }
// c++11   constexpr std::uint32_t log2(std::uint32_t n) noexcept { return (n > 1) ? 1 + log2(n >> 1) : 0; }
// c++20   std::bit_width(index) - 1;

//   - log2(0)=-1
//   - log2(1)=0 log2(8)=3 log2(256)=8 ...    but log2(10)=3
// note: UINT64_C(0) - macro expand to eg 0UUL - depends on compiler, uint64_t is not working in this macro
#if defined(BINF_SSIZE16)
    static int uintXX_log2(uint16_t n) {
        #define S(k) if (n >= (UINT16_C(1) << k)) { i += k; n >>= k; }
        int i = -(n == 0); S(8); S(4); S(2); S(1); return i;
        #undef S
    }
#elif defined(BINF_SSIZE32)
    static int uintXX_log2(uint32_t n) {
        #define S(k) if (n >= (UINT32_C(1) << k)) { i += k; n >>= k; }
        int i = -(n == 0); S(16); S(8); S(4); S(2); S(1); return i;
        #undef S
    }
#elif defined(BINF_SSIZE64)
    static int uintXX_log2(uint64_t n) {
        #define S(k) if (n >= (UINT64_C(1) << k)) { i += k; n >>= k; }
        int i = -(n == 0); S(32); S(16); S(8); S(4); S(2); S(1); return i;
        #undef S
    }
// #elif defined(BINF_SSIZE128)
//     static int uintXX_log2(uint128_t n) {
//         #define S(k) if (n >= (UINT128_C(1) << k)) { i += k; n >>= k; }
//         int i = -(n == 0); S(64); S(32); S(16); S(8); S(4); S(2); S(1); return i;
//         #undef S
#else
    #error "undefined macro BINF_USIZEXX, specify as one of BINF_SSIZE16 | BINF_SSIZE32 | BINF_SSIZE64 "
#endif

// object size - 8 16 32 64 ... power of 2
// return  0  - succes
//        !=0 - error
int binf_write_item(BinF_IO* bio, uint8_t* bin_data, uint8_t object_type, uint16_t object_size, BINF_INT array_length) {
    // printf("ptr:%p type:%c size:%d length:%ld\n",bin_data,object_type,object_size,array_length);
    if(object_size==0) return -1;
    uint8_t object_size_u8 = uintXX_log2(object_size);  // get power of 2
    uint8_t length_size_u8 = 0;
    if(array_length > 0) {
        length_size_u8 = uintXX_log2(array_length)/8+1;  // min memory space for array length
    }

    // - we store object size and length size in one byte
    //   - object size is nonzero value [   2^0 to 2^15]
    //   - length size may be also zero [0, 2^0 to 2^14]
    //      - 0 means scalar value, 
    //      - non zero means array, next item specify length of array
    //        - arrays up to 255=2^8-1 items has length size equal 1
    //        - arrays up to 2^16-1 items has length size equal 2
    //        - and so on, so arrays almost of any practical size may be stored
    //   - join object size as lower half byte and array length size as upper half
    uint8_t size_u8 = (object_size_u8 & 0x0f) | ((length_size_u8 & 0x0f)<<4);

    // printf("object_size_u8=0x%X length_size_u8=0x%X size=0x%X\n",object_size_u8, length_size_u8, size_u8);
    if(BINF_WRITE(bio, &object_type, 1) !=1) return -1;  // 1B bi-endian BIE
    if(BINF_WRITE(bio, &size_u8,     1) !=1) return -1;  // 1B bi-endian BIE

    BINF_INT byte_length=1 << object_size_u8;  // 2 ** object_size_u8
    if(byte_length != object_size) {
        fprintf(stderr,"binf_write_item: Error - byte_length differ from object size\n");
        return -1;
    }
    if(array_length) { // array
        BINF_INT array_length_le = HTOLE(array_length); // convert to LE
        if(BINF_WRITE(bio,&array_length_le,length_size_u8) != length_size_u8) return -1;  // valid only for LE - we write only part of uint64 (1 2 4 8 bytes)
        byte_length *= array_length;
    }
    if(BINF_WRITE(bio,bin_data,byte_length) != byte_length) return -1; // here we cannot check endianess - binary data

    return 0;
}

// - note - not zero ending, length given by note_length
// - no endianess
int binf_write_note(BinF_IO* bio, uint8_t *note, uint16_t note_length) {
    return binf_write_item(bio, note,     '#', 1, note_length);
}

// - var_name - not zero ending, length given by var_name_length
// - no endianess
int binf_write_varname(BinF_IO* bio, uint8_t *var_name, uint16_t var_name_length) {
    return binf_write_item(bio, var_name, 'V', 1, var_name_length);
}

// - param - not zero ending, length given by param_length
// - no endianess
int binf_write_param(BinF_IO* bio, uint8_t *param, uint16_t param_length) {
    return binf_write_item(bio, param,    'P', 1, param_length);
}

// - shape - is array                    eg [2,3,4]
// - shape_length - count of dimensions  eg 3
int binf_write_shape(BinF_IO* bio, BINF_INT* shape, uint16_t shape_length) {
    if(shape_length==0) return 0; // no shape information
    uint8_t buf[sizeof(*shape) * shape_length]; // same size as shape
    uint max_ax=0;
    int i,j,k;
    for(i=0; i < shape_length; ++i) if(shape[i] > max_ax) max_ax=shape[i]; //  find max
    uint8_t myshape_size = 1 << uintXX_log2(max_ax)/8;  // get power of 2
    
    for(i=0,k=0; i < shape_length; ++i) {
        // printf("%ld\n", shape[i]);
        BINF_INT shape_le = HTOLE(shape[i]);     // converting host to LE
        for(j=0; j < myshape_size; ++j) {
            buf[k++]=( (uint8_t*)&(shape_le) )[j];
        }
    }
    // printf("max_ax=%d  size=%d\n",max_ax, myshape_size);
    binf_write_item(bio, buf, 'D', myshape_size, shape_length);
    return 0;
}

// bio          - IO data
// bin_data     - pointer to start of binary data, user is responsible data are in LE
// object_type  - object specification (1 char) "sui..."
// object_size  - size in bytes must be power of 2, (max 2^15 - to store size in 4 bits)
// array_length - total length of array (if shape is given, should prod(shape) = array_length) - not checked
// name         - name of next item, length given by name_length
// shape        - array shape, count of dimensions, (fortran rank) given by shape_length
int binf_write(BinF_IO* bio, uint8_t* bin_data, uint8_t object_type, uint16_t object_size, BINF_INT array_length, 
                uint8_t *name, uint16_t name_length, BINF_INT* shape, uint16_t shape_length) {
    if(name_length) {
        if(binf_write_varname(bio,name,name_length)!=0) return -1;
    }
    if(shape_length) {
        if(binf_write_shape(bio,shape, shape_length)!=0) return -1;
        // check prod(shape) == array_length - not checked, user responsibility
    }
    return binf_write_item(bio, bin_data, object_type, object_size, array_length);
}

// -----------------------------------------------------------------------------------------------------

// return  0  - is binf header
//       <>0  - not binf header 
// returns:     -2 -1 0 1
int binf_read_header(BinF_IO* bio) {
    char buf[8];
    // printf("binf_read_header bio %p data: %p writer: %p reader: %p\n", bio, bio->data, bio->writer, bio->reader);
    if(BINF_READ(bio,buf,8) != 8) return -2;
    return strncmp(buf,"binF2022",8);
}

// ----------------------------------------------------------------------------------------------------
// read one item
// | object type | size |
int binf_read_item(BinF_IO* bio, BinF *b) {
    size_t r;
    if((r=BINF_READ(bio,&b->object_type,1)) != 1) return -1;            // cannot read data or EOF

    uint8_t size;
    if(BINF_READ(bio,&size,1)!=1) return -1;                            // bi-endian
    b->object_size = 1 << (size & 0x0f);  // 2**n = 1<<n
    uint16_t length_size = (size & 0xf0) >> 4;
    BINF_INT length_le;
    
    if(length_size!=0) {
        length_size = 1 << (length_size-1); // 2**(length_size-1)
    }

    if(b->object_size > sizeof(b->val) || length_size > sizeof(b->length)) { // check size of val and length in BinF
        fprintf(stderr, "binf_read_item: Error - overflow in object_size or length_size\n");
        return -1;
    }

    if(length_size == 0) {               //  scalar value
        b->isarray=0;
        if(strchr("suifc", b->object_type) != NULL) {
            if(BINF_READ(bio,b->val,b->object_size) != b->object_size) return -1;
        }
        else {
            fprintf(stderr, "binf_read_item: Error - unsuported scalar [%c]\n", b->object_type);
            return -1;
        }
    }
    else {
        b->isarray=1;
        // b->length=0; 
        length_le=0;
        if(BINF_READ(bio,&length_le,length_size) != length_size) return -1; // data-in are LE
        b->length = LETOH(length_le);        // convert little endian to host representation
        b->total_size = b->length * b->object_size;
    }
    // binf_printf(b);
    return 0;
}

// general reading
// - endianes of binary data are not affected (should be LE) 
int binf_read(BinF_IO* bio, BinF *b, BinF_VD *vd) {
    int i;
    memset(b, 0, sizeof(BinF));             // clear data
    memset(vd, 0, sizeof(BinF_VD));         // clear data
    for(;;) {
        if(binf_read_item(bio, b) < 0) return -1;
        switch (b->object_type) {
            case 'V': // variable name
                if(b->total_size > sizeof(vd->name)) {
                    fprintf(stderr, "binf_read: Error - Variable name is too long\n");
                    return -1;
                }
                if(BINF_READ(bio,vd->name,b->total_size)!=b->total_size) return -1; // string without endianess
                vd->name_length=b->total_size;
                break;
            case 'D': // dimension
                if(b->total_size > sizeof(vd->shape)) {
                    fprintf(stderr, "binf_read: Error - Shape array is too big\n");
                    return -1;
                }
                // assuming little endian and vd.shape is zero filled
                // we use uint64 in vd.shape as union type for uint8 uint16 uint32 uint64 - valid only for little endian
                for(i=0;i < b->length;i++) {
                    BINF_INT shape_le=0;
                    if(BINF_READ(bio, &shape_le, b->object_size)!=b->object_size) return -1;  // stored as LE
                    vd->shape[i]=LETOH(shape_le);           
                }
                vd->shape_length = b->length;
                break;
            default:
                return 0;
                break;
        }
    }
    return 0;
}
