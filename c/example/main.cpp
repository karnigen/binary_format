
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>   // open
#include <unistd.h>  // read 

#include <complex>
#include <memory>

#include "binary_format/binf.h"


// #include <armadillo>
// #include <fmt/ostream.h>

int check_writer(BinF_IO *bio) {
    int i;
    if( binf_write_header(bio) != 0) {
        fprintf(stderr, "Error: cannot write binF header\n");
        return -1;
    }


    BINFW_PARAM(bio,"data:main");
    BINFW_NOTE(bio,"BINF Hello note");

    char s='w';
    BINFW_SCALARN(bio,s,'s',"A");

    uint8_t  u8=1;
    BINFW_SCALARN(bio, u8,'u',"B");
    uint16_t u16=11;
    BINFW_SCALARN(bio,u16,'u',"C");
    uint32_t u32=1111;
    BINFW_SCALARN(bio,u32,'u',"D");
    uint64_t u64=11111111;
    BINFW_SCALARN(bio,u64,'u',"E");

    int8_t  i8=-1;
    BINFW_SCALARN(bio, i8,'i',"F");
    int16_t i16=-11;
    BINFW_SCALARN(bio,i16,'i',"G");
    int32_t i32=-1111;
    BINFW_SCALARN(bio,i32,'i',"H");
    int64_t i64=-11111111;
    BINFW_SCALARN(bio,i64,'i',"I");

    float  f4=-3.1415;
    BINFW_SCALARN(bio, f4,'f',"J");
    double f8=-3.1415;
    BINFW_SCALARN(bio, f8,'f',"K");

    std::complex<float> c8=-6-7i;
    BINFW_SCALARN(bio, c8,'c',"L");
    std::complex<double> c16=-8-9i;
    BINFW_SCALARN(bio, c16,'c',"M");


    int N=6;
    uint8_t u8n[N];
    for(i=0;i<N;++i) u8n[i]=i+10;
    BINFW_ARRAYN(bio, u8n, 'u', "N");

    uint16_t u16n[N];
    for(i=0;i<N;++i) u16n[i]=i+20;
    BINFW_ARRAYN(bio, u16n, 'u', "O");

    uint32_t u32n[N];
    for(i=0;i<N;++i) u32n[i]=i+30;
    BINFW_ARRAYN(bio, u32n, 'u', "P");

    uint64_t u64n[N];
    for(i=0;i<N;++i) u64n[i]=i+40;
    BINFW_ARRAYN(bio, u64n, 'u', "Q");

    // --------------------------------------------------
    int8_t i8n[N];
    for(i=0;i<N;++i) i8n[i]=i-N/2;
    BINFW_ARRAYN(bio, i8n, 'i', "R");

    int16_t i16n[N];
    for(i=0;i<N;++i) i16n[i]=i-N/2;
    BINFW_ARRAYN(bio, i16n, 'i', "S");

    int32_t i32n[N];
    for(i=0;i<N;++i) i32n[i]=i-N/2;
    BINFW_ARRAYN(bio, i32n, 'i', "T");

    int64_t i64n[N];
    for(i=0;i<N;++i) i64n[i]=i-N/2;
    BINFW_ARRAYN(bio, i64n, 'i', "U");

    // --------------------------------------------------------
    float f4n[N];
    for(i=0;i<N;++i) f4n[i]=i-N/2;
    BINFW_ARRAYN(bio, f4n, 'f', "V");

    double f8n[N];
    for(i=0;i<N;++i) f8n[i]=1000.*(i-N/2);
    BINFW_ARRAYN(bio, f8n, 'f', "X");

    // --------------------------------------------------------
    std::complex<float> c8n[N];
    for(i=0;i<N;++i) c8n[i]=std::complex<float>(-N/2, - (i-N/2));
    BINFW_ARRAYN(bio, c8n, 'c', "V");

    std::complex<double> c16n[N];
    for(i=0;i<N;++i) c16n[i]=100. * std::complex<double>(i-N/2, - (i-N/2));
    BINFW_ARRAYN(bio, c16n, 'c', "V");

    // shape
#ifdef BINF_SSIZE64
    int64_t shape[2]={3,2};   // x=3 y=2 fortran memory layout
    BINFW_ARRAYND(bio, u8n, 'u', "N", shape, 2);
#endif
    return 0;
}



int check_reader(BinF_IO *bio) {
    if( binf_read_header(bio) != 0) {
        fprintf(stderr, "Error: not binF header\n");
        return -1;
    }

    int64_t bufn=1024;
    char *buf = (char*)malloc(bufn);
    std::shared_ptr<char> bufh(buf,free); // auto free

    BinF b;
    BinF_VD vd;

    for(int j=0;;j++) {
        int r = binf_read(bio, &b, &vd);
        if(r<0) break;  // end of reading
        printf("j=%2d r=%2d %c ",j,r, b.object_type);

        printf("*** ");
        if(vd.name_length) printf("Name: %*s, ", vd.name_length, vd.name);
        if(vd.shape_length) {
            printf("*** Shape:[");
            #ifdef BINF_SSIZE64
                for(int i=0;i<vd.shape_length;i++) printf("%ld,", vd.shape[i]);
            #else
                for(int i=0;i<vd.shape_length;i++) printf("%d,", vd.shape[i]);
            #endif
            printf("] ");
        }

        // printf("%c\n", b.object_type);
        switch (b.object_type) {
        case '#':
        case 's':
        case 'P':
            if(b.isarray==0) { // scalar
                printf("val=%c\n",*(char*)b.val);
            }
            else { // array
                if(b.total_size>bufn) {
                    printf("Warn: buffer too small, realocating.\n");
                    bufn=b.total_size+1024;
                    buf=(char*)realloc(buf,bufn);
                }
                if(BINF_READ(bio,buf,b.total_size)!=b.total_size) return -1;
                printf("%.*s\n",(int)b.total_size, buf);
            }
            break;
        
        case 'i':
            if(b.isarray==0) { // not array - value already in b.val
                switch (b.object_size) {
                case 1: printf("val=%d\n", *(int8_t* )b.val); break;
                case 2: printf("val=%d\n", *(int16_t*)b.val); break;
                case 4: printf("val=%d\n", *(int32_t*)b.val); break;
                case 8: printf("val=%ld\n",*(int64_t*)b.val); break;
                
                default: 
                    fprintf(stderr,"i bad size\n"); 
                    return -1;
                }

            }
            else {  // array
                uint8_t *ary = (uint8_t*)malloc(b.total_size);
                std::shared_ptr<uint8_t> aryh(ary,free);  // auto free
                if(BINF_READ(bio,ary,b.total_size)!=b.total_size) return -1;  // read data
                printf("val=[");
                for(unsigned i=0;i<b.length;i++) {
                    switch (b.object_size) {
                    case 1: printf("%d,",((int8_t*)ary)[i]); break;
                    case 2: printf("%d,",((int16_t*)ary)[i]); break;
                    case 4: printf("%d,",((int32_t*)ary)[i]); break;
                    case 8: printf("%ld,",((int64_t*)ary)[i]); break;
                    
                    default: 
                        fprintf(stderr,"i bad size\n"); 
                        return -1;
                    }
                }
                printf("]\n");
            }
            break;
        case 'u':
            if(b.isarray==0) { // not array - value already in b.val
                printf("val=%ld\n",*(uint64_t*)b.val);
            }
            else {  // array
                uint8_t *ary = (uint8_t*)malloc(b.total_size);
                std::shared_ptr<uint8_t> aryh(ary,free);    // auto free
                if(BINF_READ(bio,ary,b.total_size)!=b.total_size) return -1;  // read data
                printf("val=[");
                for(unsigned i=0;i<b.length;i++) {
                    switch (b.object_size) {
                    case 1: printf("%d,", ((uint8_t* )ary)[i]); break;
                    case 2: printf("%d,", ((uint16_t*)ary)[i]); break;
                    case 4: printf("%d,", ((uint32_t*)ary)[i]); break;
                    case 8: printf("%ld,",((uint64_t*)ary)[i]); break;
                    
                    default: 
                        fprintf(stderr,"u bad size\n"); 
                        return -1;
                    }
                }
                printf("]\n");
            }
            break;
        case 'f':
            if(b.isarray==0) { // not array - value already in b.val
                if(b.object_size==4)
                    printf("val float=%f\n",  *(float* )b.val);
                else if(b.object_size==8)
                    printf("val double=%lf\n",*(double*)b.val);
                else {
                    fprintf(stderr, "Error: float bad size %d\n", b.object_size);
                    return -1;
                }
            }
            else {
                uint8_t *ary = (uint8_t*)malloc(b.total_size);
                std::shared_ptr<uint8_t> aryh(ary,free);    // auto free
                if(BINF_READ(bio,ary,b.total_size)!=b.total_size) return -1;  // read data
                printf("val=[");
                for(unsigned i=0;i<b.length;i++) {
                    switch (b.object_size) {
                    case 4: printf("%f,", ((float* )ary)[i]); break;
                    case 8: printf("%lf,",((double*)ary)[i]); break;
                    
                    default: 
                        fprintf(stderr,"bad size\n"); 
                        return -1;
                    }
                }
                printf("]\n");
            }
            break;
        case 'c':
            if(b.isarray==0) { // not array - value already in b.val
                if(b.object_size==8)
                    printf("val std::complex<float>=%f%+fi\n",   (*(std::complex<float>*)b.val).real(),(*(std::complex<float>*)b.val).imag());
                else if(b.object_size==16)
                    printf("val std::complex<double>=%lf%+lfi\n",(*(std::complex<double>*)b.val).real(),(*(std::complex<double>*)b.val).imag());
                else {
                    fprintf(stderr, "Error: complex bad size %d\n", b.object_size);
                    return -1;
                }
            }
            else {
                uint8_t *ary = (uint8_t*)malloc(b.total_size);
                std::shared_ptr<uint8_t> aryh(ary,free);         // auto free
                if(BINF_READ(bio,ary,b.total_size)!=b.total_size) return -1;  // read data
                printf("val=[");
                for(unsigned i=0;i<b.length;i++) {
                    switch (b.object_size) {
                    case 8: printf("%f%+fi,",   (((std::complex<float>*)ary)[i]).real(),(((std::complex<float>*)ary)[i]).imag() ); break;
                    case 16: printf("%lf%+lfi,",(((std::complex<double>*)ary)[i]).real(),(((std::complex<double>*)ary)[i]).imag()); break;
                    
                    default: 
                        fprintf(stderr,"bad size\n"); 
                        return -1;
                    }
                }
                printf("]\n");
            }
            break;

        default:
            printf("Error: unknown object_type=%c\n", b.object_type);
            return -1;
            break;
        }

        // if(j>=15) break;
    }

    return 0;
}

// ######################################################################################
// - uni io open/read/write
int test_uni_io() {
    const char *fn = "x010_data_cpp_uni.binf";

    // uni IO for binf format
    BinF_IO bio;
    {
        int fo = open(fn,O_WRONLY|O_CREAT|O_TRUNC,0644); // create and truncate !!!
        if(fo<0) {
            fprintf(stderr,"Error cannot open file: %s\n", fn);
            return -1;
        }
        binf_uni_init(&bio,&fo);        // basic IO for binf format
        int retw = check_writer(&bio);
        close(fo);
        if(retw<0) {
            fprintf(stderr,"Error in check_writer: %d\n", retw);
            return -1;
        }
    }

    {
        int fi = open(fn,O_RDONLY);
        if(fi<0) {
            fprintf(stderr,"Error cannot open file: %s\n", fn);
            return -1;
        }
        binf_uni_init(&bio,&fi);
        int retr = check_reader(&bio);
        close(fi);
        if(retr<0) {
            fprintf(stderr,"Error in check_reader: %d\n", retr);
            return -1;
        }
    }
    return 0;
}

// ######################################################################################
// - std io - fopen,fwrite,fread
int test_std_io() {
    const char *fn = "x010_data_cpp_std.binf";

    // basic IO for binf format
    BinF_IO bio;
    {
        FILE *fp = fopen(fn,"wb");  // create and truncate, binary !!!
        if(fp == nullptr) {
            fprintf(stderr,"Error cannot open file: %s\n", fn);
            return -1;
        }
        std::shared_ptr<FILE> fh(fp,fclose);  // auto close
        binf_std_init(&bio,fp);
        int retw = check_writer(&bio);
        if(retw<0) {
            fprintf(stderr,"Error in check_writer: %d\n", retw);
            return -1;
        }
    }

    {
        FILE *fp = fopen(fn,"rb");   // binary
        if(fp == nullptr) {
            fprintf(stderr,"Error cannot open file: %s\n", fn);
            return -1;
        }
        std::shared_ptr<FILE> fh(fp,fclose); // auto close
        binf_std_init(&bio,fp);
        int retr = check_reader(&bio);
        if(retr<0) {
            fprintf(stderr,"Error in check_reader: %d\n", retr);
            return -1;
        }
    }
    return 0;
}

// ######################################################################################
// - use of external compression xz
//   - popen/pclose
int test_lzma_read() {
    BinF_IO bio;
    const char *fn = "x010_data_cpp_zip.binf.xz";
    const int N=256;
    char buf[N];
    int ret;
    // ----------------------------------------------
    ret = snprintf(buf,N,"xz -z - >%s",fn);   // -z compress, '-' stdin, '>' redirect
    if(ret>=N) {
        fprintf(stderr,"Error too long for buf[%d] length:%d str:\"%s\"\n", N, ret, buf);
        return -1;
    }
    {
        FILE *fp = popen(buf, "w");
        // printf("FD:%p cmd:%s\n", fp, buf);
        if(fp == NULL) {
            fprintf(stderr,"Error cannot open file: %s\n", buf);
            return -1;
        }
        std::shared_ptr<FILE> fh(fp,pclose);  // auto close
        binf_std_init(&bio,fp);        // std io 
        int retw = check_writer(&bio);
        if(retw<0) {
            fprintf(stderr,"Error in check_writer: %d\n", retw);
            return -1;
        }
    }

    // ----------------------------------------------
    ret = snprintf(buf,N,"xz -dc %s",fn);   // -d decompress
    if(ret>=N) {
        fprintf(stderr,"Error too long for buf[%d] length:%d str:\"%s\"\n", N, ret, buf);
        return -1;
    }
    {
        FILE *fp = popen(buf, "r");
        // printf("FD:%p %s\n", fp, buf);
        if(fp==NULL) {
            fprintf(stderr,"Error cannot open file: %s\n", buf);
            return -1;
        }
        std::shared_ptr<FILE> fh(fp,pclose);  // auto close
        binf_std_init(&bio,fp);
        int retr = check_reader(&bio);
        if(retr<0) {
            fprintf(stderr,"Error in check_reader: %d\n", retr);
            return -1;
        }
    }
    return 0;
}


int main(int /*argc*/, char*[] /*args*/) {
    printf("Binf library int size:%d\n", binf_int_size());
    test_uni_io();
    test_std_io();
    test_lzma_read();
    return 0;
}