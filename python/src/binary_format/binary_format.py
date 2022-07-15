#!/usr/bin/env python3

# Copyright 2022 Tony Karnigen

import numpy as np

# Basics
# - memory layout is fortran and don't change it for speed reasons
# - dimesions are for fortran memory layout - xyz in Python[zyx] Fortran[xyz] C,C++[arma,eigen:xyz|zyx] ...
# - endianess - little endian LE
# - alignment - not aligned NA

# header: optional
#  - "binF2022"

# file extension: .binf

# Data types
#  scalars and arrays (flag in array length)
#    s ascii character (for unicode bytes etc use raw u8)
#    i signed integer
#    u unsigned integer
#    f float  (f32 or f64)
#    c complex (size c32 is 2 x f16 for real and complex part)

# other:
#    # note of row data, of any type
#    P - ascii string private data, eg to control creation of complex data
#
#    V - variable name of next data as ascii string    - optional
#    D - dimension of uintXX in fortran order [xyz...] - optional

# format
#  1. 1B object_type [ascii char 1B]     - eg: siufc #PVD
#  2. 1B object_size_log2 [lower 4bits] with length_size_log2 for array [higher 4bits]
#        - object size [bytes] = 2**object_size_log2 
#                                eg. (2^0=1 2^1=2 2^2=4 2^3=8 2^4=16 ... 2^15)
#        - if object_length_size_log2 == 0: scalar value
#        - if object_length_size_log2  > 0: length_size [bytes] = 2**(object_length_size_log2-1) 
#          - length size is storage size of length for array
#            for array with up to 255       elements is length size=1B
#                                 256*256-1 elements is length size=2B etc.
#  3. scalar - missing
#     arrays - [1B 2B 4B ....] array length as 1D array, shape is given by 'V' type

def binf_log(msg): return
# def binf_log(msg): print(msg)


def write_header(fo):
    fo.write(bytes("binF2022",'ascii'))

# - object_size - in bytes 1 2 4 8 ... 2^n
#   array_length - count of elements, 
#    - total size in bytes
#        for scalar = object_size (array_length is 0)
#        for array  = object_size * array_length
def write_item(fo, bin_data, object_type, object_size, array_length=0):
    binf_log(f"write object_type={object_type} object_size={object_size} array_length={array_length} bin_data={bin_data}")
    object_type_u8 = np.uint8(ord(object_type))
    object_size_low = int(np.log2(object_size))  # get power of 2
    length_size_high = 0
    if array_length > 0:
        length_size_high = int(np.log2(array_length)/8)+1  # min memory space for array length

    # join object size with object length size
    size_u8 = np.uint8( (object_size_low & 0x0f) | ((length_size_high & 0x0f)<<4) )

    fo.write(object_type_u8)
    fo.write(size_u8)
    if array_length:                                   # for array write numer of elements
        length = np.zeros(1, dtype=f"u{length_size_high}") # dynamic size uint8 16 32 ...
        length[0]=array_length                         # put array length into 
        fo.write(length)                               # write array size
    fo.write(bin_data)                                 # write binary data

# write ascii string for - s#V types
def write_str(fo, i, object_type):
    ss=bytes(i,'ascii')
    write_item(fo, ss, object_type, 1, len(ss))

# shape as numpy - 'D' type
def write_shape(fo, shape):
    myshape_size = 2**int(np.log2( np.max(shape)  ) / 8)  # size for shape
    myshape = np.zeros(len(shape), dtype=f"u{myshape_size}")
    myshape[:]=np.flip(shape)   # numpy is strongly C order xyz we need to swap coord to get Fortran zyx here
    write_item(fo, myshape.data, 'D', myshape.itemsize, myshape.size)

def write(fo, i, var_name=None):
    if var_name is not None:
        ss=bytes(var_name,'ascii')
        write_item(fo, ss, "V", 1, len(ss))
    if isinstance(i, (np.generic,np.ndarray)):  # is numpy type
        object_type = str(i.dtype)[0]
        if object_type not in "uifc":
            raise Exception(f"Unknown numpy type: {i.dtype}")
        if np.isscalar(i):
            write_item(fo, i.data, object_type, i.itemsize)
        else:
            if i.ndim > 1:  # shape only for 2D and more dimensions
                write_shape(fo, i.shape)
            write_item(fo, i.data, object_type, i.itemsize, i.size)
    elif isinstance(i, str):
        write_str(fo, i, "s")
    elif isinstance(i, bytes):
        write_item(fo, i, "u", 1, len(i))
    else:
        raise Exception(f"Unknown data type: {type(i)}")


# add note into data
# - any data may be manually set as not and ignored by read
def write_note(fo, msg):
    write_str(fo, msg, "#")

# param - any string 
#   - for binf format it may looks like "--binF='F LE NA'" - Fortran order LE little endian NA not aligned
#   - user may specify his own switches
def write_param(fo, par):
    write_str(fo, par, "P")

# -------------------------------------------------------------------         
def read_header(fi):
    ss = fi.read(8)
    s = str(ss,'ascii')
    if s != "binF2022":
        raise Exception(f"Not binF format: {s}")

# np.fromfile - is not working with compress modules but should be more effective, we not use it
# return: value, object_type, object_size, length, position
#  - position from beginnig of file, only for arrays
def read_item(fi, skip_arrays=False):
    r=fi.read(1)
    if len(r)!=1: return [None] * 5
    object_type = str(r,'ascii')

    size = int.from_bytes(fi.read(1), "little")
    object_size = 2**(size & 0x0f)
    length_size = (size & 0xf0) >> 4
    binf_log(f"read object_type={object_type}, object_size={object_size}, length_size={length_size}")

    if length_size:
        length_size = 2**(length_size-1)

    if length_size == 0: # scalar value
        if object_type in 'uifc':
            # ii = np.fromfile(fi, dtype=f"{object_type}{object_size}", count=1)
            b = fi.read(object_size)
            ii = np.frombuffer(b, dtype=f"{object_type}{object_size}")
            return ii[0], object_type, object_size, 0,-1
        if object_type in 'sPV#':
            s=str(fi.read(1),'ascii')
            return s, object_type, object_size ,0,-1
        else:
            raise Exception(f"Unknown object type '{object_type}'")
    else:  # ndarray,string,bytes
        length = int.from_bytes(fi.read(length_size), "little")
        if object_type in 'uifc': # numeric array
            # ii = np.fromfile(fi, dtype=f"{object_type}{object_size}", count=length)
            pos = fi.tell()
            if skip_arrays == True:
                fi.seek(object_size*length,1)
                ii=[]
            else:
                b = fi.read(object_size*length)
                ii = np.frombuffer(b, dtype=f"{object_type}{object_size}")
            return ii, object_type, object_size, length, pos
        elif object_type in 'D': # dimension array
            # ii = np.fromfile(fi, dtype=f"u{object_size}", count=length)
            b = fi.read(object_size*length)
            ii = np.frombuffer(b, dtype=f"u{object_size}")
            return ii, object_type, object_size, length, -1
        elif object_type in 'sPV#': # strings
            s = str(fi.read(length),'ascii')
            return s, object_type, object_size, length, -1
        else:
            raise Exception(f"Unknown object type '{object_type}'")

# arrays=False skips array value
def reader(fi, header=True, skip_arrays=False):
    if header:
        read_header(fi)

    val_name=""
    val_shape=[]
    while True:
        value,object_type,object_size,length,file_position=read_item(fi, skip_arrays)
        if value is None:
            return
        if object_type == 'V':          # name of variable
            val_name = value
            continue
        if object_type == 'D':          # array dimension
            val_shape = value
            continue

        if len(val_shape) != 0:
            binf_log(f"var_shape={val_shape}")
            if len(value)>0:
                value = value.reshape(np.flip(val_shape))
        yield value, object_type, val_name, val_shape, object_size, length, file_position
        val_name = ""
        val_shape = []


