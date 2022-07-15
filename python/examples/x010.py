#!/usr/bin/env python3

import binary_format as binf
import numpy as np
from pathlib import Path
import lzma


def write_binf(fo):
    binf.write_header(fo)

    binf.write_param(fo, '--binf="F LE NA"')

    binf.write_note(fo, "BINF Hello note")
    s="Hello"
    binf.write(fo, s, 'A')

    binf.write_note(fo, "string:w")
    s="w"
    binf.write(fo, s, 'B')

    # numeric scalar
    binf.write_note(fo, "int8:1")
    i=np.int8(1)
    binf.write(fo, i, 'C')


    binf.write_note(fo, "uint8:2")
    i=np.uint8(2)
    binf.write(fo, i, 'D')


    binf.write_note(fo, "float32:3.0")
    i=np.float32(3.0)
    binf.write(fo, i, 'E')

    binf.write_note(fo, "float64:4.0")
    i=np.float64(4.0)
    binf.write(fo, i, 'F')

    #  --------------------------------------------------------------------
    # int array
    binf.write_note(fo, "int8[2]:[5 -6]")
    i=np.array([5,-6],dtype=np.int8)
    binf.write(fo, i, 'G')

    binf.write_note(fo, "int16[2]:[55 -66]")
    i=np.array([55,-66],dtype=np.int16)
    binf.write(fo, i, 'GG')

    binf.write_note(fo, "int32[2]:[5555 -6666]")
    i=np.array([5555,-6666],dtype=np.int32)
    binf.write(fo, i, 'GGGG')

    binf.write_note(fo, "int64[2]:[55555555 -66666666]")
    i=np.array([55555555,-66666666],dtype=np.int64)
    binf.write(fo, i, 'GGGGGGGG')

    #  --------------------------------------------------------------------
    # uint array
    binf.write_note(fo, "uint8[2]:[7 8]")
    i=np.array([7,8],dtype=np.uint8)
    binf.write(fo, i, 'H')

    binf.write_note(fo, "uint16[2]:[77 88]")
    i=np.array([77,88],dtype=np.uint16)
    binf.write(fo, i, 'HH')

    binf.write_note(fo, "uint32[2]:[7777 8888]")
    i=np.array([7777,8888],dtype=np.uint32)
    binf.write(fo, i, 'HHHH')

    binf.write_note(fo, "uint64[2]:[77777777 88888888]")
    i=np.array([77777777,88888888],dtype=np.uint64)
    binf.write(fo, i, 'HHHHHHHH')

    #  --------------------------------------------------------------------
    # binary bytes array - as uint8
    binf.write_note(fo, "bytes:scrzyai-utf8")
    s = bytes("ščřžýáí","utf8")  # save unicode string
    binf.write(fo, s, "I")


    #  --------------------------------------------------------------------
    # real array
    binf.write_note(fo, "float32[2]:[10.0 -11.0]")
    i=np.array([10.0,-11.0],dtype=np.float32)
    binf.write(fo, i, 'J')

    binf.write_note(fo, "float64[2]:[12.0 -13.0]")
    i=np.array([12.0,-13.0],dtype=np.float64)
    binf.write(fo, i, 'K')

    #  --------------------------------------------------------------------
    # complex scalar
    binf.write_note(fo, "complex64:14-15j")
    i=np.complex64(14-15j)
    binf.write(fo, i, 'L')

    binf.write_note(fo, "complex128:16-17j")
    i=np.complex128(16-17j)
    binf.write(fo, i, "M")

    #  --------------------------------------------------------------------
    # complex array
    binf.write_note(fo, "complex64[2]:18-+19j")
    i=np.zeros(2,dtype=np.complex64)
    i[0]= 18-19j
    i[1]= 18+19j
    binf.write(fo, i, "N")

    binf.write_note(fo, "complex128[2]:20-+21j")
    i=np.zeros(2,dtype=np.complex128)
    i[0]= 20-21j
    i[1]= 20+21j
    binf.write(fo, i, "O")

    #  --------------------------------------------------------------------
    # with shape
    binf.write_note(fo, "int16[3][2]:[0,1,2,3,4,5]-3")  # fortran memory layout
    # i=np.zeros(6,dtype=np.int8) + -22
    i=np.arange(6,dtype=np.int16) + -3
    i=i.reshape(2,3) # y=2 x=3   in python we must reverse and use zyx for fortran memory layout
    binf.write(fo, i, "P")

def read_binf(fi):
    for value,val_type,val_name,val_shape,obj_size,ary_length,file_position in binf.reader(fi):
        print(f"name:{val_name:>4s} {val_type}{obj_size}[{ary_length}]: shape:{val_shape} pos:[{file_position}]  {value}")


def main():
    fn=Path("x010_data.binf")
    with fn.open("wb") as fo:   # always use binary !!!
        write_binf(fo)
    with fn.open("rb") as fi:
        read_binf(fi)

    # with lzma compression
    fn=Path("x010_data.binf.xz")
    with lzma.open(fn, "wb") as fo:
        write_binf(fo)
    with lzma.open(fn,"rb") as fi:
        read_binf(fi)


if __name__ == "__main__":
    main()