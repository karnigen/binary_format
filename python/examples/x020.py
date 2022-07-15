#!/usr/bin/env python3

import binary_format as binf
import numpy as np
from pathlib import Path
import lzma



def read_binf(fi):
    for value,val_type,val_name,val_shape,obj_size,ary_length,file_position in binf.reader(fi):
        print(f"name:{val_name:>4s} {val_type}{obj_size}[{ary_length}]: shape:{val_shape} pos:[{file_position}]  {value}")


def main():
    fn=Path("x010_data_c.binf")
#    fn=Path("x010_data_f.binf")
    # with fn.open("wb") as fo:
    #     write_binf(fo)
    with fn.open("rb") as fi:
        read_binf(fi)



if __name__ == "__main__":
    main()