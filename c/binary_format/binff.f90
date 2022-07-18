#include "binf.hpp"

! Copyright 2022 Tony Karnigen

! data types as 
module binff_types
    use, intrinsic :: iso_c_binding
    implicit none

    type, bind(C) :: BinF_IO
        type(c_ptr) data
        type(c_funptr) writer
        type(c_funptr) reader
    end type BinF_IO

    type, bind(C) :: BinF
        integer(c_int8_t)  :: object_type          ! type suifc #PVD
        integer(c_int8_t)  :: isarray              ! flag for array=1 scalar=0
        integer(c_int16_t) :: object_size          ! size of one element up to 32 B 
        integer(c_int64_t) :: length               ! length of array - count of elements
        integer(c_int64_t) :: total_size           ! = length * object_size in bytes for arrays
        integer(c_int8_t)  :: val(BINF_VAL_LENGTH) ! buffer for scalar values
    end type BinF

    type, bind(C) :: BinF_VD
    integer(c_int16_t) :: name_length;                    ! length of var name, 0 means name is not used
    integer(c_int8_t ) :: name(BINF_VD_MAX_NAME_LENGTH)   ! variable name 'V'
    integer(c_int16_t) :: shape_length                    ! count of dimension 0 means shape is not used
    integer(c_int64_t) :: shape(BINF_VD_MAX_SHAPE_COUNT)  ! shape
    end type BinF_VD
end module binff_types


module binff
    use, intrinsic :: iso_c_binding
    use binff_types
    implicit none

! ##############################################################
interface 
! ##############################################################

function binff_int_size() result(v) bind(c,name="binf_int_size")
    use iso_c_binding
    use binff_types
    integer(kind=c_int) :: v
end function binff_int_size

! uni io --------------------------------------------------------
!   c - open/write/read
function c_binff_uni_open(length, cname, mode) result(fh) bind(c,name="binff_uni_open")
    use iso_c_binding
    use binff_types
    integer(kind=c_int), value :: length
    character(kind=c_char), dimension(*) :: cname
    character(kind=c_char), value :: mode

    integer(kind=c_int) :: fh
end function c_binff_uni_open

subroutine binff_uni_init(bio,fh) bind(c,name="binff_uni_init")
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(inout) :: bio
    integer(kind=c_int),intent(in) :: fh
end subroutine binff_uni_init

function binff_uni_close(fh) result(v) bind(c,name="binff_uni_close")
    use iso_c_binding
    use binff_types
    integer(c_int),value :: fh
    integer(c_int) :: v
end function binff_uni_close

! std io --------------------------------------------------------
!   c - fopen/fwrite/rfead
function c_binff_std_open(length, cname, mode) result(fp) bind(c,name="binff_std_open")
    use iso_c_binding
    use binff_types
    integer(kind=c_int), value :: length
    character(kind=c_char), dimension(*) :: cname
    character(kind=c_char), value :: mode

    type(c_ptr) :: fp
end function c_binff_std_open

subroutine binff_std_init(bio,fp) bind(c,name="binff_std_init")
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(inout) :: bio
    type(c_ptr),value :: fp
end subroutine binff_std_init

function binff_std_close(fp) result(v) bind(c,name="binff_std_close")
    use iso_c_binding
    use binff_types
    type(c_ptr),value :: fp
    integer(c_int) :: v
end function binff_std_close

! zip io --------------------------------------------------------
!   c - fopen/fwrite/rfead
function c_binff_zip_open(length, cname, mode) result(fp) bind(c,name="binff_zip_open")
    use iso_c_binding
    use binff_types
    integer(kind=c_int), value :: length
    character(kind=c_char), dimension(*) :: cname
    character(kind=c_char), value :: mode

    type(c_ptr) :: fp
end function c_binff_zip_open

subroutine binff_zip_init(bio,fp) bind(c,name="binff_std_init")  ! same as std
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(inout) :: bio
    type(c_ptr),value :: fp
end subroutine binff_zip_init

function binff_zip_close(fp) result(v) bind(c,name="binff_zip_close")
    use iso_c_binding
    use binff_types
    type(c_ptr),value :: fp
    integer(c_int) :: v
end function binff_zip_close
! -----------------------------------------------------------------

integer(kind=c_int) function binff_write_header(bio) bind(c,name="binf_write_header")
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(in) :: bio
end function binff_write_header

integer(kind=c_int) function binff_read_header(bio) bind(c,name="binf_read_header")
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(in) :: bio
end function binff_read_header

integer(kind=c_int) function binff_read(bio,b,vd) bind(c,name="binf_read")
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(in) :: bio
    type(BinF),intent(inout) :: b
    type(BinF_VD),intent(inout) :: vd
end function binff_read

! ##### write #####################################

function c_binff_write_item(bio, ptr, object_type, object_size, array_length) result(v) bind(c,name="binff_write_item")
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    type(c_ptr),intent(in) ::ptr
    character(c_char),intent(in) :: object_type
    integer(c_int16_t),intent(in) :: object_size
    integer(c_int64_t),intent(in) :: array_length
    integer(kind=c_int) :: v
end function c_binff_write_item

function c_binff_write_shape(bio, val_shape, shape_length) result(v) bind(c,name="binff_write_shape")
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    integer(kind=c_int64_t) :: val_shape(:)
    integer(kind=c_int16_t) :: shape_length
    integer(kind=c_int) :: v
end function c_binff_write_shape


! ##### read #####################################

integer(kind=c_int) function binff_read_char(b, val) bind(c,name="binff_read_char")
    use iso_c_binding
    use binff_types
    type(BinF),intent(inout) :: b
    character(c_char),intent(inout) :: val
end function binff_read_char

integer(kind=c_int) function binff_read_nchar(bio,b,buf) bind(c,name="binff_read_nchar")
    use iso_c_binding
    use binff_types
    type(BinF_IO),intent(in) :: bio
    type(BinF),intent(inout) :: b
    character(kind=c_char), dimension(*) :: buf
end function binff_read_nchar

integer(kind=c_int) function binff_read_uint(b, val) bind(c,name="binff_read_uint")
    use iso_c_binding
    use binff_types
    type(BinF),intent(inout) :: b
    integer(kind=c_int64_t),intent(inout) :: val
end function binff_read_uint

integer(kind=c_int) function binff_read_int(b, val) bind(c,name="binff_read_int")
    use iso_c_binding
    use binff_types
    type(BinF),intent(inout) :: b
    integer(kind=c_int64_t),intent(inout) :: val
end function binff_read_int

integer(kind=c_int) function binff_read_double(b, val) bind(c,name="binff_read_double")
    use iso_c_binding
    use binff_types
    type(BinF),intent(inout) :: b
    real(kind=c_double),intent(inout) :: val
end function binff_read_double

integer(kind=c_int) function binff_read_double_complex(b, val) bind(c,name="binff_read_double_complex")
    use iso_c_binding
    use binff_types
    type(BinF),intent(inout) :: b
    complex(kind=c_double_complex),intent(inout) :: val
end function binff_read_double_complex

! ##### array #####################################
! - direct call changes bounds (0:n-1) of array (gfortran) - why?
integer(kind=c_int) function c_binff_read_nuint8(bio, b, vals) bind(c,name="binff_read_nuint8")
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    type(BinF),intent(inout) :: b
    type(c_ptr),intent(inout) :: vals            ! simulate void*
end function c_binff_read_nuint8

end interface

! ##############################################################
contains
! ##############################################################


! ##############################################################

function binff_uni_open(fname, mode) result(fh)
    use iso_c_binding
    character(kind=c_char,len=*), intent(in) :: fname
    character(kind=c_char), value :: mode
    integer(kind=c_int) :: fh
    integer(kind=c_int) :: length

    length = len_trim(fname)
    fh = c_binff_uni_open(length, trim(fname), mode)
end function binff_uni_open

function binff_std_open(fname, mode) result(fp)
    use iso_c_binding
    character(kind=c_char,len=*), intent(in) :: fname
    character(kind=c_char), value :: mode
    type(c_ptr) :: fp
    integer(kind=c_int) :: length

    length = len_trim(fname)
    fp = c_binff_std_open(length, trim(fname), mode)
end function binff_std_open

function binff_zip_open(fname, mode) result(fp)
    use iso_c_binding
    character(kind=c_char,len=*), intent(in) :: fname
    character(kind=c_char), value :: mode
    type(c_ptr) :: fp
    integer(kind=c_int) :: length

    length = len_trim(fname)
    fp = c_binff_zip_open(length, trim(fname), mode)
end function binff_zip_open


! get name from vd structure
! - name is already in vd, here we only transform it to fortran string
function binff_get_name(vd) result(v)
    type(BinF_VD), intent(in) :: vd
    character(len=vd%name_length) :: v
    integer :: i
    do i=1,vd%name_length
        v(i:i) = char(vd%name(i))
    end do
end function binff_get_name


! ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
! fptr = loc(ary) - array of any size and rank
! - only function for all tpes, ranks and sizes
! - fortran doesn't allow dynamic ranks
! - usege: ret = binff_read_bytes(BinF_IO bio, BinF b, c_int64_t loc(val_n_array))
function binff_read_bytes(bio, b, fptr) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    type(BinF),intent(inout) :: b
    integer(c_int64_t),intent(in) :: fptr
    integer(kind=c_int) :: v

    type(c_ptr) :: ptr
    ptr = transfer(fptr,ptr) ! typecast loc() -> c_ptr, assuming loc(vals) return first memory element of array

    ! print*, "vin:",vals,"shape:",shape(vals),"lbound:",lbound(vals,1),"ubound:",ubound(vals,1)
    v = c_binff_read_nuint8(bio,b,ptr)
    ! print*, "vou:",vals,"shape:",shape(vals),"lbound:",lbound(vals,1),"ubound:",ubound(vals,1)
end function binff_read_bytes

! bio - io struct
! fpt - memory location of data - loc(var)
! object_type - one of s|u|i|f|c|P|V|#
! object_size - size of one element, power of 2
! array_length - zero for scalar or count of elements for array
function binff_write_item(bio, fptr, object_type, object_size, array_length) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    integer(c_int64_t),intent(in) :: fptr
    character(c_char),intent(in) :: object_type
    integer,intent(in) :: object_size
    integer,intent(in) :: array_length
    integer(kind=c_int) :: v

    integer(c_int16_t) :: c_object_size
    integer(c_int64_t) :: c_array_length
    type(c_ptr) :: ptr

    ptr = transfer(fptr,ptr) ! typecast loc() -> c_ptr, assuming loc(vals) return first memory element of array
    c_object_size = int(object_size,2)
    c_array_length = int(array_length,8)

    ! print '("binff_write_item: 0x",Z0)',ptr
    ! print*, object_type,c_object_size,c_array_length

    v = c_binff_write_item(bio,ptr,object_type,c_object_size,c_array_length)
end function binff_write_item


function binff_write_param(bio, param) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    character(kind=c_char, len=*),intent(in) :: param
    integer(kind=c_int) :: v
    v = binff_write_item(bio, loc(param), 'P', 1, len_trim(param))
end function binff_write_param

function binff_write_note(bio, param) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    character(kind=c_char, len=*),intent(in) :: param
    integer(kind=c_int) :: v
    v = binff_write_item(bio, loc(param), '#', 1, len_trim(param))
end function binff_write_note

function binff_write_varname(bio, val_name) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int) :: v
    v = binff_write_item(bio, loc(val_name), 'V', 1, len_trim(val_name))
end function binff_write_varname


function binff_write_char(bio, val, val_name) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    character(kind=c_char),intent(in) :: val
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int) :: v

    if(len_trim(val_name)>0) then
        v = binff_write_varname(bio, val_name)
        if(v/=0) return
    end if

    v = binff_write_item(bio, loc(val), 's', 1, 0)
end function binff_write_char

function binff_write_uint(bio, val, object_size, val_name) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    integer(c_int64_t), intent(in) :: val
    integer, intent(in) :: object_size
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int) :: v

    if(len_trim(val_name)>0) then
        v = binff_write_varname(bio, val_name)
        if(v/=0) return
    end if

    v = binff_write_item(bio, loc(val), 'u', object_size, 0)
end function binff_write_uint

function binff_write_int(bio, val, object_size, val_name) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    integer(c_int64_t),intent(in) :: val
    integer,intent(in) :: object_size
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int) :: v

    if(len_trim(val_name)>0) then
        v = binff_write_varname(bio, val_name)
        if(v/=0) return
    end if

    v = binff_write_item(bio, loc(val), 'i', object_size, 0)
end function binff_write_int

function binff_write_real(bio, cdouble, object_size, val_name) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    real(c_double),intent(in) :: cdouble
    integer,intent(in) :: object_size
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int) :: v

    real(c_float) :: cfloat

    if(len_trim(val_name)>0) then
        v = binff_write_varname(bio, val_name)
        if(v/=0) return
    end if

    select case (object_size)
    case (4)
        cfloat = real(cdouble,4)
        v = binff_write_item(bio, loc(cfloat), 'f', object_size, 0)
    case (8)
        v = binff_write_item(bio, loc(cdouble), 'f', object_size, 0)
    case default
        print*, "Bad real size", object_size
        v=-1 
    end select
end function binff_write_real


function binff_write_complex(bio, cdouble, object_size, val_name) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    complex(c_double_complex),intent(in) :: cdouble
    integer,intent(in) :: object_size
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int) :: v

    complex(c_float_complex) :: cfloat

    if(len_trim(val_name)>0) then
        v = binff_write_varname(bio, val_name)
        if(v/=0) return
    end if

    select case (object_size)
    case (8)
        cfloat = cmplx(cdouble,kind=c_float_complex)
        v = binff_write_item(bio, loc(cfloat), 'c', object_size, 0)
    case (16)
        v = binff_write_item(bio, loc(cdouble), 'c', object_size, 0)
    case default
        print*, "Bad complex size", object_size
        v=-1 
    end select
end function binff_write_complex

! total_object_size - sizeof(one_element)*array_length - due to fortran sizeof(x) return total size in bytes for arrays
function binff_write_bytes(bio, fptr, object_type, total_object_size, array_length, val_name, val_shape) result(v)
    use iso_c_binding
    use binff_types
    type(BinF_IO), intent(in) :: bio
    integer(c_int64_t),intent(in) :: fptr
    character(kind=c_char),intent(in) :: object_type
    integer(c_int64_t),intent(in) :: total_object_size          !  16 is enough
    integer(c_int32_t),intent(in) :: array_length
    character(kind=c_char, len=*),intent(in) :: val_name
    integer(kind=c_int64_t),optional :: val_shape(:)
    integer(kind=c_int) :: v
    integer :: object_size

    object_size = int(total_object_size / array_length)  ! size of one element
    ! print*, "otype:",object_type, " tsize:",total_object_size, " osize:", object_size, &
    !         " alen:",array_length, " name:",val_name, " shape:",present(val_shape)
    if(len_trim(val_name)>0) then
        v = binff_write_varname(bio, val_name)
        if(v/=0) return
    end if

    if(present(val_shape)) then
        if(size(val_shape)>0) then
            ! print "(Z0)",loc(val_shape) 
            v = c_binff_write_shape(bio, val_shape, size(val_shape, kind=2))
        end if
    end if

    v = binff_write_item(bio, fptr, object_type, object_size, array_length)
end function binff_write_bytes


end module binff


