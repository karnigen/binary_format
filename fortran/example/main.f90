
subroutine check_writer(bio)
    use iso_c_binding
    use iso_fortran_env  ! defines int8,16,32,64 real32,64,128
    use binff
    use binff_types
    implicit none

    type(BinF_IO),intent(in) :: bio

    ! char
    character :: val_char
    ! character(len=:),allocatable :: nval_char             ! check char as array len=1 and nval_char(:)

    ! int | uint
    integer(kind=c_int64_t) :: val_int

    integer(kind=c_int8_t ),allocatable ::  val_nint8(:)  !  8bit int
    integer(kind=c_int8_t ),allocatable ::  val_nint8r2(:,:)  !  8bit int rank2 - see fun1,fun2 how share different ranks
    
    integer(kind=c_int16_t),allocatable :: val_nint16(:)  ! 16bit int
    integer(kind=c_int32_t),allocatable :: val_nint32(:)  ! 32bit int
    integer(kind=c_int64_t),allocatable :: val_nint64(:)  ! 64bit int

    ! real
    real(kind=c_double) :: val_double
    real(kind=c_float),  allocatable :: val_nfloat(:)        ! 32bit real
    real(kind=c_double), allocatable :: val_ndouble(:)       ! 64bit real

    ! complex
    complex(kind=c_double_complex) :: val_complex !  fortran specify size only of one component, kind8=c_double_complex=2x8B 64b+64b=128b  
    complex(kind=c_float_complex),  allocatable  :: val_nfloat_complex(:)    !  64bit = 2*32
    complex(kind=c_double_complex), allocatable  :: val_ndouble_complex(:)   ! 128bit = 2*64


    ! other
    integer :: ret                 ! return value of funcion call
    integer(1) :: i1
    integer(2) :: i2
    integer(4) :: i4
    integer(8) :: i8
    
    ! ! ################################################################

    ret = binff_write_header(bio)  ! read binf header, optional
    if( ret /= 0 ) then
        print*, "Error: cannot write binf format", ret
        stop
    end if

    ret = binff_write_param(bio, "data:main")    
    ret = binff_write_note(bio, "BINF Hello note")

    val_char='w'
    ret = binff_write_char(bio, val_char, "A")

    val_int=1
    ret = binff_write_uint(bio, val_int, 1, "B")  ! byte size for uint8 is 1
    val_int=11
    ret = binff_write_uint(bio, val_int, 2, "C")  ! byte size for uint16 is 2
    val_int=1111
    ret = binff_write_uint(bio, val_int, 4, "D")  ! byte size for uint32 is 4
    val_int=11111111
    ret = binff_write_uint(bio, val_int, 8, "E")  ! byte size for uint64 is 8

    val_int=-1
    ret = binff_write_int(bio, val_int, 1, "F")  ! byte size for uint8 is 1
    val_int=-11
    ret = binff_write_int(bio, val_int, 2, "G")  ! byte size for uint16 is 2
    val_int=-1111
    ret = binff_write_int(bio, val_int, 4, "H")  ! byte size for uint32 is 4
    val_int=-11111111
    ret = binff_write_int(bio, val_int, 8, "I")  ! byte size for uint64 is 8

    val_double=-3.1415
    ret = binff_write_real(bio, val_double, 4, "J")  ! byte size for float  is 4
    ret = binff_write_real(bio, val_double, 8, "K")  ! byte size for double is 8

    val_complex=(-6,-7)
    ret = binff_write_complex(bio, val_complex, 8, "L")  ! byte size for float complex  is 2x4=8 (fortran show size only for real part)
    val_complex=(-8,-9)
    ret = binff_write_complex(bio, val_complex, 16, "M")  ! byte size for float complex  is 2x4=8 (fortran show size only for real part)

    ! arrays
    ! - allocatable is auto allocated if right side has known size
    val_nint8r2 = reshape( [integer(1) :: (/10,11,12,13,14,15/)], [2,3])  ! auto allocated - known size
    ! print*, "sizeof:", kind(val_nint8r2)

    ! object_size - kind(x) unusable due to complex numbers, where it returns only size for real part
    !             - sizeof(x(1)) - ok, but we must know rank and bound limits
    !             - sizeof(x)/size(x)=total_size_in_bytes/array_length - seems to be usable
    ret = binff_write_bytes(bio,loc(val_nint8r2),"u",sizeof(val_nint8r2), size(val_nint8r2), "Z", shape(val_nint8r2,8))


    ! ------------------------------------------------
    ! uint
    val_nint8 = [integer(1) :: (i1,i1=10,15)]
    ret = binff_write_bytes(bio, loc(val_nint8),"u",sizeof(val_nint8), size(val_nint8), "N")

    val_nint16 = [integer(2) :: (i2,i2=20,25)]
    ret = binff_write_bytes(bio, loc(val_nint16),"u",sizeof(val_nint16), size(val_nint16), "O")

    val_nint32 = [integer(4) :: (i4,i4=30,35)]
    ret = binff_write_bytes(bio, loc(val_nint32),"u",sizeof(val_nint32), size(val_nint32), "P")

    val_nint64 = [integer(8) :: (i8,i8=40,45)]
    ret = binff_write_bytes(bio, loc(val_nint64),"u",sizeof(val_nint64), size(val_nint64), "Q")

    ! ------------------------------------------------
    ! int
    val_nint8 = [integer(1) :: (i1,i1=-30,20,10)]
    ret = binff_write_bytes(bio, loc(val_nint8),"i",sizeof(val_nint8), size(val_nint8), "R")

    val_nint16 = [integer(2) :: (i2,i2=-300,200,100)]
    ret = binff_write_bytes(bio, loc(val_nint16),"i",sizeof(val_nint16), size(val_nint16), "S")

    val_nint32 = [integer(4) :: (i4,i4=-3000,2000,1000)]
    ret = binff_write_bytes(bio, loc(val_nint32),"i",sizeof(val_nint32), size(val_nint32), "T")

    val_nint64 = [integer(8) :: (i8,i8=-30000,20000,10000)]
    ret = binff_write_bytes(bio, loc(val_nint64),"i",sizeof(val_nint64), size(val_nint64), "U")

    ! ------------------------------------------------
    ! real
    val_nfloat = [(i4,i4=-3,2)]
    ret = binff_write_bytes(bio, loc(val_nfloat),"f",sizeof(val_nfloat), size(val_nfloat), "V")
    val_ndouble = [(i4,i4=-3000,2000,1000)]
    ret = binff_write_bytes(bio, loc(val_ndouble),"f",sizeof(val_ndouble), size(val_ndouble), "W")

    ! ------------------------------------------------
    ! complex
    allocate(val_nfloat_complex(6))
    do i4=1,6
        val_nfloat_complex(i4)=cmplx(i4-4,-(i4-4))
    end do
    ret = binff_write_bytes(bio, loc(val_nfloat_complex),"c",sizeof(val_nfloat_complex), size(val_nfloat_complex), "X")

    allocate(val_ndouble_complex(6))
    do i4=1,6
        val_ndouble_complex(i4)=100.0*cmplx(i4-4,-(i4-4))
    end do
    ret = binff_write_bytes(bio, loc(val_ndouble_complex),"c",sizeof(val_ndouble_complex), size(val_ndouble_complex), "Y")

end subroutine check_writer



subroutine check_reader(bio)
    use iso_c_binding
    use binff
    use binff_types
    implicit none

    type(BinF_IO),intent(in) :: bio

    type(BinF) :: b                ! binf data
    type (BinF_VD) ::  vd          ! binf name and shape

    ! variables for binf data
    character :: val_char
    character(len=:),allocatable :: nval_char             ! check char as array len=1 and nval_char(:)
    integer(kind=c_int64_t) :: val_int

    integer(kind=c_int8_t ),allocatable ::  val_nint8(:)  !  8bit int
    integer(kind=c_int8_t ),allocatable ::  val_nint8r2(:,:)  !  8bit int rank2
    
    integer(kind=c_int16_t),allocatable :: val_nint16(:)  ! 16bit int
    integer(kind=c_int32_t),allocatable :: val_nint32(:)  ! 32bit int
    integer(kind=c_int64_t),allocatable :: val_nint64(:)  ! 64bit int

    ! real
    real(kind=c_double) :: val_double
    real(kind=c_float),  allocatable :: val_nfloat(:)        ! 32bit real
    real(kind=c_double), allocatable :: val_ndouble(:)       ! 64bit real

    ! complex
    complex(kind=c_double_complex) :: val_complex !  fortran specify size only of one component, kind8=c_double_complex=2x8B 64b+64b=128b  
    complex(kind=c_float_complex),  allocatable  :: val_nfloat_complex(:)    !  64bit = 2*32
    complex(kind=c_double_complex), allocatable  :: val_ndouble_complex(:)   ! 128bit = 2*64


    ! other
    integer(8) :: i,j                 ! cycle
    character(len=256) :: name     ! variable name from vd struct
    integer :: ret                 ! return value of funcion call
    ! ################################################################

    ret = binff_read_header(bio)  ! read binf header, optional
    if( ret /= 0 ) then
        print*, "Error: not binf format", ret
        stop
    end if

    do while(.true.)
        ret = binff_read(bio, b, vd)
        if(ret<0) then
            print*, "EOF"
            exit
        end if

        name = binff_get_name(vd)

        if(vd%shape_length>0) then
            write(*,'(" shape: ", 100(I0," "))', advance='yes') (vd%shape(i), i=1,vd%shape_length)
        end if
        write(*, '("name: ",A2," type: ",A1)', advance="no" ) trim(name), char(b%object_type)

        select case (char(b%object_type))
        case ('P','#','s')
            if(b%isarray == 0) then    ! scalar value
                ret=binff_read_char(b,val_char)
                print '(":",A," ret:",I0)', val_char, ret
            else
                if(allocated(nval_char)) deallocate(nval_char)
                allocate(character(len=b%total_size):: nval_char)   ! string is scalar in fortran
                ret = binff_read_nchar(bio,b,nval_char)
                print '("["I0"]: "A)', b%length, nval_char
            end if
       case ('u')
            if(b%isarray == 0) then    ! scalar value
                ret = binff_read_uint(b, val_int)
                print '(I0,":",I0," ret:",I0)', b%object_size*8, val_int, ret
            else
                write(*, '(I0,"["I0"]: ")', advance='no' ) b%object_size*8, b%length
                select case(b%object_size)  ! each size alone
                case (1)
                    select case(vd%shape_length)  ! like rank
                    case (0,1)
                        if(allocated(val_nint8)) deallocate(val_nint8)
                        allocate(val_nint8(b%length))
                        ret = binff_read_bytes(bio, b, loc(val_nint8))  ! in c as **ptr
                        write(*, '(100I3)') (val_nint8(j), j=1,b%length)
                    case (2)
                        if(allocated(val_nint8r2)) deallocate(val_nint8r2)
                        allocate(val_nint8r2(vd%shape(1),vd%shape(2)))
                        ret = binff_read_bytes(bio, b, loc(val_nint8r2))  ! in c as **ptr
                        print*, val_nint8r2
                    case default
                        print*, "Unknown shape"
                        stop
                    end select
                case (2)
                    if(allocated(val_nint16)) deallocate(val_nint16)
                    allocate(val_nint16(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint16))  ! in c as **ptr
                    write(*, '(100I3)') (val_nint16(j), j=1,b%length)
                case (4)
                    if(allocated(val_nint32)) deallocate(val_nint32)
                    allocate(val_nint32(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint32))
                    write(*, '(100I3)') (val_nint32(j), j=1,b%length)
                case (8)
                    if(allocated(val_nint64)) deallocate(val_nint64)
                    allocate(val_nint64(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint64))
                    write(*, '(100I3)') (val_nint64(j), j=1,b%length)
                case default
                    print*, "Uknown object size",b%object_size
                    stop
                end select
            end if
        case ('i')
            if(b%isarray == 0) then    ! scalar value
                ret = binff_read_int(b, val_int)
                print '(I0,":",I0," ret:",I0)', b%object_size*8, val_int, ret
            else
                write(*, '(I0,"["I0"]: ")', advance='no' ) b%object_size*8, b%length
                select case(b%object_size)  ! each size alone
                case (1)
                    if(allocated(val_nint8)) deallocate(val_nint8)
                    allocate(val_nint8(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint8))  ! in c as **ptr
                    write(*, '(100I4)') (val_nint8(j), j=1,b%length)
                case (2)
                    if(allocated(val_nint16)) deallocate(val_nint16)
                    allocate(val_nint16(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint16))  ! in c as **ptr
                    write(*, '(100I5)') (val_nint16(j), j=1,b%length)
                case (4)
                    if(allocated(val_nint32)) deallocate(val_nint32)
                    allocate(val_nint32(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint32))  ! in c as **ptr
                    write(*, '(100I6)') (val_nint32(j), j=1,b%length)
                case (8)
                    if(allocated(val_nint64)) deallocate(val_nint64)
                    allocate(val_nint64(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nint64))  ! in c as **ptr
                    write(*, '(100I7)') (val_nint64(j), j=1,b%length)
                case default
                    print*, "Uknown object size",b%object_size
                    stop
                end select
            end if
        case ('f')
            if(b%isarray == 0) then    ! scalar value
                ret = binff_read_double(b, val_double)
                print '(I0,":",f0.2," ret:",I0)', b%object_size*8, val_double, ret
            else
                write(*, '(I0,"["I0"]: ")', advance='no' ) b%object_size*8, b%length
                select case(b%object_size)  ! each size alone
                case (4)
                    if(allocated(val_nfloat)) deallocate(val_nfloat)
                    allocate(val_nfloat(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nfloat))
                    write(*, '(100f8.2)') (val_nfloat(j), j=1,b%length)
                case (8)
                    if(allocated(val_ndouble)) deallocate(val_ndouble)
                    allocate(val_ndouble(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_ndouble))
                    write(*, '(100f8.2)') (val_ndouble(j), j=1,b%length)
                case default
                    print*, "Uknown object size",b%object_size
                    stop
                end select
            end if
        case ('c')
            if(b%isarray == 0) then    ! scalar value
                ret = binff_read_double_complex(b, val_complex)
                print '(I0,":",f0.2," ",f0.2"i ret:",I0)', b%object_size*8, real(val_complex), aimag(val_complex), ret
            else
                write(*, '(I0,"["I0"]: ")', advance='no' ) b%object_size*8, b%length
                select case(b%object_size)  ! each size alone
                case (8)
                    if(allocated(val_nfloat_complex)) deallocate(val_nfloat_complex)
                    allocate(val_nfloat_complex(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_nfloat_complex))
                    print*, val_nfloat_complex
                case (16)
                    if(allocated(val_ndouble_complex)) deallocate(val_ndouble_complex)
                    allocate(val_ndouble_complex(b%length))
                    ret = binff_read_bytes(bio, b, loc(val_ndouble_complex))
                    print*, val_ndouble_complex
                case default
                    print*, "Uknown object size",b%object_size
                    stop
                end select
            end if

        case default
            print*, "Unknown type: ", char(b%object_type)
            stop
        end select

        ! if(i>28) exit
    end do


end subroutine check_reader

! how to share same memory with different rank array
! target - is required if you need pointer to it, due optimalisation
subroutine fun1()
    use iso_c_binding
    integer,target :: a(6)               ! origin is linear array
    integer :: i
    integer,pointer :: b(:,:)            ! will be 2d view on a
    a = [(i,i=1,6)]
    call c_f_pointer(c_loc(a),b,[2,3])   ! different rank to same memory
    a(1)=10
    print*, a
    print*, b,shape(b)
end subroutine fun1

subroutine fun2()
    use iso_c_binding
    integer,allocatable,target :: a(:,:)     ! origin is 2d array
    integer :: i
    integer,pointer :: b(:)                  ! will be 1d,linear view on a
    allocate(a(2,3))                         ! seems that allocatable,target requires allocate() ???
    a = reshape([(i,i=1,6)],[2,3])    
    call c_f_pointer(c_loc(a),b,[size(a)])   ! different rank to same memory
    a(1,1)=10
    print*, a
    print*, b,shape(b)
end subroutine fun2

! uni - c open/read/write
subroutine test_uni_io()
    use binff
    use binff_types
    implicit none

    character(len=256) :: fn
    type(BinF_IO) :: bio

    integer(c_int) :: fi,fo           ! file handler
    integer :: ret                    ! return value of funcion call

    fn = "x010_data_f_uni.binf"
    ! -------------------------------------------------
    fo = binff_uni_open(fn,"w")  ! return bio and handler
    print *, "F open handler", fo
    if(fo < 0) then
        print*, "Error: cannot open for writing",fn
        stop
    end if

    call binff_uni_init(bio,fo)    ! fill bio with data
    call check_writer(bio)
    ret = binff_uni_close(fo)

    ! -------------------------------------------------
    fi = binff_uni_open(fn,"r")  ! return bio and handler
    print *, "F open handler", fi;
    if(fi < 0) then
        print*, "Error: cannot open for reading",fn
        stop
    end if

    call binff_uni_init(bio,fi)    ! fill bio with data
    call check_reader(bio)
    ret = binff_uni_close(fi)
end subroutine test_uni_io

! std - c fopen/fread/fwrite
subroutine test_std_io()
    use binff
    use binff_types
    implicit none

    character(len=256) :: fn
    type(BinF_IO) :: bio

    ! integer(c_int) :: fi,fo           ! file handler
    type(c_ptr) :: fi,fo              ! FILE*
    integer :: ret                    ! return value of funcion call

    fn = "x010_data_f_std.binf"
    ! -------------------------------------------------
    fo = binff_std_open(fn,"w")  ! return bio and handler
    print "('F open handler 0x',Z0)", fo

    if(.not. c_associated(fo)) then
        print*, "Error: cannot open for writing",fn
        stop
    end if

    call binff_std_init(bio,fo)    ! fill bio with data
    call check_writer(bio)
    ret = binff_std_close(fo)

    ! ! -------------------------------------------------
    fi = binff_std_open(fn,"r")  ! return bio and handler
    print "('F open handler 0x',Z0)", fi

    if(.not. c_associated(fi)) then
        print*, "Error: cannot open for reading",fn
        stop
    end if

    call binff_std_init(bio,fi)    ! fill bio with data
    call check_reader(bio)
    ret = binff_std_close(fi)
end subroutine test_std_io


! std - c fopen/fread/fwrite
subroutine test_zip_io()
    use binff
    use binff_types
    implicit none

    character(len=256) :: fn, buf
    type(BinF_IO) :: bio

    ! integer(c_int) :: fi,fo           ! file handler
    type(c_ptr) :: fi,fo              ! FILE*
    integer :: ret                    ! return value of funcion call

    fn = "x010_data_f_zip.binf"
    ! -------------------------------------------------
    buf = "xz -z - >" // trim(fn)
    fo = binff_zip_open(buf,"w")  ! return bio and handler
    print "('F open handler 0x',Z0)", fo

    if(.not. c_associated(fo)) then
        print*, "Error: cannot open for writing",fn
        stop
    end if

    call binff_zip_init(bio,fo)    ! fill bio with data
    call check_writer(bio)
    ret = binff_zip_close(fo)

    ! ! ! -------------------------------------------------
    buf = "xz -dc " // trim(fn)
    fi = binff_zip_open(buf,"r")  ! return bio and handler
    print "('F open handler 0x',Z0)", fi
    if(.not. c_associated(fi)) then
        print*, "Error: cannot open for reading",fn
        stop
    end if

    call binff_zip_init(bio,fi)    ! fill bio with data
    call check_reader(bio)
    ret = binff_zip_close(fi)
end subroutine test_zip_io


program binary_fortran
    use binff
    use binff_types
    implicit none

    integer :: ret                    ! return value of funcion call
    ! ------------------------------------------------------------------------

    ! call fun2()     ! check memory sharing of different rank

    ret = binff_int_size()
    if(ret /= 64) then
        print*, "Incorrect unit size",ret, "should 64"
        stop
    end if

    ! call test_uni_io()  ! open/read/write
    ! call test_std_io()  ! fopen/fread/fwrite
    call test_zip_io()
    
    stop
end program binary_fortran
