






module stdlib_io_disp

    use stdlib_kinds, only: sp, dp, qp, &
                            int8, int16, int32, int64, &
                            lk, c_bool

    use stdlib_string_type, only: string_type, char
    use stdlib_optval, only: optval
    use stdlib_strings, only: to_string
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    private

    public :: disp

    interface disp
        module procedure disp_0_rsp
        module procedure disp_0_rdp
        module procedure disp_0_rqp
        module procedure disp_0_csp
        module procedure disp_0_cdp
        module procedure disp_0_cqp
        module procedure disp_0_iint8
        module procedure disp_0_iint16
        module procedure disp_0_iint32
        module procedure disp_0_iint64
        module procedure disp_0_llk
        module procedure disp_0_lc_bool
        module procedure disp_0_tstring_type
        module procedure disp_1_rsp
        module procedure disp_1_rdp
        module procedure disp_1_rqp
        module procedure disp_1_csp
        module procedure disp_1_cdp
        module procedure disp_1_cqp
        module procedure disp_1_iint8
        module procedure disp_1_iint16
        module procedure disp_1_iint32
        module procedure disp_1_iint64
        module procedure disp_1_llk
        module procedure disp_1_lc_bool
        module procedure disp_1_tstring_type
        module procedure disp_2_rsp
        module procedure disp_2_rdp
        module procedure disp_2_rqp
        module procedure disp_2_csp
        module procedure disp_2_cdp
        module procedure disp_2_cqp
        module procedure disp_2_iint8
        module procedure disp_2_iint16
        module procedure disp_2_iint32
        module procedure disp_2_iint64
        module procedure disp_2_llk
        module procedure disp_2_lc_bool
        module procedure disp_2_tstring_type
    end interface disp

contains


    subroutine disp_0_rsp(x, header, unit, brief, format, width, sep)
    
        real(sp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_rsp
    subroutine disp_0_rdp(x, header, unit, brief, format, width, sep)
    
        real(dp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_rdp
    subroutine disp_0_rqp(x, header, unit, brief, format, width, sep)
    
        real(qp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_rqp
    subroutine disp_0_csp(x, header, unit, brief, format, width, sep)
    
        complex(sp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_csp
    subroutine disp_0_cdp(x, header, unit, brief, format, width, sep)
    
        complex(dp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_cdp
    subroutine disp_0_cqp(x, header, unit, brief, format, width, sep)
    
        complex(qp), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_cqp
    subroutine disp_0_iint8(x, header, unit, brief, format, width, sep)
    
        integer(int8), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_iint8
    subroutine disp_0_iint16(x, header, unit, brief, format, width, sep)
    
        integer(int16), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_iint16
    subroutine disp_0_iint32(x, header, unit, brief, format, width, sep)
    
        integer(int32), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_iint32
    subroutine disp_0_iint64(x, header, unit, brief, format, width, sep)
    
        integer(int64), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_iint64
    subroutine disp_0_llk(x, header, unit, brief, format, width, sep)
    
        logical(lk), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_llk
    subroutine disp_0_lc_bool(x, header, unit, brief, format, width, sep)
    
        logical(c_bool), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(1))
        x_str(1) = string_type(to_string(x, format_))
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_lc_bool
    subroutine disp_0_tstring_type(x, header, unit, brief, format, width, sep)
    
        type(string_type), intent(in) :: x
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        
        write(unit_, *) format_output_string([x], width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_0_tstring_type
    subroutine disp_1_rsp(x, header, unit, brief, format, width, sep)
    
        real(sp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_rsp
    subroutine disp_1_rdp(x, header, unit, brief, format, width, sep)
    
        real(dp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_rdp
    subroutine disp_1_rqp(x, header, unit, brief, format, width, sep)
    
        real(qp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_rqp
    subroutine disp_1_csp(x, header, unit, brief, format, width, sep)
    
        complex(sp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_csp
    subroutine disp_1_cdp(x, header, unit, brief, format, width, sep)
    
        complex(dp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_cdp
    subroutine disp_1_cqp(x, header, unit, brief, format, width, sep)
    
        complex(qp), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_cqp
    subroutine disp_1_iint8(x, header, unit, brief, format, width, sep)
    
        integer(int8), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_iint8
    subroutine disp_1_iint16(x, header, unit, brief, format, width, sep)
    
        integer(int16), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_iint16
    subroutine disp_1_iint32(x, header, unit, brief, format, width, sep)
    
        integer(int32), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_iint32
    subroutine disp_1_iint64(x, header, unit, brief, format, width, sep)
    
        integer(int64), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_iint64
    subroutine disp_1_llk(x, header, unit, brief, format, width, sep)
    
        logical(lk), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_llk
    subroutine disp_1_lc_bool(x, header, unit, brief, format, width, sep)
    
        logical(c_bool), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 1)))
        do i = 1, size(x, 1)
            x_str(i) = string_type(to_string(x(i), format_))
        end do
        write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_lc_bool
    subroutine disp_1_tstring_type(x, header, unit, brief, format, width, sep)
    
        type(string_type), intent(in) :: x(:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        
        write(unit_, *) format_output_string(x, width=width_, brief=brief_, sep=sep_)
        
        
        
    end subroutine disp_1_tstring_type
    subroutine disp_2_rsp(x, header, unit, brief, format, width, sep)
    
        real(sp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_rsp
    subroutine disp_2_rdp(x, header, unit, brief, format, width, sep)
    
        real(dp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_rdp
    subroutine disp_2_rqp(x, header, unit, brief, format, width, sep)
    
        real(qp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_rqp
    subroutine disp_2_csp(x, header, unit, brief, format, width, sep)
    
        complex(sp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_csp
    subroutine disp_2_cdp(x, header, unit, brief, format, width, sep)
    
        complex(dp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_cdp
    subroutine disp_2_cqp(x, header, unit, brief, format, width, sep)
    
        complex(qp), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_cqp
    subroutine disp_2_iint8(x, header, unit, brief, format, width, sep)
    
        integer(int8), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_iint8
    subroutine disp_2_iint16(x, header, unit, brief, format, width, sep)
    
        integer(int16), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_iint16
    subroutine disp_2_iint32(x, header, unit, brief, format, width, sep)
    
        integer(int32), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_iint32
    subroutine disp_2_iint64(x, header, unit, brief, format, width, sep)
    
        integer(int64), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_iint64
    subroutine disp_2_llk(x, header, unit, brief, format, width, sep)
    
        logical(lk), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_llk
    subroutine disp_2_lc_bool(x, header, unit, brief, format, width, sep)
    
        logical(c_bool), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        allocate(x_str(size(x, 2)))
        do i = 1, size(x, 1)
            do j = 1, size(x, 2)
                x_str(j) = string_type(to_string(x(i, j), format_))
            end do
            write(unit_, *) format_output_string(x_str, width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_lc_bool
    subroutine disp_2_tstring_type(x, header, unit, brief, format, width, sep)
    
        type(string_type), intent(in) :: x(:,:)
        character(len=*), intent(in), optional :: header
        integer, intent(in), optional :: unit
        logical, intent(in), optional :: brief
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: width
        character(len=*), intent(in), optional :: sep
        
        integer :: unit_, width_
        logical :: brief_
        character(len=:), allocatable :: format_, sep_
        integer :: i, j
        type(string_type), allocatable :: x_str(:)
        
        !> State default values
        unit_   = optval(unit, output_unit)
        brief_  = optval(brief, .false.)
        format_ = optval(format, "g0.4")
        width_  = optval(width, 80)
        width_  = merge(width_, 4, width_ >= 4)
        sep_    = optval(sep, "  ")
            
        if (present(header)) then
            write(unit_, *) format_output_string([string_type(header)], width=width_, brief=brief_, sep=sep_)
        end if
        
        
        do i = 1, size(x, 1)
            write(unit_, *) format_output_string(x(i, :), width=width_, brief=brief_, sep=sep_)
        end do
        
        
    end subroutine disp_2_tstring_type
    
    function format_output_string(x, width, brief, sep) result(str)
        type(string_type), intent(in) :: x(:)
        integer, intent(in) :: width
        logical, intent(in) :: brief
        character(len=*), intent(in) :: sep
        character(width), allocatable :: str(:)
        character(len=:), allocatable :: tmp(:)
        
        integer :: max_len, elem_len, num1, num2, i
        
        !> 获取最长的字符串长度
        elem_len = max_len + len(sep)
        num1 = (width-1)/elem_len
        if (num1 /= 0) then
            num2 = size(x, 1)/num1
            allocate(str(merge(num2, num2 + 1, mod(size(x, 1), num1)==0)))
            
            do i = 1, size(str) - 1
                str(i) = char(x(num1*i))//sep//char(x(num1*i+1))//sep//char(x(num1*i+2))//sep
            end do
        else
            allocate(str(1))
        end if
        str(size(str)) = char(x(num1*size(str)))//sep
        
    end function format_output_string

end module stdlib_io_disp
