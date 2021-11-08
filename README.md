# Fortran stdlib `disp` draft

`disp` subroutine is for displaying header and array nicely.

## API

```fortran
call disp( [x, header, unit, brief, format, width, sep] )
```

### Arguments

`x`: Shall be a `logical/integer/real/complex/character(len=*)/string_type` scalar or `logical/integer/real/complex` and rank-1/rank-2 array.
This argument is `intent(in)` and `optional`.

`header`: Shall be a `character(len=*)` scalar.
This argument is `intent(in)` and `optional`.

`unit`: Shall be an `integer(int32)` scalar linked to an IO stream.
This argument is `intent(in)` and `optional`.
The default value is `output_unit` from `iso_fortran_env` module.

`brief`: Shall be a `logical` scalar, controls an abridged version of the `x` array to be outputed.
This argument is `intent(in)` and `optional`.
The default value is `.false.`

`format`: Shall be a `character(len=*)` scalar.
This argument is `intent(in)` and `optional`.
The default value is `g0.4`.

`width`: Shall be an `integer(int32)` scalar, controls the outputed max width.
This argument is `intent(in)` and `optional`.
The default value is `80`.

`sep`: Shall be an `character(len=*)` scalar, separator.
This argument is `intent(in)` and `optional`.
The default value is `  `, two spaces.

### Effects

```sh


```

## Links

- [stdlib_logger]()
- [fortran-csv-module]()
- [old disp](https://github.com/fortran-lang/stdlib/pull/520/files)
- [forlab](https://github.com/fortran-fans/forlab)