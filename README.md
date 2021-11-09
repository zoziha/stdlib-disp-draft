# Fortran stdlib `disp` draft

`disp` subroutine is for displaying header and array nicely.

## Run code

```sh
# Fypp && Format && fpm run
fypp stdlib_io_disp.fypp stdlib_io_disp.f90 && fprettify -i 4 stdlib_io_disp.f90 && fpm test
```

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
x
 dada
 [vector size: 2]
1  2
 disp(string):
string
It is a note.

 disp(r):
 [matrix size: 2×6]
-0.1000E-10  -0.1000E+11  1.000        1.000        1.000        1.000
1.000        1.000        1.000        1.000        1.000        1.000
 disp(r(1,:))
 [vector size: 6]
-0.1000E-10  -0.1000E+11  1.000        ..           1.000
 disp(c):
 [matrix size: 2×3]
(1.000,0.000)              (1.000,0.000)              (1.000,0.000)
(1.000,0.000)              (-0.1000E+11,-0.1000E+11)  (1.000,0.000)
 disp(i):
 [matrix size: 2×3]
1  1  1
1  1  1
 disp(l):
 [matrix size: 10×10]
T, T, T, ..,T,
T, T, T, ..,T,
T, T, T, ..,T,
:, :, :, :, :,
T, T, T, ..,T,
 disp(c_3d(:,:,3)):
 [matrix size: 2×20]
(1000.,0.1000E-2)  (0.1000E+5,100.0)  (2.000,0.000)      ..                 (2.000,0.000)
(2.000,0.000)      (2.000,0.000)      (2.000,0.000)      ..                 (2.000,0.000)
 disp(r_2d):
 [matrix size: 50×20]
13.90,  26.66,  27.78,  ..,     85.34,
91.48,  77.18,  69.54,  ..,     71.10,
0.9104, 11.65,  46.48,  ..,     86.06,
:,      :,      :,      :,      :,
83.40,  38.29,  42.59,  ..,     61.97,
 disp(s):
 [vector size: 2]
abcdefghijk  abcdefghijk
 disp(s):
abcdefghijk
 disp(s):
 [vector size: 2]
abcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghij**  &
abcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghij**
 disp(s):
abcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghij**
 disp(s):
 [vector size: 2]
abcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghij**  &
abcdefghijk
xyz
```

## Links

- [stdlib_logger]()
- [fortran-csv-module]()
- [old disp](https://github.com/fortran-lang/stdlib/pull/520/files)
- [forlab](https://github.com/fortran-fans/forlab)