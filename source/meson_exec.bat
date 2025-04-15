pushd %~dp0

call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022
SET NEMSPYENV=C:\python_environments\aeo2025_py311_D
call %NEMSPYENV%\Scripts\activate

echo "build pyf"
python PyFilerf2py_build.py
python -m numpy.f2py pyfiler1.pyf
python -m numpy.f2py pyfiler2.pyf

echo "now parse the wrapper and module.c file"
python meson_wrapper_parser.py
python meson_module_c_parser.py

sleep 5

echo "run meson build to generate .pyd"
rmdir builddir /S /Q

meson setup builddir
meson.exe compile -C builddir

echo "Complete pyd build"
echo "done"
pause
