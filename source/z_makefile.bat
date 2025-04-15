::Make OBJ files in meson build without running NEMS
::Command Prompt must be pathed to source folder

call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022
CALL "C:\python_environments\aeo2025_py311_D\Scripts\activate"
meson setup builddir
meson.exe compile -C builddir

pause