@Echo off
Echo.
Echo =============================
Echo  Compiling source
Echo =============================
Echo.
Del Bin\QuaBlocks.exe
Del Setup\QuaBlocks.exe
cd Source
C:\Programs\Borland\Delphi6\Bin\DCC32 QuaBlocks
cd ..
cd Bin
copy QuaBlocks.exe ..\Setup\ /y
Copy QuaBlocksEN.lang ..\Setup\ /y
Copy QuaBlocksFR.lang ..\Setup\ /y
Echo.
Echo =============================
Echo  Done
Echo =============================
Echo.
Pause