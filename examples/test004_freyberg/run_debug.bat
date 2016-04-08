rem cleanup
del example.cel
del example.elv
del example.nod
del example.see

rem make example files
python ..\..\python\dis2gridrep.py freyberg.nam --zscale 10.

..\..\bin\gridrepd.exe
pause
