# GEMCEAS

// TODO: WIP

## MacOS Installation Instructions
- Download and install a stable version of SWI Prolog from their homepage [here](https://www.swi-prolog.org/download/stable). 
- Move to the directory that contains `libjpl.dylib` (should be under 
_/Applications/SWI-Prolog.app/Contents/swipl/lib/fat-darwin/_) or on intel machines it is going to be at x86_64-darwin.
- Use the linux `find` command to find the location of _libjsig.dylib_ and _libjvm.dylib_ in your JDK
```shell
find /Users/<username>/Library/Java/JavaVirtualMachines/ -name libjsig.dylib
find /Users/<username>/Library/Java/JavaVirtualMachines/ -name libjvm.dylib
```
- Edit and run the following commands to change the run path search path
```shell
install_name_tool -change @rpath/libjsig.dylib <location of libjsig.dylib> libjpl.dylib
install_name_tool -change @rpath/libjvm.dylib <location of libjvm.dylib> libjpl.dylib
install_name_tool -change @rpath/libswipl.dylib /Applications/SWI-Prolog.app/Contents/Frameworks/libswipl.dylib libjpl.dylib
install_name_tool -change @rpath/libswipl.9.dylib /Applications/SWI-Prolog.app/Contents/Frameworks/libswipl.dylib libjpl.dylib
```
- Add the following VM option by editing the run configuration in IntelliJ
`-Djava.library.path=/Applications/SWI-Prolog.app/Contents/swipl/lib/fat-darwin/`  or on intel machines it is going to be at x86_64-darwin as in `-Djava.library.path=/Applications/SWI-Prolog.app/Contents/swipl/lib/x86_64-darwin/`
- Set the SWI_HOME_DIR env var in the same run config `SWI_HOME_DIR=/Applications/SWI-Prolog.app/Contents/swipl`



## Notes

Section for random notes. Will be cleaned up later

// -Djava.library.path=/Applications/SWI-Prolog.app/Contents/swipl/lib/fat-darwin/
// SWI_HOME_DIR=/Applications/SWI-Prolog.app/Contents/swipl

// install_name_tool -change @rpath/libjsig.dylib /Users/melburne/Library/Java/JavaVirtualMachines/corretto-17.0.6/Contents/Home/lib/server/libjsig.dylib libjpl.dylib
// install_name_tool -change @rpath/libjvm.dylib /Users/melburne/Library/Java/JavaVirtualMachines/corretto-17.0.6/Contents/Home/lib/server/libjvm.dylib libjpl.dylib
// install_name_tool -change @executable_path/Applications/SWI-Prolog.app/Contents/Frameworks/libswipl.dylib /Applications/SWI-Prolog.app//Contents/Frameworks/libswipl.dylib libjpl.dylib

// install_name_tool -change @rpath/libswipl.9.dylib /Applications/SWI-Prolog.app//Contents/Frameworks/libswipl.dylib libjpl.dylib

// install_name_tool -change @rpath/libswipl.dylib /Applications/SWI-Prolog.app//Contents/Frameworks/libswipl.dylib libjpl.dylib

// run prolog from /Applications/SWI-Prolog.app/Contents/MacOS/swipl
