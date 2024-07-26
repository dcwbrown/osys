# Hosted Oberon V5

Built from Niklaus Wirth's last and most simplified Oberon system and compiler, with adaptations to run on Intel/AMD 64 bit processors under Windows and Linux.

# *This is a work in progress*

## Visual changes

Modern screens have a higher resolution than the Oberon V5 fixed implementation of
1024x768. Therefore
- hosted Oberon opens a window of 1536x1152
- the dfault font is Syntax14.Scn.Fnt instead of Oberon10.Scn.Fnt.

Above the standard tracks there is a full width MenuViewer providing a Window
title - it contains System.Minimize and System.Quit commands, and can be used to
drag the Oberon window around the screen.

## Oberon compiler

- The compiler generates code for AMD/INTEL X64 processors in '.X64' files
- `INTEGER` and `SET` are 64 bits.  There is no LONGINT type.
- `SYSTEM` provides `INT8`, `INT16`, `INT32`, `CARD16` and `CARD32` types. There is no unsigned 64 bit type.
- Any type can be passed to a formal (`VAR`) `ARRAY OF BYTE` parameter.
- `REAL` is not yet implemented.
- Parameters are passed on the stack, procedure call overhead is minimal.
- Limited Windows and Linux 64 bit API support is available, e.g.:
  - `ExitProcess: PROCEDURE\ (exitcode: INTEGER);` declares a procedure variable using the Windows X64 ABI.
  - `PROCEDURE\ ExceptionHandler(p: ExceptionPointers);` Decalres a Windows 64 bit ABI procedure.
  - `PROCEDURE/ SignalHandler(signal, infoadr, contextadr: INTEGER);` Declares a Linux 64 bit ABI procedure.
  - `PROCEDURE #60 sysexit(i: INTEGER);` declares a Linux 64 bit syscall procedure.
- `POINTER-` and `RECORD-` declare non-GC types.
- `(*$la+lc+*)` enables extensive code generation tracing and listing -- this will likely be removed, or possibly tidied up. It has provided invaluable support for the development of X64 code generation.

There is no support for linking or generating object libraries, although LoadLibrary/GetProcAddress are available on Windows. No C runtime libraries are used.
On Linux all host system functionality is obtained through syscalls to the kernel, on
Windows calls are to KERNEL32, GDI32 and USER32.

## Linking, startup and Host.Mod (actually Lin.Host.Mod and Win.HostMod)

In native Oberon V5, the inner core consists of Kernel, FieDir, Files and Modules. These are prelinked by 'ORL.Link Modules' and stored in the boot area by BootLoad.Mod.

In hosted Oberon, the inner core consists of Host, Kernel, Files and Modules. The Link command combines these and they form the text section of the ELF or PE executable binary.

The link command parameter is either a Module name (e.g. '`Oberon`') or a command (e.g. '`ORP.Compile`'. Link walks the import tree of the specified module and includes the .X64
files for all referenced modules other than the inner core modules in a second 'preload'
section.

Startup proceeds similarly to native Oberon V5 in that control is passed initially to
the main body of Modules.Mod, which calls Files.Init, which calls Kernel.Init, and
Kernel.Init calls Host.Init.

When Files, Kernel and Host initialisation have completed, control returns to Modules.Mod
which calls Modules.Load on the module originally named on the Link command.

Modules.Load is extended to preload all modules in the preload section. In this manner all
the modules included by the linker will be loaded and eventually the main body of the
module named on the linker command will run.

Finally, if the parameter on the Link command was a full command (e.g. '`ORP.Compile`'),
the command will be invoked.

The Link command also accepts a list of additional modules to be included in the preload
section with syntax such as 'Link.PE Oberon+System+ORP+Link'. This allows the generated
executable to include all necessary resources and so be runnable anywhere.

## Building hosted Oberon on Windows and Linux

The git repository includes build.cmd and build.sh scripts at the top level.

The directory 'knowngood' includes the following pre-built executables:
- `ORP.Compile.exe` and `Link.PE.exe` for Windows
- `ORP.Compile` and `Link.ELF` for Linux

The build scripts:
- generate a subdirectory (build.win or build.lin)
- populate it with source files from `src` and executables from `knowngood`
- build both the compiler and the Oberon system
- start the Oberon system

The build process is agreeably fast.

## Filesystem

Oberon separates files and directories more completely than Windows or even Linux.

- files exist independent of any presence in a directory
- Oberon stores file details at the start of the file, including date, length and sectors
- the Oberon directory is only a mapping from name to first file sector

Neither Windows nor Linux has such a complete separation of concerns.

Hosted Oberon emulates native Oberon by implementing unregistered files as
temporary files named as name.unreg-nnnnnn. Resigstering the file renames it
to it's registered name. Deleting the reistered name renames it back to the
temp name.

When an unregistered file is freed by garbage collection, or by Oberon termination,
the temporary file is deleted.

On Windows the 'delete on close' disposition flag is also maintained which should
make it impossible for a crash to leave a temporary file behind. There is no similar
mechanism on Linux.

The FileDesc record contains up to 4 4K buffers. For any given registered file there
is no more than one FileDesc record, which is shared between all modules accessing
that file. This means that multiple modules in one Oberon executable can manipulate
a file and see each others changes.

This is not true for separate Oberon executables. No attempt is made for separate
host executables to communicate with each other. Nor is there monitoring of files
for changes made by non-Oberon executables.

This means for example that it is not practicable to work on a file in the Oberon
environment and edit it in an external editor at the same time.

