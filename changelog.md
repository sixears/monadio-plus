2.5.1.26 2022-11-18
===================
- has-callstack->1.0.1.13

2.5.1.25 2022-11-18
===================
- base1->0.0.9.21

2.5.1.24 2022-11-18
===================
- flake-build-utils->1.0.0.12

2.5.1.23 2022-11-18
===================
- fpath->1.3.2.21

2.5.1.22 2022-11-18
===================
- tasty-plus->1.5.2.16

2.5.1.21 2022-11-18
===================
- exited->1.0.4.16

2.5.1.20 2022-11-18
===================
- monaderror-io->1.2.5.13

2.5.1.19 2022-11-17
===================
- tfmt->0.2.7.15

2.5.1.18 2022-11-17
===================
- has-callstack->1.0.1.12

2.5.1.17 2022-11-17
===================
- number->r1.1.2.10

2.5.1.16 2022-11-17
===================
- base0t->r0.0.1.9

2.5.1.15 2022-11-17
===================
- base0->r0.0.4.7

2.5.1.14 2022-11-17
===================
- upgrade to callPackage-based versions

2.5.1.13 2022-11-13
===================
- fix fixed-package-name typo in flake-build-utils

2.5.1.12 2022-11-12
===================
- make flake build work

2.5.1.11 2022-11-04
===================
- fix package names

2.5.1.10 2022-11-03
===================
- remove redundant "output" flake-utils

2.5.1.9 2022-11-03
==================
- remove redundant "output" flake-utils

2.5.1.8 2022-11-03
==================
- flake-build-utils->1.0.0.6

2.5.1.7 2022-11-03
==================
- base0t->0.0.1.3

2.5.1.6 2022-11-03
==================
- base0->0.0.4.2; base0t->0.0.1.2

2.5.1.5 2022-11-03
==================
- base0->0.0.4.1; base0t->0.0.1.1

2.5.1.4 2022-11-02
==================
- natural->0.0.1.2

2.5.1.3 2022-11-02
==================
- more-unicode -> 0.0.17.2

2.5.1.2 2022-11-02
==================
- upgrade flake-build-utils to 1.0.0.3

2.5.1.1 2022-08-14
==================
- write Strings as latin1

2.5.1.0 2022-08-14
==================
- add String instance to OutputData for tempfiles tests

2.5.0.0 2022-07-04
==================
- big changes to resolvelink, also readlink, canonicalize, normalize

2.4.3.0 2022-05-23
==================
- add lstat', lstats, stat', stats, pathTypes, lsdir

2.4.2.0 2022-05-16
==================
- add ByteString to ToMaybeTexts; generate (x,y) instances of ToMaybeTexts

2.4.1.0 2022-05-13
==================
- resolvelink' now takes a list of prior seen values, to detect symlink cycles

2.4.0.1 2022-05-11
==================
- fix resolvelink' to call itself, not resolvelink (without the "'") recursively

2.4.0.0 2022-05-09
==================
- readlink & resolvelink now take an AbsFile as input.
- add resolvelink'
- resolvelink now properly returns dir/file as appropriate

2.3.2.0 2022-04-10
==================
- factor out throwExit from MonadIO.Process.system

2.3.1.0 2022-04-09
==================
- add MonadIO.Process.getPid{,'}

2.3.0.1 2022-04-07
==================
- Upgrade dependencies

2.3.0.0 2022-04-06
==================
- Remove io argument from Process.system(x); factor out procWait

2.2.0.0 2022-04-05
==================
- Process.system(x) now take an io argument for, e.g., logging actions

2.1.0.0 2022-04-03
==================
- process functions (system*, doProc) now return ExitInfo (including pid)

2.0.10.0 2022-03-06
===================
- add explicit quantification ordering to MonadIO.FStat

2.0.9.0 2022-02-28
==================
- add MonadIO.Temp.testsWithTempfiles'
- MonadIO.Temp.testsWithTempfiles now takes a test name

2.0.8.0 2022-02-08
==================
- add MonadIO.Temp.testsWithTempfiles
- use Base1T

2.0.7.0 2021-12-29
==================
- add MonadIO.Temp.testsWithTempfile

2.0.6.0 2021-12-27
==================
- add rename

2.0.5.1 2021-11-30
==================
- add explicit type variables in MonadIO.User

2.0.5.0 2021-11-30
==================
- add MonadIO.User.homePath

2.0.4.0 2021-11-30
==================
- add MonadIO.User

2.0.3.2 2021-11-29
==================
- fix for pResolveDir @AbsFile in non-extant dir

2.0.3.1 2021-11-27
==================
- add forall clauses to getCwd{,'}

2.0.3.0 2021-11-26
==================

- add FStat.isDir & FStat.lisDir
- move getCwd to Cwd (from FPath)
- add Cwd.getCwdY, Cwd.getCwd'{,Y}
- move devnull to OpenFile from File
- add HasCallStack to error-throwing functions
- add cwd, cwd'

2.0.2.1 2021-10-17
==================
- fix requirement on monaderror-io to >= rather than ==

2.0.2.0 2021-10-17
==================
- factor out throwSig{,'}

2.0.1.0 2021-10-16
==================
- doProc takes a simple input
- add `systemN`, `systemS`
- add `stdin`, `stdout`, `stderr` to NamedHandle

2.0.0.0 2021-10-14
==================
- Introduce NamedHandle & Flock

1.4.14.1 2021-09-04
===================
- fixup T.Process for updated MonadError free type ordering

1.4.14.0 2021-08-27
===================
- add Base.getArgs

1.4.13.1 2021-08-11
===================
- Use fpath-1.2.0.0

1.4.13.0 2021-07-22
===================

- Add NFData instances of CmdExe,CreateProcError,ProcExitError
- Add Printable instances of CmdArgs,CmdExe
- Add HasCmd* instances of ProcExitError
- Add tests for Process.system

1.4.12.0 2021-06-11
===================
- Add NFData instances to Signal,ExitStatus

1.4.11.0 2021-06-26
===================
- Add Eq instances to ProcExitError{,X}

1.4.10.0 2021-06-26
===================
- Add ProcExitError

1.4.9.0 2021-06-07
==================
- Add Printable instance of CreateProcError & ProcError

1.4.8.1 2021-06-06
==================
- Make FileAs instance of MkInputStream OVERLAPPABLE

1.4.8.0 2021-06-06
==================
- rename MkStream to MkInputStream (deprecate MkStream); add FileAs instance to MkInputStream

1.4.7.0 2021-06-03
==================
- export MonadIO.OpenFile.fileOpenMode

1.4.6.0 2021-06-01
==================
- add MonadIO.Process.system (and plenty of supporting bits)

1.4.5.0 2021-05-25
==================
- add `chdir`

1.4.4.0 2021-05-22
==================
- rework MonadIO.Temp, now providing withTempfile* & tempfile*

1.4.3.0 2021-05-16
==================
- add with{UTF8,Binary}TempFH

1.4.2.1 2021-05-09
==================
- litter the landscape with HasCallStack wherever there is MonadError

1.4.2.0 2021-04-26
==================
- split File into FPath,FStat,OpenFile,Temp
- add Tasty
- add inDir,mkdir,mkpath,nuke (in Directory)
- add PResolvable( pResolveDir, pResolve),getCwd (in FPath) (moved from fpath)
- add extantP{,'}
- export fileWritable, isWritableFile, isWritableDir, fileFoldLinesUTF8,
  fileFoldLinesH
- add readlink, resolvelink

1.4.1.0 2021-03-12
==================
- add lfexists{,'}

1.4.0.0 2021-03-10
==================
- {l,}stat return FStat objects

1.3.7.0 2021-03-07
==================
- export fexists, fexists', FExists(..)

1.3.6.1 2021-03-03
==================
- use fpath 1.1, using DirAs( _Dir_) rather than DirLike

1.3.6.0 2021-03-01
==================
- chmod now accepts any AsFilePath, not just FileAs

1.3.5.0 2021-02-28
==================
- add lstat

1.3.4.0 2021-02-28
==================
- export AccessMode(..)

1.3.3.0 2021-02-28
==================
- add Eq,Show to AccessMode

1.3.2.0 2021-02-27
==================
- add with{Read,Write,Append}*{Binary,UTF8}ME

1.3.1.0 2021-02-24
==================
- export chmod, unlink

1.3.0.0 2021-02-24
==================
- {read,write,append}*Binary now works with ByteString not Text

1.2.0.0 2021-02-22
==================
- flip order of perms (FileMode) & OpenFileFlags in {open,with}File*

1.1.1.0 2021-02-21
==================
- export {read,write,append}*Flags

1.1.0.0 2021-02-19
==================
- rework openFile*, withFile*

1.0.7.0 2021-02-14
==================
- add MonadIO.File.devnull

1.0.6.0 2021-02-11
==================
- add openFile{,A,R,RW,W}, withFile{A,R,RW,W}, fileFoldLines, fileFoldLinesH
- use FPath.File.FileAs rather FPath.FileLike.IsFile

1.0.5.0 2020-09-16
==================
- add fexists, withFile{,T}, access, writable, isWritable{File,Dir},
  fileWritable
- stat takes an AsFilePath

1.0.4.0 2020-01-31
==================
- add hGetContentsUTF8Lenient, getContentsUTF8Lenient, readFUTF8Lenient,
  readHandleBinary

1.0.3.0 2020-01-19
==================
- add readFUTF8

1.0.2.0 2020-01-19
==================
- add getContentsUTF8, hGetContentsUTF8

1.0.1.0 2020-01-01
==================
- integrate say, warn from minfo

1.0.0.0 2019-10-08
==================
- factored out from fluffy
