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
