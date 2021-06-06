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
