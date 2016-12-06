unit packdefs;

{$MODE Delphi}

       {Structures and headers for unzip, unarj and unlzh}

// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: Rupert Roeder, detlef oertel
// credits: http://www.opsi.org/credits/

//***************************************************************************
// Subversion:
// $Revision: 288 $
// $Author: oertel $
// $Date: 2015-01-29 18:19:14 +0100 (Do, 29 Jan 2015) $
//***************************************************************************


interface

const
		 Dirlength = 259;

type
     TDirtype=array[0..DirLength] of char;
     TPackRec=packed record
       internal:array[0..11] of byte;  {Used internally by the dll}
       Time,                     {file time}
       Size,                     {file size}
       CompressSize,             {size in zipfile}
       headeroffset,             {file offset in zip: needed in unzipfile}
       CRC: Longint;             {CRC, sort of checksum}
       FileName: tdirtype;       {file name}
       PackMethod,               {pack method, see below}
       Attr,                     {file attribute}
       Flags:word;               {lo byte: arj_flags; hi byte: file_type}
     end;

const zip_ok=0;
      zip_FileError=-1;        {Error reading zip file}
      zip_InternalError=-2;    {Error in zip file format}
      zip_NoMoreItems=1;       {Everything read}

const   {Error codes, delivered by unarjfile}
  unzip_Ok=0;               {Unpacked ok}
  unzip_CRCErr=1;           {CRC error}
  unzip_WriteErr=2;         {Error writing out file: maybe disk full} 
  unzip_ReadErr=3;          {Error reading zip file}
  unzip_ZipFileErr=4;       {Error in zip structure}  
  unzip_UserAbort=5;        {Aborted by user}
  unzip_NotSupported=6;     {ZIP Method not supported!}
  unzip_Encrypted=7;        {Zipfile encrypted}
  unzip_InUse=-1;           {DLL in use by other program!}
  unzip_DLLNotFound=-2;     {DLL not loaded!}

implementation

end.

