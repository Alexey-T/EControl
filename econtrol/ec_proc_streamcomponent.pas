(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit ec_proc_StreamComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SaveComponentToFile(Component: TComponent; const FileName: string);
procedure SaveComponentToStream(Component: TComponent; Stream: TStream);
procedure LoadComponentFromFile(Component: TComponent; const FileName: string; OnError: TReaderError);
procedure LoadComponentFromStream(Component: TComponent; Stream: TStream; OnError: TReaderError = nil);


implementation


procedure SaveComponentToStream(Component: TComponent; Stream: TStream);
var
  MemSt: TStream;
  Writer: TWriter;
begin
  MemSt := TMemoryStream.Create;
  try
    if Component.Owner = nil then
      MemSt.WriteComponent(Component)
    else
      begin
        Writer := TWriter.Create(MemSt, 4096);
        try
          Writer.Root := Component.Owner;
          Writer.WriteSignature; //Delphi/ FPC 3.0 needs
          Writer.WriteComponent(Component);
        finally
          FreeAndNil(Writer);
        end;
      end;
    MemSt.Position := 0;
    ObjectBinaryToText(MemSt, Stream);
  finally
    FreeAndNil(MemSt);
  end
end;

procedure SaveComponentToFile(Component: TComponent; const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveComponentToStream(Component, Stream);
  finally
    FreeAndNil(Stream);
  end
end;

procedure LoadComponentFromStream(Component: TComponent; Stream: TStream; OnError: TReaderError = nil);
var
  MemSt: TStream;
  Reader: TReader;
  Sign: array[0..3] of char = '    ';
begin
   MemSt := TMemoryStream.Create;
   try
     ObjectTextToBinary(Stream, MemSt);
     MemSt.Position := 0;
     Reader := TReader.Create(MemSt, 4096);
     Reader.OnError := OnError;
    try
      if Component.Owner = nil then
        Reader.ReadRootComponent(Component)
      else
        begin
            Reader.Root := Component.Owner;

            //Reader.ReadSignature; //AT
            Reader.Read(Sign, SizeOf(Sign));

            Reader.BeginReferences;
            try
              Reader.ReadComponent(Component);
              Reader.FixupReferences;
            finally
              Reader.EndReferences;
            end;
        end;
    finally
      FreeAndNil(Reader);
    end;
  finally
    FreeAndNil(MemSt);
  end;
end;

procedure LoadComponentFromFile(Component: TComponent; const FileName: string; OnError: TReaderError);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadComponentFromStream(Component, Stream, OnError);
  finally
    FreeAndNil(Stream);
  end;
end;

end.

