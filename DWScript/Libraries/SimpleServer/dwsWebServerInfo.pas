{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsWebServerInfo;

{$I dws.inc}

interface

uses
   System.Types,
   dwsWebEnvironmentTypes, dwsHTTPSysServerEvents;

type

   IWebServerInfo = interface
      function Name : String;
      function HttpPort : Integer;
      function HttpsPort : Integer;
      function Authentications : TWebRequestAuthentications;

      function LiveQueries : String;

      function CompilationInfoJSON(const sourceName : String) : String;
      function ExecutionInfoJSON(const sourceName : String) : String;
      function CompiledPrograms : TStringDynArray;
      procedure FlushCompiledPrograms;

      function ServerEvents : IdwsHTTPServerEvents;

      function  GetURLRewriteRules : String;
      procedure SetURLRewriteRules(const json : String);
   end;

implementation

end.
