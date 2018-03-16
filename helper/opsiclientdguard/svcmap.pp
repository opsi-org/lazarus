// This code is part of the opsi.org project

// Copyright (c) uib gmbh (www.uib.de)
// This sourcecode is owned by the uib gmbh, D55118 Mainz, Germany
// and published under the Terms of the GNU Affero General Public License.
// Text of the AGPL: http://www.gnu.org/licenses/agpl-3.0-standalone.html
// author: detlef oertel
//
// derivated from:
// fpc example cleandirs.lpi (C:\lazarus\examples\cleandir)
// http://wiki.lazarus.freepascal.org/Daemons_and_Services
// https://www.freepascal.org/~michael/articles/daemons/daemons.pdf
// https://www.freepascal.org/~michael/articles/services/services.pdf
// Thanks to Michaël Van Canneyt


unit svcmap;

{$mode objfpc}{$H+}

interface

{$R *.lfm}

uses
  Classes, SysUtils, daemonapp;

type

  { TServiceMapper }

  TServiceMapper = class(TDaemonMapper)
    procedure ServiceMapperDaemonDefs0CreateInstance(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ServiceMapper: TServiceMapper;

implementation

procedure RegisterMapper; 
begin
  RegisterDaemonMapper(TServiceMapper)
end;

{ TServiceMapper }

procedure TServiceMapper.ServiceMapperDaemonDefs0CreateInstance(Sender: TObject
  );
begin
  Application.Log(etInfo, 'CreateInstance');
end;

{ TServiceMapper }


initialization
  RegisterMapper;
end.

