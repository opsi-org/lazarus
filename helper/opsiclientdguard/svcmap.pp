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
  Classes,
  SysUtils,
  svcservice,
  daemonapp;



type

  { TServiceMapper }

  TServiceMapper = class(TDaemonMapper)
    procedure ServiceMapperDaemonDefs0CreateInstance(Sender: TObject);
    Constructor Create(AOwner : TComponent); override;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ServiceMapper: TServiceMapper;

implementation

(*
starttype
https://msdn.microsoft.com/en-us/library/system.serviceprocess.servicestartmode(v=vs.110).aspx

Automatic:
Indicates that the service is to be started (or was started) by the operating system, at system start-up. If an automatically started service depends on a manually started service, the manually started service is also started automatically at system startup.

Boot:
Indicates that the service is a device driver started by the system loader. This value is valid only for device drivers.

Disabled:
Indicates that the service is disabled, so that it cannot be started by a user or application.

Manual:
Indicates that the service is started only manually, by a user (using the Service Control Manager) or by an application.

System
Indicates that the service is a device driver started by the IOInitSystem function. This value is valid only for device drivers.
*)

constructor TServiceMapper.Create(AOwner: TComponent);
Var
  D : TDaemonDef;
begin
  inherited Create(AOwner);
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='opsiclientdguard';
  D.Name:='opsiclientdguard';
  D.DaemonClassName:='TServiceDaemon';
  D.WinBindings.ServiceType:=stWin32;
  D.WinBindings.StartType:=stAuto;
  D.Options:= [doAllowStop];
end;


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

