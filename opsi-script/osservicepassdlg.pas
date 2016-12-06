unit osservicepassdlg;

{$MODE Delphi}
{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}
{$IOCHECKS ON}
{$OBJECTCHECKS ON}
{$VARSTRINGCHECKS ON}
{$LONGSTRINGS ON}



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

uses
	SysUtils, Types, Classes, Forms,
  Dialogs, Controls, StdCtrls, LResources, ExtCtrls;

type

  { TDialogServicePassword }

  TDialogServicePassword = class(TForm)
		OKButton: TButton;
		CancelButton: TButton;
    EditServiceURL: TEdit;
    EditUsername: TEdit;
    EditPassword: TEdit;
    LabelPassword: TLabel;
    LabelServiceURL: TLabel;
    LabelUsername: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  DialogServicePassword: TDialogServicePassword;

implementation



procedure TDialogServicePassword.FormCreate(Sender: TObject);
begin
//
end;

procedure TDialogServicePassword.FormShow(Sender: TObject);

begin
  EditUsername.SetFocus;
 //SetFocusedControl(EditServiceUrl);
// EditUsername.text := 'a';

end;

initialization
 {$i osservicepassdlg.lrs}



end.
