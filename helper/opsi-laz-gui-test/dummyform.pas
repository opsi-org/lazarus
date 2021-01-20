unit dummyform;

{ The goal of this form is to have a sapmle of needed components
to detect problems with the opsi-laz-gui-test }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ColorBox, ValEdit, ComCtrls, EditBtn, ShellCtrls, FileCtrl,
  DateTimePicker;

type

  { TFormDummy }

  TFormdummy = class(TForm)
    BitBtn1: TBitBtn;
    ColorBox1: TColorBox;
    DateTimePicker1: TDateTimePicker;
    Edit1: TEdit;
    EditButton1: TEditButton;
    FilterComboBox1: TFilterComboBox;
    FlowPanel1: TFlowPanel;
    Label1: TLabel;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    RadioButton1: TRadioButton;
    ShellListView1: TShellListView;
    TimeEdit1: TTimeEdit;
    TrackBar1: TTrackBar;
    ValueListEditor1: TValueListEditor;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Formdummy: TFormdummy;

implementation

{$R *.lfm}

{ TFormdummy }

procedure TFormdummy.FormCreate(Sender: TObject);
begin

end;

end.

