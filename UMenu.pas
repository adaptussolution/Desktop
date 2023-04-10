unit UMenu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, ComCtrls;

type
  TfrmMenu = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    Register: TMenuItem;
    Report: TMenuItem;
    Person: TMenuItem;
    Operational: TMenuItem;
    Usurio1: TMenuItem;
    company: TMenuItem;
    Employee: TMenuItem;
    Exit: TMenuItem;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExitClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PersonClick(Sender: TObject);
    procedure Usurio1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMenu: TfrmMenu;

implementation

uses UDM_PRINCIPAL, URegisterPerson, URegisterUser;

{$R *.dfm}

procedure TfrmMenu.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Terminate;
end;

procedure TfrmMenu.ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMenu.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels.Items[1].Text := DateTimeToStr(Now);
end;

procedure TfrmMenu.PersonClick(Sender: TObject);
begin
  if not Assigned(frmRegisterPerson) then
    frmRegisterPerson := TfrmRegisterPerson.Create(Self);

  frmRegisterPerson.Show;
end;

procedure TfrmMenu.Usurio1Click(Sender: TObject);
begin
  if not Assigned(frmRegisterUser) then
    frmRegisterUser := TfrmRegisterUser.Create(Self);

  frmRegisterUser.Show;
end;

end.
