unit UMenu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, ComCtrls, StdCtrls;

type
  TMenuThread = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TfrmMenu = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    Register: TMenuItem;
    Report: TMenuItem;
    Person: TMenuItem;
    Operational: TMenuItem;
    User: TMenuItem;
    company: TMenuItem;
    Employee: TMenuItem;
    Exit: TMenuItem;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    REGISTERCOMPANY: TMenuItem;
    SECTOR: TMenuItem;
    OFFICE: TMenuItem;
    RegisterAccess: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExitClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PersonClick(Sender: TObject);
    procedure UserClick(Sender: TObject);
    procedure REGISTERCOMPANYClick(Sender: TObject);
    procedure SECTORClick(Sender: TObject);
    procedure EmployeeClick(Sender: TObject);
    procedure OFFICEClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RegisterAccessClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMenu: TfrmMenu;

implementation

uses
  UDM_PRINCIPAL, URegisterPerson, URegisterUser, URegisterAccess,
  URegisterCompany, URegisterSector, URegisterOffice, URegisterEmployee;

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

procedure TfrmMenu.UserClick(Sender: TObject);
begin
  if not Assigned(frmRegisterUser) then
    frmRegisterUser := TfrmRegisterUser.Create(Self);

  frmRegisterUser.Show;
end;

procedure TfrmMenu.REGISTERCOMPANYClick(Sender: TObject);
begin
  if not Assigned(frmRegisterCompany) then
    frmRegisterCompany := TfrmRegisterCompany.Create(Self);

  frmRegisterCompany.Show;
end;

procedure TfrmMenu.SECTORClick(Sender: TObject);
begin
  if not Assigned(frmRegisterSector) then
    frmRegisterSector := TfrmRegisterSector.Create(Self);

  frmRegisterSector.Show;
end;

procedure TfrmMenu.EmployeeClick(Sender: TObject);
begin
  if not Assigned(frmRegisterEmployee) then
    frmRegisterEmployee := TfrmRegisterEmployee.Create(Self);

  frmRegisterEmployee.Show;
end;

procedure TfrmMenu.OFFICEClick(Sender: TObject);
begin
  if not Assigned(frmRegisterOffice) then
    frmRegisterOffice := TfrmRegisterOffice.Create(Self);

  frmRegisterOffice.Show;
end;

procedure TfrmMenu.RegisterAccessClick(Sender: TObject);
begin
  if not Assigned(frmRegisterAccess) then
    frmRegisterAccess := TfrmRegisterAccess.Create(Self);

  frmRegisterAccess.Show;
end;

procedure TfrmMenu.FormCreate(Sender: TObject);
var
  xMenu: TMenuThread;
begin
  xMenu := TMenuThread.Create;
  xMenu.FreeOnTerminate := True;
  xMenu.Resume;
end;

{ TMenuThread }

constructor TMenuThread.Create();
begin
  inherited Create(True);
end;

destructor TMenuThread.Destroy;
begin

  inherited;
end;

procedure TMenuThread.Execute;
begin
  inherited;
  if Assigned(frmMenu) then
    frmMenu.Caption := 'Usu�rio Logado: ' + DM_PRINCIPAL.FParameters.Login;

  Terminate;
  WaitFor;
  Free;
end;

initialization
  RegisterClass(TfrmMenu);


finalization
  UnRegisterClass(TfrmMenu);

end.

