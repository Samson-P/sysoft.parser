program Lab2;

uses
  Forms,
  FormLab2 in 'FormLab2.pas' {Lab2Form},
  TblElem in '..\Common\TblElem.pas',
  FncTree in '..\Common\FncTree.pas',
  LexType in 'LexType.pas',
  LexElem in '..\Common\LexElem.pas',
  LexAuto in 'LexAuto.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Lab. work 2';
  Application.CreateForm(TLab2Form, Lab2Form);
  Application.Run;
end.
