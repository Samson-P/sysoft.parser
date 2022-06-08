unit FormLab2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Grids, LexElem;

type
  TLab2Form = class(TForm)
    PageControl1: TPageControl;
    SheetFile: TTabSheet;
    SheetLexems: TTabSheet;
    BtnExit: TButton;
    GroupText: TGroupBox;
    ListIdents: TMemo;
    EditFile: TEdit;
    BtnFile: TButton;
    BtnLoad: TButton;
    FileOpenDlg: TOpenDialog;
    GridLex: TStringGrid;
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnFileClick(Sender: TObject);
    procedure EditFileChange(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
   private
    { ������ ������ }
    listLex: TLexList;
    { ��������� ������������� ������� ��� ����������� ������ ������ }
    procedure InitLexGrid;
   public
    { Public declarations }
  end;

var
  Lab2Form: TLab2Form;

implementation

{$R *.DFM}

uses FncTree, LexType, LexAuto;

procedure TLab2Form.InitLexGrid;
{ ��������� ������������� ������� ��� ����������� ������ ������ }
begin
  with GridLex do
  begin
    RowCount := 2;
    Cells[0,0] := '� �/�';
    Cells[1,0] := '�������';
    Cells[2,0] := '��������';
    Cells[0,1] := '';
    Cells[1,1] := '';
    Cells[2,1] := '';
  end;
end;

procedure TLab2Form.FormCreate(Sender: TObject);
{ � ������ ���������� �������������� ������ ������ � ������� ��������������� }
begin
  InitTreeVar;
  listLex := TLexList.Create;
end;

procedure TLab2Form.FormClose(Sender: TObject; var Action: TCloseAction);
{ � ����� ���������� ������� ������ ������ � ������� ��������������� }
begin
  listLex.Free;
  ClearTreeVar
end;

procedure TLab2Form.EditFileChange(Sender: TObject);
begin
  { ����� ������ ����, ������ ����� ��� ��� �� ������ }
  BtnLoad.Enabled := (EditFile.Text <> '');
end;

procedure TLab2Form.BtnFileClick(Sender: TObject);
begin
  if FileOpenDlg.Execute then
  { ����� ����� ����� � ������� ������������ ������� }
  begin
    EditFile.Text := FileOpenDlg.FileName;
    BtnLoad.Enabled := (EditFile.Text <> '');
  end;
end;

procedure TLab2Form.BtnLoadClick(Sender: TObject);
{ ��������� ������ � ������� ����� }
var i,iCnt,iErr: integer;
begin
  { ������� ������ ������ }
  listLex.Clear;
  { ������� ������� ����������� ������ ������ }
  InitLexGrid;
  try
    { ������ ����� � ������ ����� }
    ListIdents.Lines.LoadFromFile(EditFile.Text);
    except
      MessageDlg('������ ������ �����!',mtError,[mbOk],0);
      Exit;
  end;
  { ������ ������ ����� � ���������� ������ ������ }
  iErr := MakeLexList(ListIdents.Lines,listLex);
  if iErr <> 0 then
  { ���� ������ �� ��������, ������� ��������� �� ������ }
  begin
    MessageDlg(Format('������ "%s" � ������ %d!',[listLex[0].LexInfoStr,iErr]),
               mtWarning,[mbOk],0);
    { ����� ������� ��������� ������� �� ��������� ������� � ������ ������
      � ������������� �� ��� ������ ����� }
    ListIdents.SelStart := listLex[0].PosAll;
    ListIdents.SelLength := listLex[0].PosNum;
    { ���������� ������� ������ � ������ ����� }
    ListIdents.SetFocus;
  end
  else
  { ���� ������ ��������, ���������� ������ ������ � ������� }
  begin
    MessageDlg(Format('������� %d ������.',[listLex.Count]),mtInformation,[mbOk],0);
    { ���� �� ���� ����������� �������� }
    GridLex.RowCount := listLex.Count+1;
    iCnt := listLex.Count-1;
    for i:=0 to iCnt do
    begin
      { ������ ������� - ����� }
      GridLex.Cells[0,i+1] := IntToStr(i+1);
      { ������ ������� - ��� ������� }
      GridLex.Cells[1,i+1] := LexTypeName(listLex[i].LexType);
      { ������ ������� - �������� ������� }
      GridLex.Cells[2,i+1] := listLex[i].LexInfoStr;
    end;
  end;
end;

procedure TLab2Form.BtnExitClick(Sender: TObject);
{ ���������� ������ � ���������� }
begin
  Self.Close;
end;

end.
