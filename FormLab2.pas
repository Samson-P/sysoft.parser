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
    { Список лексем }
    listLex: TLexList;
    { Процедура инициализации таблицы для отображения списка лексем }
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
{ Процедура инициализации таблицы для отображения списка лексем }
begin
  with GridLex do
  begin
    RowCount := 2;
    Cells[0,0] := '№ п/п';
    Cells[1,0] := 'Лексема';
    Cells[2,0] := 'Значение';
    Cells[0,1] := '';
    Cells[1,1] := '';
    Cells[2,1] := '';
  end;
end;

procedure TLab2Form.FormCreate(Sender: TObject);
{ В начале выполнения инициализируем список лексем и таблицу идентификаторов }
begin
  InitTreeVar;
  listLex := TLexList.Create;
end;

procedure TLab2Form.FormClose(Sender: TObject; var Action: TCloseAction);
{ В конце выполнения очищаем список лексем и таблицу идентификаторов }
begin
  listLex.Free;
  ClearTreeVar
end;

procedure TLab2Form.EditFileChange(Sender: TObject);
begin
  { Можно читать файл, только когда его имя не пустое }
  BtnLoad.Enabled := (EditFile.Text <> '');
end;

procedure TLab2Form.BtnFileClick(Sender: TObject);
begin
  if FileOpenDlg.Execute then
  { Выбор имени файла с помощью стандартного диалога }
  begin
    EditFile.Text := FileOpenDlg.FileName;
    BtnLoad.Enabled := (EditFile.Text <> '');
  end;
end;

procedure TLab2Form.BtnLoadClick(Sender: TObject);
{ Процедура чтения и анализа файла }
var i,iCnt,iErr: integer;
begin
  { Очищаем список лексем }
  listLex.Clear;
  { Очищаем таблицу отображения списка лексем }
  InitLexGrid;
  try
    { Чтение файла в список строк }
    ListIdents.Lines.LoadFromFile(EditFile.Text);
    except
      MessageDlg('Ошибка чтения файла!',mtError,[mbOk],0);
      Exit;
  end;
  { Анализ списка строк и заполнение списка лексем }
  iErr := MakeLexList(ListIdents.Lines,listLex);
  if iErr <> 0 then
  { Если анализ не успешный, выводим сообщение об ошибке }
  begin
    MessageDlg(Format('Ошибка "%s" в строке %d!',[listLex[0].LexInfoStr,iErr]),
               mtWarning,[mbOk],0);
    { Берем позицию ошибочной лексемы из фиктивной лексемы в начале списка
      и позиционируем на нее список строк }
    ListIdents.SelStart := listLex[0].PosAll;
    ListIdents.SelLength := listLex[0].PosNum;
    { Отображаем позицию ошибки в списке строк }
    ListIdents.SetFocus;
  end
  else
  { Если анализ успешный, отображаем список лексем в таблице }
  begin
    MessageDlg(Format('Считано %d лексем.',[listLex.Count]),mtInformation,[mbOk],0);
    { Цикл по всем прочитанным лексемам }
    GridLex.RowCount := listLex.Count+1;
    iCnt := listLex.Count-1;
    for i:=0 to iCnt do
    begin
      { Первая колонка - номер }
      GridLex.Cells[0,i+1] := IntToStr(i+1);
      { Вторая колонка - тип лексемы }
      GridLex.Cells[1,i+1] := LexTypeName(listLex[i].LexType);
      { Третья колонка - значение лексемы }
      GridLex.Cells[2,i+1] := listLex[i].LexInfoStr;
    end;
  end;
end;

procedure TLab2Form.BtnExitClick(Sender: TObject);
{ Завершение работы с программой }
begin
  Self.Close;
end;

end.
