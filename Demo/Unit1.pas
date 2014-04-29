unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, ARSortGrid;

type
  TForm1 = class(TForm)
    SortGrid1: TARSortGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    chkfilter: TCheckBox;
    ChkCase: TCheckBox;
    FindButton: TButton;
    FindNext: TButton;
    findedit: TEdit;
    Label1: TLabel;
    ComColumn: TComboBox;
    Label2: TLabel;
    btnPrint: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SortGrid1GetCellFormat(Sender: TObject; Col, Row: Integer;
      State: TGridDrawState; var FormatOptions: TFormatOptions);
    procedure Button3Click(Sender: TObject);
    procedure chkfilterClick(Sender: TObject);
    procedure ChkCaseClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FindNextClick(Sender: TObject);
    procedure ComColumnChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure findeditChange(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor create( AOwner: TComponent ); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


constructor TForm1.create( AOwner: TComponent );
begin
  inherited create( AOwner );
  SortGrid1.SortSymbol := sgBendsenArrow;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SortGrid1.SaveToFile('file.dat', ',' {True});
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SortGrid1.LoadFromFile( 'file.dat', ',' );
end;

procedure TForm1.SortGrid1GetCellFormat(Sender: TObject; Col, Row: Integer;
  State: TGridDrawState; var FormatOptions: TFormatOptions);
begin
  (*
  if Row = 0 then
    FormatOptions.Font.Color := clBlue;
  if Row = 2 then
    FormatOptions.Font.Style := [fsbold];
  *)
  if( Row = 0 ) then
    FormatOptions.Font.Style := [fsbold];

  (*
  if Col = 0 then
    FormatOptions.AlignmentHorz := taLeftJustify;
  if Col = 1 then
    FormatOptions.AlignmentHorz := taCenter;
  if Col = 2 then
    FormatOptions.AlignmentHorz := taRightJustify;
  *)
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i, irow, icol: integer;
begin
  for i := 0 to 25 do
    begin
      irow := sortgrid1.AppendRow();
      for icol := 0 to sortgrid1.ColCount do
        sortgrid1.cells[icol, irow] := inttostr(random(9999));
    end
  ;
end;

procedure TForm1.chkfilterClick(Sender: TObject);
begin
  SortGrid1.SearchOptions.Filtered := ChkFilter.Checked;
  FindButton.Enabled := not ChkFilter.Checked;
  if ChkFilter.Checked = True then
    FindNext.Enabled := False;
end;

procedure TForm1.ChkCaseClick(Sender: TObject);
begin
  SortGrid1.CaseSensitive := ChkCase.Checked;
  if ChkFilter.Checked = True then
  begin
    SortGrid1.SearchOptions.Filtered := False;
    SortGrid1.SearchOptions.Filtered := True;
  end;
  FindNext.Enabled := False;
end;

procedure TForm1.FindButtonClick(Sender: TObject);
begin
  if FindEdit.Text = '' then
    FindEdit.SetFocus
  else
    SortGrid1.SearchOptions.SearchText := findEdit.text;

    if Form1.SortGrid1.FindFirst = True then
      FindNext.Enabled := True
    else
      MessageDlg('Search string '''+FindEdit.Text+''' not found!', mtInformation, [mbOK], 0)
    ;
end;

procedure TForm1.FindNextClick(Sender: TObject);
begin
  if SortGrid1.FindNext = False then
    MessageDlg('Search string '''+FindEdit.Text+''' not found!', mtInformation, [mbOK], 0);
end;

procedure TForm1.ComColumnChange(Sender: TObject);
begin
  SortGrid1.SearchOptions.SearchCol := ComColumn.ItemIndex-1;
  if ChkFilter.Checked = True then
  begin
    Form1.SortGrid1.SearchOptions.Filtered := False;
    Form1.SortGrid1.SearchOptions.Filtered := True;
  end;
  FindNext.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComColumn.ItemIndex := 0;
end;

procedure TForm1.findeditChange(Sender: TObject);
begin
  if FindEdit.Text = '' then
  begin
    ChkFilter.Checked := False;
    ChkFilter.Enabled := False;
  end
  else
    ChkFilter.Enabled := True;

  Form1.SortGrid1.SearchOptions.SearchText := FindEdit.Text;
  if ChkFilter.Checked = True then
  begin
    ChkFilter.Checked := False;
    ChkFilter.Checked := True;
  //  Form1.SortGrid1.SearchOptions.Filtered := False;
  //  Form1.SortGrid1.SearchOptions.Filtered := True;
  end;

  FindNext.Enabled := False;
end;

procedure TForm1.btnPrintClick(Sender: TObject);
begin
  sortGrid1.PrintPreview();
end;

end.
