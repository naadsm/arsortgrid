Unit ARSortGridPreview;

INTERFACE

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, Printers,
  ExtDlgs, ARSortGrid;

type
  TARSortGridPreviewForm = class( TForm )
    ScrollBox1:          TScrollBox;
    PreviewImage:        TImage;
    Panel1:              TPanel;
    Header:              TEdit;
    Headers:             TListBox;
    Margins:             TListBox;
    ckborders:           TCheckBox;
    btnprint:            TSpeedButton;
    btnshow:             TSpeedButton;
    btnsetup:            TSpeedButton;
    btnfull:             TSpeedButton;
    btnpic:              TSpeedButton;
    lblpages:            TLabel;
    cklive:              TCheckBox;
    PrinterSetupDialog1: TPrinterSetupDialog;
    OpenPictureDialog1:  TOpenPictureDialog;
    PreviewPage: TEdit;
    Margin: TEdit;

    procedure btnshowClick( Sender: TObject );
    procedure MarginsClick( Sender: TObject );
    procedure btnprintClick( Sender: TObject );
    procedure MarginChange( Sender: TObject );
    procedure HeaderChange( Sender: TObject );
    procedure FormShow( Sender: TObject );
    procedure ckbordersClick( Sender: TObject );
    procedure previewpageChange( Sender: TObject );
    procedure HeadersClick( Sender: TObject );
    procedure ckliveClick( Sender: TObject );
    procedure btnsetupClick( Sender: TObject );
    procedure btnfullClick( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure PreviewImageClick( Sender: TObject );
    procedure btnpicClick( Sender: TObject );

  private
    fGrid:       TARSortGrid;
    fPrintImage: TBitmap;

    procedure SetGrid( Const Value: TARSortGrid );
    procedure FullSize;
    procedure SetPrintImage( Const Value: TBitmap );
    procedure Zoom( factor: Extended );

  published
   property Grid: TARSortGrid read fGrid write SetGrid;
   property PrintImage: TBitmap read fPrintImage write SetPrintImage;
end;

var
  SortGridPreviewForm: TARSortGridPreviewForm;

IMPLEMENTATION

{$R *.DFM}

procedure TARSortGridPreviewForm.SetGrid( const Value: TARSortGrid );
begin
   fGrid := Value;
end;

procedure TARSortGridPreviewForm.btnshowClick( Sender: TObject );
begin
   if Assigned( fGrid ) then
   begin
      fGrid.UpdatePreview( fPrintImage.Canvas );
      PreviewImage.Picture.Bitmap.Assign( fPrintImage );
   end;
end;

procedure TARSortGridPreviewForm.MarginsClick( Sender: TObject );
var
   index, s: Integer;
begin
   index := margins.ItemIndex;
   case index of
     0: s := Grid.PrintOptions.MarginTop;
     1: s := Grid.PrintOptions.PageTitleMargin;
     2: s := Grid.PrintOptions.MarginLeft;
     3: s := Grid.PrintOptions.MarginRight;
     4: s := Grid.PrintOptions.MarginBottom;
     5: s := Grid.PrintOptions.Leftpadding;
     6: s := Grid.PrintOptions.HeaderSize;
     7: s := Grid.PrintOptions.FooterSize;
     else
       Exit;
   end;
   Margin.Text := IntToStr( s );
end;

procedure TARSortGridPreviewForm.btnprintClick( Sender: TObject );
begin
   Grid.Print;
end;

procedure TARSortGridPreviewForm.MarginChange( Sender: TObject );
var
   index, v: Integer;
begin
   index := margins.ItemIndex;
   v := StrToInt( Margin.Text );
   case index of
     0: Grid.PrintOptions.MarginTop := v;
     1: Grid.PrintOptions.PageTitleMargin := v;
     2: Grid.PrintOptions.MarginLeft := v;
     3: Grid.PrintOptions.MarginRight := v;
     4: Grid.PrintOptions.MarginBottom := v;
     5: Grid.PrintOptions.Leftpadding := v;
     6: Grid.PrintOptions.HeaderSize := v;
     7: Grid.PrintOptions.FooterSize := v;
   else
     Exit;
   end;
   if cklive.Checked then btnshow.Click;
end;

procedure TARSortGridPreviewForm.HeaderChange( Sender: TObject );
var
   index: Integer;
begin
   index := headers.ItemIndex ;
   if index = -1 then Exit;
   if index = 0 then
      Grid.PrintOptions.PageTitle := header.Text
   else if index=1 then
      Grid.PrintOptions.PageFooter := header.Text
   else if index=2 then
      Grid.PrintOptions.DateFormat :=header.Text
   else if index=3 then
     Grid.PrintOptions.TimeFormat := header.Text
   else if index=4 then
     Grid.PrintOptions.Logo := header.Text;
   if cklive.Checked then btnshow.Click;
end;

procedure TARSortGridPreviewForm.FormShow( Sender: TObject );
begin
   Header.Text := Grid.PrintOptions.PageTitle;
   Margin.Text := IntToStr( Grid.Printoptions.MarginTop );
   Margins.ItemIndex := 0;
//   Previewpage.MaxValue := Grid.PageCount;
   lblpages.Caption := 'of' + ' ' + IntToStr( Grid.PageCount );
   Grid.PrintOptions.PreviewPage := 1;
   PreviewPage.Text := '1';
   ckBorders.Checked := (Grid.PrintOptions.Borderstyle = bsSingle);
   Header.Text := Grid.PrintOptions.PageTitle;
   Headers.ItemIndex := 0;
end;

procedure TARSortGridPreviewForm.ckbordersClick( Sender: TObject );
begin
   if ckborders.Checked then
      Grid.PrintOptions.BorderStyle := bsSingle
   else
     Grid.PrintOptions.BorderStyle := bsNone;
   if cklive.Checked then btnshow.Click;
end;

procedure TARSortGridPreviewForm.previewPageChange( Sender: TObject );
begin
   Grid.PrintOptions.PreviewPage := StrToInt( PreviewPage.Text );
   if cklive.Checked then btnshow.Click;
end;

procedure TARSortGridPreviewForm.HeadersClick( Sender: TObject );
var
   index: Integer;
begin
   index := headers.ItemIndex;
   if index = -1 then Exit;
   btnpic.Enabled := False;
   if index = 0 then
     header.Text := Grid.PrintOptions.PageTitle
   else if index = 1 then
     header.Text := Grid.PrintOptions.PageFooter
   else if index = 2 then
     header.Text := Grid.PrintOptions.DateFormat
   else if index = 3 then
     header.Text := Grid.PrintOptions.TimeFormat
   else if index = 4 then
   begin
     header.Text := Grid.PrintOptions.Logo;
     btnpic.Enabled := True;
   end;
end;

procedure TARSortGridPreviewForm.ckliveClick( Sender: TObject );
begin
   if cklive.checked then btnshow.Click ;
end;

procedure TARSortGridPreviewForm.btnsetupClick( Sender: TObject );
begin
   if PrinterSetupDialog1.Execute then
   begin
      Grid.PrintOptions.Orientation := Printer.Orientation;
//      Previewpage.MaxValue := Grid.PageCount;
      lblpages.Caption := 'of' + ' ' + IntToStr( Grid.PageCount );
      if cklive.checked then btnshow.Click;
   end;
end;

procedure TARSortGridPreviewForm.FullSize;
var
   bm:   TBitmap;
   w, h: Integer;
begin
   w := PrintImage.Width;
   h := PrintImage.Height ;
   bm := TBitmap.Create;
   bm.Width := Scrollbox1.ClientWidth;
   bm.Height := Round( h / w * bm.Width );
   PrintImage.PixelFormat := pf24bit;
   bm.PixelFormat := pf24bit;
   fGrid.SmoothResize( fPrintImage, bm );
   PreviewImage.Picture.Bitmap.Assign( bm );
   bm.Free;
end;

procedure TARSortGridPreviewForm.btnfullClick( Sender: TObject );
begin
   FullSize;
end;

procedure TARSortGridPreviewForm.SetPrintImage( const Value: TBitmap );
begin
   fPrintImage := Value;
end;

procedure TARSortGridPreviewForm.FormCreate( Sender: TObject );
begin
   fPrintImage := TBitmap.Create ;
end;

procedure TARSortGridPreviewForm.FormDestroy( Sender: TObject );
begin
   fPrintImage.Free;
end;

procedure TARSortGridPreviewForm.PreviewImageClick( Sender: TObject );
var
   w, w1: Integer;
begin
   w1 := PreviewImage.Picture.Bitmap.Width;
   w  := fPrintImage.Width;
   if ( Round( w * 0.8 ) < w1 ) then
   begin
      PreviewImage.Picture.Bitmap.Assign( fPrintImage );
   end else
   begin
      Zoom( w1 / w / 0.8 );
   end;
end;

procedure TARSortGridPreviewForm.Zoom( factor: Extended );
var bm:   TBitmap;
    w, h: Integer;
begin
   w := PrintImage.Width;
   h := PrintImage.Height ;
   bm := TBitmap.Create;
   bm.Width := Round( factor * w );
   bm.Height:= Round( h / w * bm.Width );
   PrintImage.PixelFormat := pf24bit;
   bm.PixelFormat := pf24bit;
   fGrid.SmoothResize( fPrintImage, bm );
   Previewimage.Picture.Bitmap.Assign( bm );
   bm.Free;
end;

procedure TARSortGridPreviewForm.btnpicClick( Sender: TObject );
begin
   if OpenPictureDialog1.Execute then if Headers.ItemIndex = 4 then
   begin
      header.Text := OpenPictureDialog1.Filename;
      Grid.PrintOptions.Logo := OpenPictureDialog1.Filename;
   end;
end;

End.
