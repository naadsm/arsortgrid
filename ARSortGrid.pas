{*******************************************************************************
   Class: TARSortGrid
   Copyright 1996,1997,1999
   Author: Bill Menees
           bmenees@usit.net
		   www.public.usit.net/bmenees
   Modified by Eric W. Engler, Feb 1997
    - fixed a bug in autodetection of type
    - OnBeginSort event was called before the autodetect of type; moved to after.
    - expanded date sort to incl datetime (these are usu compatible in Delphi)
    - added a time sort
   Modified by Ivo Eichler <eichler@sce.cz>, Jan 1999
    - now supports the national string setting in the control panel by
      using ANSICompare and ANSIUppercase functions.
   Modified by Chris Vleghert Jan. 19 1999.
    - Fixed a memory leak in the procedure TSortedList.Reset;
   Modified by Andrea Gnesutta Mar. 8 1999.  (gnes@zero.it)
    - Added property LastSortDirection (ReadOnly)
    - Added property SortState (ReadOnly)
    - Added property LastSortedCol (ReadOnly)
   Modified by Chris Vleghert Mar. 10 1999. (RCV02)
    - Changed property LastSortDirection into SortDirection (R/W)
    - Changed property LastSortedCol into SortColumn (R/W)
    - Changed property ClickSorting into SortOnClick
    - Added property SortSymbol
    - Added property SortSpacingHor
    - Added property SortSpacingVert
    - Added property SortBitMapA
    - Added property SortBitMapD
    - Added property SortFooter
    - Added property FooterRows
    - Added property FooterColor
    - Added property FooterFont
    - Added property FooterBevelStyle
    - Added function InsertRows
    - Added function InsertCols
    - Added function ClearRows
    - Added function ClearFrom
    - Added function RemoveRows
    - Added function RemoveCols
    - Added function ClearCols
    - Added function FindFirst
    - Added function FindNext
    - Added event OnMouseEnter
    - Added event OnMouseLeave
   Modified by Chris Vleghert Jan. 1 2000. (RCV03)
   (integrated MultiGrd http://www.pablop.demon.co.uk/marley/tmultigrid.htm into SortGrid)
    - Added extended multiselect
    - Added property Selected[RowNumber], do not use the Selection property anymore!
    - Added property SelectedCount
    - Added property SelectedItems[1 to SelectedCount];
    - Added function ClearSelection
   Modified by Chris Vleghert May. 11 2000. (RCV04)
    - Added method ShowRows( StartRow, EndRow: LongInt )
    - Added method HideRows( StartRow, EndRow: LongInt )
    - Added method AutoSizeRow( aRow: LongInt )
    - Added method AutoSizeRows( StartRow, EndRow: LongInt )
    - Added method AutoSizeVisibleRows( StartRow, EndRow: LongInt )
    - Added method AutoSizeHiddenRows( StartRow, EndRow: LongInt )
    - Added method HideCols( StartCol, EndCol: LongInt )
    - Added method ShowCols( StartCol, EndCol: LongInt )
    - Added method Print            only D4+ and BCB4+
    - Added method PrintPreview     only D4+ and BCB4+
    - Added method PageCount        only D4+ and BCB4+
    - Added method UpdatePreview( aCanvas: TCanvas )         only D4+ and BCB4+
    - Added property PrintOptions   only D4+ and BCB4+
    - Added property WordWrap
   Modified by Chris Vleghert Jul. 28 2000, Bug report: Florian Schick
    - FindFirst and FindNext did not do what they were supposed to be doing.
   Modified by Russell Peters, Jan 2003
    - Stop comparison exceptions (Versions 6+)
   
   Modified by Aaron Reeves, Feb 2005 (TARSortGrid Version 3.0.0)
   
    Modified version Copyright (c) 2005 - 2009 Animal Population Health Institute at Colorado State University
	  This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
		Public License as published by the Free Software Foundation; either version 2 of the License, or
		(at your option) any later version.    
    
    Website: http://www.naadsm.org/opensource/delphi
    
    - Added method AppendRow()
    - Integrated filter (only one filter at a time) and a few other capabilities from
      K. Bendsen's version of TSortGrid 1.30 (see http://delphi.icm.edu.pl/authors/a0002663.htm).
    - Sort order indicator now precedes the column caption.
    - Header row is no longer duplicated when printing.
    - Selected row changes to indicate location of the found item upon Find or FindNext.
    - Changed behavior of mouse click: one click on a column sorts ascending, the
      next click on the same column reverses sort direction.  Right click is no longer
      used.
    - Added property SortEnabled
    - Added property FilterEnabled to TSearchOptions
    - Added property PartialMatch to TSearchOptions
    - Added property IsReadOnly, which disallows editing
    - Added method DeleteRows, an alias for RemoveRows
    - Added method ClearRow
    - OnBeginSort and OnEndSort are now called even when there is only one data row in the grid .
    - Added TCustomFilterFunction for use with TSearchOptions

   Modified by Aaron Reeves, May 2007 (TARSortGrid Version 3.1.0)
    - Added event OnVScroll
    - Added event OnHScroll
    - Added event OnMouseWheel

   Modified by Aaron Reeves, February 2009 (TARSortGrid version 3.1.1)
    - CSV output now uses the list separator specified by a system's regional settings

   Modified by Aaron Reeves, April 2009 (TARSortGrid version 3.1.2)
    - CSV output now uses the list separator specified in unit I88n, from the APHI General Purpose Delphi Library

   IMPORTANT! For users of old versions of Delphi (5 or earlier),
   Do NOT use BCB's or Delphi's "Break on Exception" option if
   you run a program that uses this component from the GUI.  This
   VCL uses exceptions during normal processing.
***************************************************************************}

UNIT ARSortGrid;

{$INCLUDE DelphiVers.inc}


(*
STUFF TO FIX
------------
- Proportional scrollbars don't quite work properly
*)

INTERFACE

  uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    Grids,
    Printers
  ;

type TARSortGrid = class; // Complete definition follows.

  {TSortCompare must return < 0 if Str1 is less than Str2,
   0 if they are equal, and > 0 if Str1 is greater than Str2.}
{type} TSortCompare = function (const Str1, Str2: String): Integer;

  { TCustomFilterFunction is used with TSearchOptions.
  Unlike TSortCompare, it is not limited to simple comparison
  of 2 strings.  TCustomFilterFunction takes a row number as a
  parameter, and returns true if the contents of that row
  in the grid match whatever criteria are imposed by the
  function.}
{type} TCustomFilterFunction = function( grid: TARSortGrid; iRow: integer ): Boolean of object;

{type} TSortDirection = ( sdAscending, sdDescending );
{type} TSortStyle     = ( ssAutomatic, ssAlphabetic, ssNumeric, ssDateTime, ssTime,ssCustom );
{type} TSortSymbol    = ( sgNone, sgArrow, sgGlyph, sgBendsenArrow, sgCustom );
{type} TSortState     = ( ssUnsorted, ssSorted );  // Line modified/added by gnes@zero.it
{type} TAutoSize      = ( asAll, asVisible, asHidden );
{type} TPrintMode     = ( pmPrint, pmPreview, pmPageCount );

  {**************************************************************}
  {*** NOTE: These are the options you can set to affect sorting.}
{type} TSortOptions = record
      SortStyle:         TSortStyle;
      SortDirection:     TSortDirection;
      SortCaseSensitive: Boolean;
      SortCompare:       TSortCompare;  //Used only if SortStyle = ssCustom.
  end;

{type} TSortedListEntry = record
      Str:    String;
      RowNum: LongInt;
  end;

{type} pSortedListEntry = ^TSortedListEntry;

{type} TSortedList = class( TList )
  public
     function GetItem( const i: Integer ): pSortedListEntry;
     procedure Reset;
  end;

{type} TOldRowColEntry = record
      IsRowCol:    Integer;
	  RowColNum:   LongInt;
      RowColValue: Integer;
  end;

{type} pOldRowColEntry = ^TOldRowColEntry;

{type} TOldRowColValueList = class( TList )
  public
     procedure SetValue( const rc: LongInt; Const rcValue, IsRC: Integer );
     function  GetValue( const rc: LongInt; Const IsRC: Integer ): Integer;
     procedure RemRC( Const rc: LongInt; Const IsRC: Integer );
     procedure MoveUp( Const rc: LongInt; Const IsRC: Integer );
     procedure MoveDown( Const rc: LongInt; Const IsRC: Integer );
     procedure Reset;
  end;

{type} TCellBevelStyle = ( cbNone, cbRaised, cbLowered );
  {**********************************************************}
  {*** NOTE: This is one of the structures in TFormatOptions.}
{type} TCellBevel = record
      Style:           TCellBevelStyle;
      UpperLeftColor:  TColor;
      LowerRightColor: TColor;
  end;

{type} TVertAlignment = ( taTopJustify, taBottomJustify, taMiddle );

  {**************************************************}
  {*** NOTE: These are the display options you can set
             for each cell in OnGetCellFormat.}
{type} TFormatOptions = record
      Brush:         TBrush;
      Font:          TFont;
      AlignmentHorz: TAlignment;
      AlignmentVert: TVertAlignment;
      Bevel:         TCellBevel;
      HideText:      Boolean;
  end;

{type} TPrintOptions = class( TPersistent )
    private
      fJobTitle:        String;
      fPageTitle:       String;
      fPageTitleMargin: Cardinal;
      fCopies:          Cardinal;
      fPreviewPage:     Cardinal;
      fFromRow:         Cardinal;
      fToRow:           Cardinal;
	    fBorderStyle:     TBorderStyle;
      fLeftPadding:     Cardinal;
      fMarginBottom:    Cardinal;
      fMarginLeft:      Cardinal;
      fMarginTop:       Cardinal;
      fMarginRight:     Cardinal;
      fPageFooter:      String;
      fDateFormat:      String;
      fTimeFormat:      String;
      fHeaderSize:      Cardinal;
      fFooterSize:      Cardinal;
      fOrientation:     TPrinterOrientation;
      fLogo:            String;

      procedure SetMarginBottom( Const Value: Cardinal );
      procedure SetMarginLeft( Const Value: Cardinal );
      procedure SetMarginTop( Const Value: Cardinal );
      procedure SetMarginRight( Const Value: Cardinal );
      procedure SetPageFooter( Const Value: String );
      procedure SetDateFormat( Const Value: String );
      procedure SetTimeFormat( Const Value: String );
      procedure SetFooterSize( Const Value: Cardinal);
      procedure SetHeaderSize( Const Value: Cardinal );
	    procedure SetOrientation( Const Value: TPrinterOrientation );
      procedure SetLogo( Const Value: String );

    public
      constructor Create;

    published
      property Orientation: TPrinterOrientation read fOrientation write SetOrientation;
      property JobTitle: String read fJobTitle write fJobTitle;
      property PageTitle: String read fPageTitle write fPageTitle;
      property Logo: String read fLogo write SetLogo;
      property PageTitleMargin: Cardinal read fpageTitleMargin write fpageTitleMargin;
      property PageFooter: String read fPageFooter write SetPageFooter;
      property HeaderSize: Cardinal read fHeaderSize write SetHeaderSize;
      property FooterSize: Cardinal read fFooterSize write SetFooterSize;
      property DateFormat: String read fDateFormat write SetDateFormat;
      property TimeFormat: String read fTimeFormat write SetTimeFormat;
      property Copies: Cardinal read fCopies write fCopies default 1;
      property FromRow: Cardinal read fFromRow write fFromRow;
      property ToRow: Cardinal read fToRow write fToRow;
      property PreviewPage: Cardinal read fPreviewPage write fPreviewPage default 1;
      property BorderStyle: TBorderstyle read fBorderStyle write fBorderStyle;
      property Leftpadding: Cardinal read fLeftpadding write fLeftPadding default 2;
	    property MarginBottom: Cardinal read fMarginBottom write SetMarginBottom;
      property MarginLeft: Cardinal read fMarginLeft write SetMarginLeft;
      property MarginTop: Cardinal read fMarginTop write SetMarginTop;
      property MarginRight: Cardinal read fMarginRight write SetMarginRight;
  end;

  {These are the new events defined for TARSortGrid.}
{type} TFormatDrawCellEvent = procedure( Sender: TObject; Col, Row: LongInt; State: TGridDrawState;
                                    var FormatOptions: TFormatOptions ) of object;
{type} TClickSortEvent      = procedure( Sender: TObject; Col, Row: LongInt; var SortOptions: TSortOptions ) of object;
{type} TUpdateGridEvent     = procedure( Sender: TObject; Index: LongInt ) of object;
{type} TSizeChangedEvent    = procedure( Sender: TObject; OldColCount, OldRowCount: LongInt ) of object;
{type} TBeginSortEvent      = procedure( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions ) of object;
{type} TEndSortEvent        = procedure( Sender: TObject; Col: LongInt ) of object;
{type} TCellValidateEvent   = procedure( Sender: TObject; Col, Row: LongInt; var Value: String;
                                    var Valid: Boolean ) of object;

  {Forward declaration: full class declaration follows}
{type} TSearchOptions = class;

  {Here's the main new class: TARSortGrid}
{type} TARSortGrid = class( TStringGrid )
  private
    fMyForm:                 TForm;
    fSearchOptions:          TSearchOptions;
    fSortedList:             TSortedList;
    fSortEnabled:            Boolean;
    fAlignmentHorz:          TAlignment;
	  fAlignmentVert:          TVertAlignment;
    fBevelStyle:             TCellBevelStyle;
    fSortState:              TSortState;               // Line modified/added by gnes@zero.it
    fSortDirection:          TSortDirection; //RCV02
    fSortStyle:              TSortStyle;
    fAutoSizeCols:           Boolean;
    fProportionalScrollBars: Boolean;
    fCaseSensitive:          Boolean;
    fExtendedKeys:           Boolean;
    fSorting:                Boolean;
    fModified:               Boolean;
    fOldModifiedValue:       Boolean;
    fEntered:                Boolean;
    fSortOnClick:            Boolean; //RCV02
    fSortFooter:             Boolean; //RCV02
    fOldCellText:            String;
    fOldCol, fOldRow:        LongInt;
    fSortSymbol:             TSortSymbol; //RCV02
    fSortSpacingHor:         Integer; //RCV02
    fSortSpacingVert:        Integer; //RCV02
    fSortColumn:             Integer; // Line modified/added by gnes@zero.it
    fFooterRows:             Integer; //RCV02
    gFooterSub:              Integer; //RCV02
    fSortBMA:                TBitmap; //RCV02
    fSortBMD:                TBitmap; //RCV02
	  fFooterColor:            TColor;  //RCV02
    fFooterFont:             TFont;   //RCV02
    fFooterBevelStyle:       TCellBevelStyle; //RCV02

    fSelectedRows:           TList;    //RCV03
    fAnchor:                 LongInt;  //RCV03
    fLastMoveOn:             LongInt;  //RCV03
    fLastTopRow:             LongInt;  //RCV03
    fMouseIsDown:            Boolean;  //RCV03

    fOldRowCol:              TOldRowColValueList;  //RCV04
    fWordWrap:               Boolean;  //RCV04
    {$IfDef VERD4+}
    fPrintOptions:           TPrintOptions;  //RCV04
    fPageCount:              Cardinal; //RCV04
    fPrintImage:             TBitmap;  //RCV04
    {$EndIf}
    fOldFont:                TFont;    //RCV04
    fOldBrush:               TBrush;   //RCV04

    fOnGetCellFormat:        TFormatDrawCellEvent;
    fOnClickSort:            TClickSortEvent;
    fOnRowInsert:            TUpdateGridEvent;
	  fOnRowDelete:            TUpdateGridEvent;
    fOnColumnInsert:         TUpdateGridEvent;
    fOnColumnDelete:         TUpdateGridEvent;
    fOnColumnWidthsChanged:  TNotifyEvent;
    fOnRowHeightsChanged:    TNotifyEvent;
    fOnSizeChanged:          TSizeChangedEvent;
    fOnBeginSort:            TBeginSortEvent;
    fOnEndSort:              TEndSortEvent;
    fOnCellValidate:         TCellValidateEvent;
    fOnMouseEnter:           TNotifyEvent; //RCV02
    fOnMouseLeave:           TNotifyEvent; //RCV02

    fOnVScroll:              TNotifyEvent;
    fOnHScroll:              TNotifyEvent;
    fOnMouseWheel:           TNotifyEvent;

    GSortBM:     TBitmap; // RCV02
    GASE:        Boolean; //RCV02
    GStartRow, GEndRow, GStartCol, GEndCol: LongInt; //RCV02

    OriginalWindowProc: TWndMethod; // used for scroll events
    procedure NewWindowProc(var Message: TMessage); // used for scroll events

    function Search(StartRow: Cardinal): Boolean;

    procedure SetIsReadOnly( value: Boolean );
    function GetIsReadOnly(): Boolean;

    procedure setSortEnabled( val: boolean );

    procedure SetSortBMA( Value: TBitmap ); //RCV02
    procedure SetSortBMD( Value: TBitmap ); //RCV02
    procedure SetSortSymbol( Value: TSortSymbol ); //RCV02
    procedure SetBevelStyle( Value: TCellBevelStyle );
    procedure SetSortColumn( Value: Integer ); //RCV02
    procedure SetSortStyle( value: TSortStyle );
    procedure SetCaseSensitive( value: boolean );
    procedure setAutoSizeCols( value: boolean );
    procedure SetSortOnClick( Value: Boolean ); //RCV02
	  procedure SetSortFooter( Value: Boolean ); //RCV02
    procedure SetAlignmentHorz( Value: TAlignment );
    procedure SetAlignmentVert( Value: TVertAlignment );
    procedure SetSortDirection( Value: TSortDirection ); //RCV02
    procedure SetSortSpacingHor( Value: Integer ); //RCV02
    procedure SetSortSpacingVert( Value: Integer ); //RCV02
    procedure SetProportionalScrollBars( Value: Boolean );
    procedure SetFooterRows( Value: Integer ); //RCV02
    procedure SetFooterColor( Value: TColor ); //RCV02
    procedure SetFooterFont( Value: TFont ); //RCV02
    procedure SetFooterBevelStyle( Value: TCellBevelStyle );
    function  GetSelected( Row: LongInt ): Boolean; //RCV03
    procedure SetSelected( Row: LongInt; Select: Boolean ); //RCV03
    function  GetSelectedCount: LongInt; //RCV03
    function  GetSelItems( Index: LongInt ): LongInt; //RCV03
    procedure SetWordWrap( const Value: Boolean ); //RCV04

    procedure SetGSortSymbol; //RCV02
    function  CheckRange( startcr, endcr: Integer; IsRow: Boolean): Boolean; //RCV02
    procedure SetResetASE( SetASE: Boolean ); //RCV02
    procedure ToggleRow( aRow: LongInt ); //RCV03
    procedure SelectRow( aRow: LongInt; Select: Boolean ); //RCV03
    procedure SelectRows( aRow, bRow : LongInt; Select: Boolean ); //RCV03
	  procedure InvalidateRow( aRow : LongInt ); //RCV03
    procedure AutoSizeRowsInt( StartRow, EndRow: LongInt; How: TAutoSize ); // RCV04
    procedure ShowRC( StartRC, EndRC: LongInt; IsRC: Integer ); // RCV04
    procedure HideRC( StartRC, EndRC: LongInt; IsRC: Integer ); // RCV04
    {$IfDef VERD4+}
    procedure DrawToCanvas( aCanvas: TCanvas; Mode: TPrintMode; FromRow, ToRow: Integer ); //RCV04
    {$EndIf}
    procedure WMSize( var Msg: TWMSize); message WM_SIZE;
    procedure CMMouseEnter( var Message ); message CM_MOUSEENTER;
    procedure CMMouseLeave( var Message ); message CM_MOUSELEAVE;

  protected
    procedure SetSearchOptions(value: TSearchOptions);
    procedure ListQuickSort( const aCol: LongInt; const SortOptions: TSortOptions ); virtual;
    function  DetermineSortStyle( const aCol: LongInt ): TSortStyle; virtual;
    procedure InitializeFormatOptions( const aCol, aRow: LongInt; var FmtOpts: TFormatOptions );
    procedure DrawCell( aCol, aRow: LongInt; aRect: TRect; aState: TGridDrawState ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure ColWidthsChanged; override;
    procedure RowHeightsChanged; override;
    procedure SizeChanged( OldColCount, OldRowCount: LongInt ); override;
    procedure UpdateScrollPage; virtual;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
	  procedure SetEditText( aCol, aRow: LongInt; const Value: string ); override;
    procedure Click; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure InitValidate; virtual;
    procedure KeyPress( var Key: Char ); override;
    procedure DrawSortSymbol( x, y: Integer ); //RCV02
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );override; //RCV03
    procedure MouseMove( Shift: TShiftState; X, Y: Integer );override; //RCV03
    procedure TopLeftChanged; override; //RCV03

  public
    GSortBMWidth: Integer;
    GSortBMHeight: Integer;

    { run time properties }
    property  Sorting: Boolean read fSorting default False;
    property  Modified: Boolean read fModified write fModified default False;
    property  SortState: TSortState read fSortState;                       // Line modified/added by gnes@zero.it
    property  Selected[ Row: LongInt ]: Boolean read GetSelected write SetSelected; //RCV03
    property  SelectedCount: LongInt read GetSelectedCount; //RCV03
    property  SelectedItems[ Index: LongInt ]: LongInt read GetSelItems; //RCV03

    constructor Create( AOwner: TComponent ); override;
    destructor  Destroy; override;

    procedure clickSort( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );

    procedure MoveTo( aCol, aRow: LongInt ); virtual;
    function  Clear: Boolean; virtual;
    function  ClearFrom( FromRow: LongInt ): Boolean; virtual; //RCV02
    procedure InsertRow( aRow: LongInt ); virtual;
    function  AppendRow(): longint; virtual;
    procedure InsertColumn( aCol: LongInt ); virtual;
    procedure DeleteRow( aRow: LongInt ); {$IfDef VERD4+}reintroduce; {$EndIf} virtual;
    function  DeleteRows( sRow, eRow: integer ): Boolean;
    procedure DeleteColumn( aCol: LongInt ); {$IfDef VERD4+}reintroduce; {$EndIf}virtual;
    procedure MoveRow( FromIndex, ToIndex: LongInt ); virtual;
    procedure MoveColumn( FromIndex, ToIndex: LongInt ); virtual;
    procedure SwapRows( aRow1, aRow2: LongInt ); virtual;
    procedure SwapColumns( aCol1, aCol2: LongInt ); virtual;
    procedure AutoSizeCol( const aCol: LongInt ); virtual;
    procedure AutoSizeColumns( const DoFixedCols: Boolean; const Padding: Integer ); overload; virtual;
    procedure AutoSizeColumns(); overload; virtual;
    procedure SortByColumn( const aCol: LongInt; SortOptions: TSortOptions ); overload; virtual;
    procedure SortByColumn( const aCol: longint ); overload; virtual;
    function  IsCell( const Value: String; var aCol, aRow: LongInt ): Boolean; virtual;
    procedure LoadFromFile( const FileName: String; const Delimiter: Char; sortWhenLoaded: boolean = false ); virtual;
    procedure SaveToFile( const FileName: String; const Delimiter: Char ); virtual;
    function  delimitedText( const Delimiter: Char ): string; virtual;
    function  csvText(): string; virtual;
    function  CanUndoSort: Boolean; virtual;
    procedure UndoSort; virtual;
	  function  GetCellDrawState( const aCol, aRow: LongInt ): TGridDrawState;
    function  SelectCell( aCol, aRow: LongInt ): Boolean; override;
    procedure ValidateCell; virtual;
    function  InsertRows( aRow, rCount: Integer ): Boolean; virtual; //RCV02
    function  InsertCols( aCol, cCount: Integer ): Boolean; virtual; //RCV02
    function  ClearRows( sRow, eRow: Integer ): Boolean; virtual; //RCV02
    function  ClearRow( aRow: Integer ): Boolean;
    function  RemoveRows( sRow, eRow: Integer ): Boolean; virtual; //RCV02
    function  RemoveCols( sCol, eCol: Integer ): Boolean; virtual; //RCV02
    function  ClearCols( sCol, eCol: Integer ): Boolean; virtual; //RCV02
    function  FindFirst(): boolean; overload; virtual;
    function  FindFirst( const aStr: string ): boolean; overload; virtual;
    function  FindFirst( const aStr: String; var sCol, sRow: LongInt; eCol, eRow: LongInt ): Boolean; overload; virtual; //RCV02
    function  FindNext( var aCol, aRow: LongInt ): Boolean; overload; virtual; //RCV02
    function  FindNext(): Boolean; overload; virtual;
    procedure ClearSelection; //RCV03
    procedure ShowRows( StartRow, EndRow: LongInt ); virtual; //RCV04
    procedure HideRows( StartRow, EndRow: LongInt ); virtual; //RCV04
    procedure ShowCols( StartCol, EndCol: LongInt ); virtual; //RCV04
    procedure HideCols( StartCol, EndCol: LongInt ); virtual; //RCV04
    procedure AutoSizeRow( aRow: LongInt ); virtual; //RCV04
    procedure AutoSizeRows( StartRow, EndRow: LongInt ); virtual; //RCV04
    procedure AutoSizeVisibleRows( StartRow, EndRow: LongInt ); virtual; //RCV04
    procedure AutoSizeHiddenRows( StartRow, EndRow: LongInt ); virtual; //RCV04
    {$IfDef VERD4+}
    procedure Print; //RCV04
    procedure PrintPreview; //RCV04
	  procedure UpdatePreview( aCanvas: TCanvas ); //RCV04
    function  PageCount: Integer; //RCV04
    procedure SmoothResize( var Src, Dst: TBitmap ); //RCV04
    {$EndIf}

    procedure setCursorPosition( const col, row: integer; pos: integer ); // AR 4/22/09
    function getCursorPosition(): integer; // AR 4/22/09
    procedure lockCell(); // AR 4/22/09
    procedure unlockCell(); // AR 4/22/09
    procedure selectCellContents(); // AR 6/23/11

  published
    { Published properties }
    property SortEnabled: Boolean read fSortEnabled write setSortEnabled default true;
    property IsReadOnly: Boolean read getIsReadOnly write setIsReadOnly;
    property SearchOptions: TSearchOptions read fSearchOptions write SetSearchOptions;
    property CaseSensitive: Boolean read fCaseSensitive write setCaseSensitive default false;
    property AlignmentHorz: TAlignment read fAlignmentHorz write SetAlignmentHorz;
    property AlignmentVert: TVertAlignment read fAlignmentVert write SetAlignmentVert;
    property BevelStyle: TCellBevelStyle read fBevelStyle write SetBevelStyle default cbNone;
    property ProportionalScrollBars: Boolean read fProportionalScrollBars write SetProportionalScrollBars;
    property ExtendedKeys: Boolean read fExtendedKeys write fExtendedKeys;
    property SortSymbol: TSortSymbol read FSortSymbol write SetSortSymbol default sgNone; //RCV02
    property SortSpacingHor: Integer read FSortSpacingHor write SetSortSpacingHor default 4; //RCV02
    property SortSpacingVert: Integer read FSortSpacingVert write SetSortSpacingVert default 0; //RCV02
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection default sdAscending; //RCV02
    property SortBitMapA: TBitmap read FSortBMA write SetSortBMA nodefault; //RCV02
    property SortBitMapD: TBitmap read FSortBMD write SetSortBMD nodefault; //RCV02
    property SortColumn: Integer read FSortColumn write SetSortColumn default 0; //RCV02
    property SortStyle: TSortStyle read fSortStyle write SetSortStyle default ssAutomatic;
    property autoSizeCols: Boolean read fAutoSizeCols write setAutoSizeCols default true;
    property SortOnClick: Boolean read FSortOnClick write SetSortOnClick default False; //RCV02
    property SortFooter: Boolean read FSortFooter write SetSortFooter default False; //RCV02
    property FooterRows: Integer read FFooterRows write SetFooterRows default 0; //RCV02
	  property FooterColor: TColor read FFooterColor write SetFooterColor default clAqua; //RCV02
    property FooterFont: TFont read FFooterFont write SetFooterFont; //RCV02
    property FooterBevelStyle: TCellBevelStyle read fFooterBevelStyle write SetFooterBevelStyle default cbNone; //RCV02
    {$IfDef VERD4+}
    property PrintOptions: TPrintOptions read fPrintOptions write fPrintOptions; //RCV04
    {$EndIf}
    property WordWrap: Boolean read fWordWrap write SetWordWrap; //RCV04

    { Published events }
    property OnGetCellFormat: TFormatDrawCellEvent read fOnGetCellFormat write fOnGetCellFormat;
    property OnClickSort: TClickSortEvent read fOnClickSort write fOnClickSort;
    property OnRowInsert: TUpdateGridEvent read fOnRowInsert write fOnRowInsert;
    property OnRowDelete: TUpdateGridEvent read fOnRowDelete write fOnRowDelete;
    property OnColumnInsert: TUpdateGridEvent read fOnColumnInsert write fOnColumnInsert;
    property OnColumnDelete: TUpdateGridEvent read fOnColumnDelete write fOnColumnDelete;
    property OnColumnWidthsChanged: TNotifyEvent read fOnColumnWidthsChanged write fOnColumnWidthsChanged;
    property OnRowHeightsChanged: TNotifyEvent read fOnRowHeightsChanged write fOnRowHeightsChanged;
    property OnSizeChanged: TSizeChangedEvent read fOnSizeChanged write fOnSizeChanged;
    property OnBeginSort: TBeginSortEvent read fOnBeginSort write fOnBeginSort;
    property OnEndSort: TEndSortEvent read fOnEndSort write fOnEndSort;
    property OnCellValidate: TCellValidateEvent read fOnCellValidate write fOnCellValidate;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter; //RCV02
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave; //RCV02
    property OnVScroll: TNotifyEvent read fOnVScroll write fOnVScroll;
    property OnHScroll: TNotifyEvent read fOnHScroll write fOnHScroll;
    property OnMouseWheel: TNotifyEvent read fOnMouseWheel write fOnMouseWheel;

  end;


{type} TSearchOptions = class(TPersistent)
  private
    fGrid: TARSortGrid;
    fFiltered: Boolean;
    fFilterEnabled: Boolean;
    fSearchCol: Integer;
    fSearchText: String;
    fPartialMatch: Boolean;
    fFilterFunction: TCustomFilterFunction;
  protected
    procedure SetGrid(Grid: TARSortGrid);
    procedure SetFiltered(Value: Boolean);
    procedure RunFilter(Value: Boolean);
    property Grid: TARSortGrid read fGrid write SetGrid;
  public
    constructor Create;
    destructor Destroy; override;

    property FilterFunction: TCustomFilterFunction read fFilterFunction write fFilterFunction;
  published
    property Filtered: Boolean read fFiltered write SetFiltered default False;
    property FilterEnabled: Boolean read fFilterEnabled write fFilterEnabled default True;
    property SearchCol: Integer read fSearchCol write fSearchCol default -1;
    property SearchText: String read fSearchText write fSearchText;
    property PartialMatch: Boolean read fPartialMatch write fPartialMatch default False;
  end;



procedure Register;

function StringCompare( const Str1, Str2: String ): Integer;
function DateTimeCompare( const Str1, Str2: String ): Integer;
function NumericCompare( const Str1, Str2: String ): Integer;
function TimeCompare( const Str1, Str2: String ): Integer;   
{$IFDEF VERD5-}
// version 5 or less
function TryStrToFloat(s: string; var v: extended): boolean;
function TryStrToInt(s: string; var v: integer): boolean;
function TryStrToTime(s: string; var v: TDateTime): boolean;
function TryStrToDateTime(s: string; var v: TDateTime): boolean;
{$endif}

IMPLEMENTATION

uses
  {$IfDef VERD4+}
    ARSortGridPreview,
  {$EndIf}

  Math,
  StrUtils,

  ControlUtils,
  DebugWindow,
  I88n,
  MyStrUtils
;

{$R ARSortGrid.Res}
{$R BendsenSortGrid.Res}
{$R DefaultSort.Res}

var
   //This is here for Compare.  I can't pass it as a parameter,
   //and I can't make Compare a method.  So I had to use a global. :-(
   GlobalSortOptions: TSortOptions;

   FilterCount: Integer = 0; // Indicates how many records are hidden by the filter??


{$IFDEF VERD5-}
// version 5 or less                     
// IF YOU GET AN EXCEPTION HERE WHEN RUNNING FROM THE IDE,
// THEN YOU NEED TO TURN OFF "Break on Exception"
function TryStrToFloat(s: string; var v: extended): boolean;
begin
	Result:= true;
	try
		v := StrToFloat(s);
	except
		on EConvertError do
			Result := false;
	end;
end;
function TryStrToInt(s: string; var v: integer): boolean;
begin
	Result:= true;
	try
		v := StrToInt(s);
	except
		on EConvertError do
			Result := false;
	end;
end;
function TryStrToTime(s: string; var v: TDateTime): boolean;
begin
	Result:= true;
	try
		v := StrToTime(s);
	except
		on EConvertError do
			Result := false;
	end;
end;
function TryStrToDateTime(s: string; var v: TDateTime): boolean;
begin
	Result:= true;
	try
		v := StrToDateTime(s);
	except
		on EConvertError do
			Result := false;
	end;
end;
{$endif}

{******************************************************************************}
{** Miscellaneous Non-Member Functions                                       **}
{******************************************************************************}
procedure TokenizeGridString( const S: String; const Delimiter: Char; Tokens: TStringList );
var
   i, Len:   Integer;
   CurToken: String;
begin
     Tokens.Clear;
     CurToken := '';
     Len:=Length( S );
     for i := 1 to Len do
     begin
          if S[i] = Delimiter then
          begin
               Tokens.Add( CurToken );
               CurToken := '';
          end
          else
              CurToken := CurToken + S[i];
     end;
     Tokens.Add( CurToken );
end;

function StringCompare( const Str1, Str2: String ): Integer;
var
   c: Integer;
begin
   c := AnsiCompareStr( str1, str2 );
   if c < 0 then Result := -1
   else if c > 0 then Result := 1
   else Result := 0;
end;

function DateTimeCompare( const Str1, Str2: String ): Integer;
var
   Val1, Val2: TDateTime;
begin
	Result := 0;
   try
	if not (TryStrToDateTime( Str1 , Val1 ) and TryStrToDateTime( Str2 , Val2 )) then
		exit;
 {	  Val1 := StrToDateTime( Str1 );
	  Val2 := StrToDateTime( Str2 );  }
      if Val1 < Val2 then Result := -1
      else if Val2 < Val1 then Result := 1
      else Result := 0;
   except
      on EConvertError do Result := 0;
   end;
end;

function TimeCompare( const Str1, Str2: String ): Integer;
var
   Val1, Val2: TDateTime;
begin
	Result := 0;
   try                       
	if not (TryStrToTime( Str1 , Val1 ) and TryStrToTime( Str2 , Val2 )) then
		exit;
{      Val1:=StrToTime( Str1 );
	  Val2:=StrToTime( Str2 ); }
      if Val1 < Val2 then Result := -1
      else if Val2 < Val1 then Result := 1
      else Result := 0;
   except
      on EConvertError do Result := 0;
   end;
end;

function NumericCompare( const Str1, Str2: String ): Integer;
var
   Val1, Val2: Extended;
begin   
	Result := 0;
   try                  
	if not (TryStrToFloat( Str1 , Val1 ) and TryStrToFloat( Str2 , Val2 )) then
		exit;
{      Val1 := StrToFloat( Str1 );
	  Val2 := StrToFloat( Str2 ); }
      if Val1 < Val2 then Result := -1
      else if Val2 < Val1 then Result := 1
      else Result := 0;
   except
      on EConvertError do Result := 0;
   end;
end;

//This looks at the global variable GlobalSortOptions.
//I hated to use a global, but I can't pass any additional
//parameters to Compare, and I can't make Compare a
//method of an object.  A global seemed the only choice.
function Compare( Item1, Item2: Pointer ): Integer;
var
   Entry1, Entry2: pSortedListEntry;
begin
   Entry1 := Item1;
   Entry2 := Item2;

   //Handle Case-Insensitivity.
   if NOT GlobalSortOptions.SortCaseSensitive then
   begin
      Entry1^.Str := ANSIUppercase( Entry1^.Str );
      Entry2^.Str := ANSIUppercase( Entry2^.Str );
   end;

   //Determine compare type and do the comparison.
   case GlobalSortOptions.SortStyle of
      ssNumeric:  Result := NumericCompare( Entry1^.Str, Entry2^.Str );
      ssDateTime: Result := DateTimeCompare( Entry1^.Str, Entry2^.Str );
      ssTime:     Result := TimeCompare( Entry1^.Str, Entry2^.Str );
      ssCustom:   Result := GlobalSortOptions.SortCompare( Entry1^.Str, Entry2^.Str );
      else        Result := StringCompare( Entry1^.Str, Entry2^.Str );
   end;

   //Now, make sure we don't swap the rows if the Keys are equal.
   //If they're equal then we sort by row number.
   if Result = 0 then
   begin
      if Entry1^.RowNum < Entry2^.RowNum then Result := -1
      else if Entry1^.RowNum > Entry2^.RowNum then Result := 1
      else Result := 0; //Sometimes an item does get compared to itself.
   end
   else //Reverse polarity if descending sort.
      if GlobalSortOptions.SortDirection = sdDescending then
         Result := -1 * Result;
end;

{******************************************************************************}
{** TSearchOptions                                                           **}
{******************************************************************************}
constructor TSearchOptions.Create;
begin
  fFilterEnabled := true;
  Filtered := False;
  fSearchCol := -1;
  fSearchText := '';
  fPartialMatch := False;
end;

destructor TSearchOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TSearchOptions.SetGrid(Grid: TARSortGrid );
begin
  fGrid := Grid;
end;

procedure TSearchOptions.SetFiltered( Value: Boolean );
begin
  // If filtering is disabled, don't do anything
  if( not( filterEnabled ) ) then
    exit

  // If Value and Filtered are both already false, don't do anything.
  else if( not( Value ) and not( fFiltered ) ) then
    exit

  // If Value is false and Filter is true, then runFilter() to remove the existing filter
  else if( not( value ) and fFiltered ) then
    begin
      fFiltered := value;
      runFilter( false );
    end

  // If Value is true and Filter is true, runFilter() twice:
  // once to remove the existing filter, and again to install the new one.
  else if( value and fFiltered ) then
    begin
      fFiltered := value;
      runFilter( false );
      runFilter( true );
    end

  // If Value is true and Filter is false, runFilter() to install the new filter.
  else if( value and not( fFiltered ) ) then
    begin
      fFiltered := value;
      runFilter( true );
    end
  ;
end;

procedure TSearchOptions.RunFilter(Value: Boolean);
var
  gridOptions: TGridOptions;
  editorMode: Boolean;
begin
  if Assigned(FGrid) then
  begin
    if Value = True then
      begin
        FilterCount := 0;
        FGrid.Search(1);
      end
    else
      begin
        if FilterCount < 0 then
          begin
            FGrid.RowCount := FGrid.RowCount+(-1*FilterCount);
            FGrid.DeleteRow(1);
          end
        else
          begin
            FGrid.RowCount := FGrid.RowCount+FilterCount;
            FilterCount := 0;
          end
        ;
      end
    ;

    if( 0 > FGrid.SortColumn ) then FGrid.SortColumn := 0;

    gridOptions := FGrid.Options;
    editorMode := FGrid.EditorMode;

    FGrid.Options := FGrid.Options - [goEditing, goAlwaysShowEditor];
    FGrid.EditorMode := false;

    FGrid.SortByColumn( FGrid.SortColumn );

    FGrid.Options := gridOptions;
    FGrid.EditorMode := editorMode;
  end;
  
end;


{******************************************************************************}
{** Public Members for TSortedList                                           **}
{******************************************************************************}
function TSortedList.GetItem( const i: Integer ): pSortedListEntry;
begin
   //Cast the pointer.
   Result := pSortedListEntry( Items[ i ] );
end;

procedure TSortedList.Reset;
var
   i:     Integer;
   Item: pSortedListEntry;
begin
   //Dispose of anything in the list first.
   for i := 0 to Count - 1 do
   begin
	  if Items[i] <> nil then
      begin
         Item      := Items[ i ];
         Item^.Str := '';
         Dispose( Items[ i ] );
      end;
   end;
   // Now clear the list.
   Clear();
end;

{******************************************************************************}
{** Public Members for TOldRowColValue                                       **}
{******************************************************************************}
procedure TOldRowColValueList.SetValue( const rc: LongInt; Const rcValue, IsRC: Integer );
var
   Item: pOldRowColEntry;
begin
   New( Item );
   Item^.IsRowCol    := IsRC;
   Item^.RowColNum   := rc;
   Item^.RowColValue := rcValue;
   Add( Item );
end;

function TOldRowColValueList.GetValue( const rc: LongInt; Const IsRC: Integer ): Integer;
var
   i:     Integer;
   Item: pOldRowColEntry;
begin
   Result := -1;
   for i := 0 to Count - 1 do  // Find the row or column
   begin
      Item := Items[ i ];
      if (Item^.IsRowCol = IsRC) and (Item^.RowColNum = rc) then
      begin
         Result := Item^.RowColValue;
         Exit;
      end;
   end;
end;

procedure TOldRowColValueList.RemRC( Const rc: LongInt; Const IsRC: Integer );
var
   i:     Integer;
   Item: pOldRowColEntry;
begin
   for i := Count - 1 DownTo 0 do
   begin
      if Items[ i ] <> nil then
      begin
         Item := Items[ i ];
         if (Item^.IsRowCol = IsRC) and (Item^.RowColNum = rc) then
         begin
            Dispose( Item );
            Delete( i );
            Exit;
         end;
      end;
   end;
end;

procedure TOldRowColValueList.MoveUp( Const rc: LongInt; Const IsRC: Integer );
var
   i:     Integer;
   Item: pOldRowColEntry;
begin
   for i := 0 to Count - 1 do if Items[ i ] <> nil then
   begin
	  Item := Items[ i ];
      if (Item^.IsRowCol = IsRC) and (Item^.RowColNum >= rc) then
         Inc( Item^.RowColNum );
   end;

end;

procedure TOldRowColValueList.MoveDown( Const rc: LongInt; Const IsRC: Integer );
var
   i:     Integer;
   Item: pOldRowColEntry;
begin
   for i := 0 to Count - 1 do if Items[ i ] <> nil then
   begin
      Item := Items[ i ];
      if (Item^.IsRowCol = IsRC) and (Item^.RowColNum < rc) then
         Dec( Item^.RowColNum );
   end;
end;

procedure TOldRowColValueList.Reset;
var
   i:     Integer;
   Item: pOldRowColEntry;
begin
   for i := 0 to Count - 1 do   // Dispose of anything in the list first.
   begin
      if Items[ i ] <> nil then
      begin
         Item := Items[ i ];
         Dispose( Item );
      end;
   end;
   Clear();  // Now clear the list.
end;

{******************************************************************************}
{** Private Members for TARSortGrid                                          **}
{******************************************************************************}
procedure TARSortGrid.SetIsReadOnly( value: Boolean );
begin
  if( value ) then
    self.Options := self.Options - [goEditing]
  else
    self.Options := self.Options + [goEditing]
  ;
end;

function TARSortGrid.GetIsReadOnly(): Boolean;
begin
  result := not( goEditing in self.Options );
end;

procedure TARSortGrid.SetSearchOptions(value: TSearchOptions);
begin
 fSearchOptions.Assign(value);
end;

procedure TARSortGrid.SetAlignmentHorz(Value: TAlignment);
begin
   fAlignmentHorz:=Value;
   Invalidate;
end;

procedure TARSortGrid.SetAlignmentVert(Value: TVertAlignment);
begin
   fAlignmentVert:=Value;
   Invalidate;
end;

procedure TARSortGrid.SetBevelStyle(Value: TCellBevelStyle);
begin
   fBevelStyle:=Value;
   Invalidate;
end;

procedure TARSortGrid.WMSize(var Msg: TWMSize);
begin
   inherited;
   UpdateScrollPage;
end;

procedure TARSortGrid.SetProportionalScrollBars( Value: Boolean );
begin
   fProportionalScrollBars := Value;
   UpdateScrollPage;
end;

procedure TARSortGrid.SetSortBMA( Value: TBitmap ); //RCV02
begin
	if Value <> fSortBMA then
   begin
		fSortBMA.Assign( Value );
		SetGSortSymbol();
		InvalidateRow( 0 );
   end;
end;

procedure TARSortGrid.SetSortBMD( Value: TBitmap ); //RCV02
begin
	if Value <> fSortBMD then
   begin
		fSortBMD.Assign( Value );
		SetGSortSymbol();
		InvalidateRow( 0 );
	end;
end;

procedure TARSortGrid.SetSortOnClick( Value: Boolean ); //RCV02
begin
	if fSortOnClick <> Value then
   begin
		fSortOnClick := Value;
		InvalidateRow( 0 );
	end;
end;

procedure TARSortGrid.SetSortDirection( Value: TSortDirection ); //RCV02
begin
  fSortDirection := Value;
  SetGSortSymbol();

	if fSortDirection <> Value then
		InvalidateRow( 0 )
	;
end;

procedure TARSortGrid.SetSortColumn( Value: Integer ); //RCV02
begin
	if (fSortColumn <> Value) and {(Value >= 0) and} (Value < ColCount) then
   begin
		fSortColumn := Value;
		InvalidateRow( 0 );
	end;
end;

procedure TARSortGrid.SetSortStyle( value: TSortStyle );
begin
  if( value <> fSortStyle ) then
    begin
      fSortStyle := value;
      invalidateRow( 0 );
    end
  ;
end;

procedure TARSortGrid.setAutoSizeCols( value: boolean );
begin
  if( value <> fAutoSizeCols ) then
    begin
      fAutoSizeCols := value;
      if( fAutoSizeCols ) then autoSizeColumns( true, 0 );
    end
  ;
end;

procedure TARSortGrid.SetCaseSensitive( value: boolean );
begin
  if( value <> fCaseSensitive ) then
    begin
      fCaseSensitive := value;
      invalidateRow( 0 );
    end
  ;
end;

procedure TARSortGrid.SetSortSpacingHor( Value: Integer ); //RCV02
begin
	if fSortSpacingHor <> Value then
   begin
		fSortSpacingHor := Value;
		InvalidateRow( 0 );
	end;
end;

procedure TARSortGrid.SetSortSpacingVert( Value: Integer ); //RCV02
begin
   if fSortSpacingVert <> Value then
   begin
		fSortSpacingVert := Value;
		InvalidateRow( 0 );
	end;
end;

procedure TARSortGrid.SetSortSymbol( Value: TSortSymbol ); //RCV02
begin
	if fSortSymbol <> Value then
   begin
		fSortSymbol := Value;
		SetGSortSymbol();
		InvalidateRow( 0 );
   end;
end;

procedure TARSortGrid.CMMouseEnter( var Message ); //RCV02
begin
   if Assigned( fOnMouseEnter ) then
      fOnMouseEnter( Self );
end;

procedure TARSortGrid.CMMouseLeave( var Message ); //RCV02
begin
   if Assigned( fOnMouseLeave ) then
      fOnMouseLeave( Self );
end;


procedure TARSortGrid.NewWindowProc(var Message: TMessage);
begin
  OriginalWindowProc( Message );

  if( ( Message.msg = WM_Mousewheel ) and ( assigned( fOnMouseWheel ) ) ) then
    fOnMouseWheel( self )
  else if( ( Message.Msg = WM_VSCROLL ) and ( assigned( fOnVScroll ) ) ) then
    fOnVScroll( self )
  else if( ( Message.Msg = WM_HSCROLL ) and ( assigned( fOnHScroll ) ) ) then
    fOnHScroll( self )
  ;
end;

procedure TARSortGrid.SetSortFooter( Value: Boolean ); //RCV02
begin
   if fSortFooter <> Value then
      fSortFooter := Value;
   if fSortFooter then
      gFooterSub := 0
   else
      gFooterSub := fFooterRows;
end;

procedure TARSortGrid.SetFooterRows( Value: Integer ); //RCV02
var
   i, FootStart: Integer;
begin
   if (Value >= 0) and (Value <= RowCount - FixedRows) and (fFooterRows <> Value) then
   begin
      if Value > fFooterRows then
         FootStart := Value
      else
         FootStart := fFooterRows;
      for i := RowCount - 1 downto RowCount - FootStart do
         InvalidateRow( i );
      fFooterRows := Value;
   end;
   if NOT fSortFooter then
      gFooterSub := fFooterRows;
end;

procedure TARSortGrid.SetFooterColor( Value: TColor ); //RCV02
var
   i: Integer;
begin
   if fFooterColor <> Value then
   begin
      fFooterColor := Value;
      for i := RowCount - 1 downto RowCount - fFooterRows do
         InvalidateRow( i );
   end;
end;

procedure TARSortGrid.SetFooterFont( Value: TFont ); //RCV02
var
   i: Integer;
begin
   if fFooterFont <> Value then
   begin
      fFooterFont.Assign( Value );
      for i := RowCount - 1 downto RowCount - fFooterRows do
         InvalidateRow( i );
   end;
end;

procedure TARSortGrid.SetFooterBevelStyle( Value: TCellBevelStyle ); //RCV02
var
   i: Integer;
begin
   if fFooterBevelStyle <> Value then
   begin
      fFooterBevelStyle := Value;
      for i := RowCount - 1 downto RowCount - fFooterRows do
         InvalidateRow( i );
   end;
end;

function TARSortGrid.GetSelected( Row: LongInt ): Boolean; //RCV03
begin
   Result := fSelectedRows.IndexOf( Pointer( Row ) ) > -1 ;
end;

procedure TARSortGrid.SetSelected( Row: LongInt; Select: Boolean ); //RCV03
begin
   SelectRow( Row, Select );
end;

function TARSortGrid.GetSelectedCount: LongInt; //RCV03
begin
   Result := fSelectedRows.Count;
end;

function TARSortGrid.GetSelItems( Index: LongInt ): LongInt; //RCV03
begin
   Result := LongInt( fSelectedRows.Items[ Index - 1 ] );
end;

procedure TARSortGrid.SetWordWrap( const Value: Boolean );
begin
   if Value <> fWordWrap then
   begin
      fWordWrap := Value;
      Invalidate;
   end;
end;

function TARSortGrid.Search(StartRow: Cardinal): Boolean;
var
  iRow, iCol: Cardinal;
  SearchStr, CellStr: String;

  SearchResult: boolean;
begin
  Result := False;
  if self.CaseSensitive = False then
    SearchStr := LowerCase(SearchOptions.SearchText)
  else
    SearchStr := SearchOptions.SearchText
  ;

  if Assigned( SearchOptions.FilterFunction ) then // use the custom compare function
    begin
      for iRow := StartRow to RowCount-1 do
        begin
          if SearchOptions.FilterFunction( self, iRow ) then // compareFunction returns true if the contents of row iRow match the function's criteria.
            begin
              if SearchOptions.Filtered = True then
                begin
                  MoveRow(iRow, 1);
                  Inc(FilterCount);
                  Result := True;
                end
              else
                begin
                  MoveTo(SearchOptions.SearchCol, iRow);
                  Result := True;
                  Break;
                end
              ;
            end
          ;
        end
      ;
    end
  else if SearchOptions.SearchCol > -1 then // Only search one col
    begin
      for iRow := StartRow to RowCount-1 do
        begin
          CellStr := Cells[SearchOptions.SearchCol, iRow];
          if CaseSensitive = False then
            CellStr := LowerCase(CellStr)
          ;

          if( SearchOptions.PartialMatch ) then
            SearchResult := ( Pos(SearchStr, CellStr) > 0 )
          else
            SearchResult := ( cellStr = SearchStr )
          ;

          if( SearchResult ) then
            begin
              if SearchOptions.Filtered = True then
                begin
                  MoveRow(iRow, 1);
                  Inc(FilterCount);
                  Result := True;
                end
              else
                begin
                  MoveTo(SearchOptions.SearchCol, iRow);
                  Result := True;
                  Break;
                end
              ;
            end
          ;
        end
      ;
    end
  else //Search all Cells
    begin
      for iRow := StartRow to RowCount-1 do
        begin
          if (Result = True) and (SearchOptions.Filtered = False) then
            Break
          ;
          for iCol := 0 to ColCount-1 do
            begin
              CellStr := Cells[iCol, iRow];
                if CaseSensitive = False then
                  CellStr := LowerCase(CellStr);

                if( SearchOptions.PartialMatch ) then
                  SearchResult := ( Pos(SearchStr, CellStr) > 0 )
                else
                  SearchResult := ( cellStr = SearchStr )
                ;

                if( SearchResult ) then
                begin
                  if SearchOptions.Filtered = True then
                    begin
                      MoveRow(iRow, 1);
                      Inc(FilterCount);
                      Result := True;
                      Break;
                    end
                  else
                    begin
                      MoveTo(iCol, iRow);
                      Result := True;
                      Break;
                    end
                  ;
                end
              ;
            end
          ;
        end
      ;
    end
  ;

  if SearchOptions.Filtered = True then
    begin
      MoveTo(0,1);
      //fUpdateOnSizeChange := True;
      if FilterCount = 0 then
        begin //Because FixedRowCount < RowCount
          FilterCount := (-1*RowCount)+1;
          SwapRows(1, RowCount);
          RowCount := 2;
        end
      else if FilterCount = 1 then
        begin
          FilterCount := RowCount-2;
          RowCount := 2;
        end
      else if FilterCount > 1 then
        begin
          FilterCount := RowCount-FilterCount-1;
          RowCount := RowCount-FilterCount;
        end
      ;
    end
  ;
end;


{******************************************************************************}
{** Private Members for TPrintOptions                                        **}
{******************************************************************************}
procedure TPrintOptions.SetDateFormat( Const Value: String );
begin
   fDateFormat := Value;
end;

procedure TPrintOptions.SetFooterSize( Const Value: Cardinal );
begin
   fFooterSize := Value;
end;

procedure TPrintOptions.SetHeaderSize( Const Value: Cardinal );
begin
   fHeaderSize := Value;
end;

procedure TPrintOptions.SetLogo( Const Value: String );
begin
   fLogo := Value;
end;

procedure TPrintOptions.SetMarginBottom( Const Value: Cardinal );
begin
   fMarginBottom := Value;
end;

procedure TPrintOptions.SetMarginLeft( Const Value: Cardinal );
begin
   fMarginLeft := Value;
end;

procedure TPrintOptions.SetMarginRight( Const Value: Cardinal );
begin
   fMarginRight := Value;
end;

procedure TPrintOptions.SetMarginTop( Const Value: Cardinal );
begin
   fMarginTop := Value;
end;

procedure TPrintOptions.SetOrientation( Const Value: TPrinterOrientation );
begin
   fOrientation := Value;
end;

procedure TPrintOptions.SetPageFooter( Const Value: String );
begin
   fPageFooter := Value;
end;

procedure TPrintOptions.SetTimeFormat( Const Value: String );
begin
   fTimeFormat := Value;
end;

{******************************************************************************}
{** Private Members for TARSortGrid                                            **}
{******************************************************************************}
procedure TARSortGrid.ToggleRow( aRow: LongInt ); //RCV03
var
  aCol:  Longint;
  Index: Integer;
begin
  Index := fSelectedRows.IndexOf( Pointer( aRow ) );
  if Index <> -1 then
     fSelectedRows.Delete( Index )
  else
     fSelectedRows.Add( Pointer( aRow ) );
  for aCol := FixedCols to Pred( ColCount ) do
	InvalidateCell( aCol, aRow );
end;

procedure TARSortGrid.InvalidateRow( aRow: LongInt ); //RCV03
var
  aCol: LongInt;
begin
  for aCol := FixedCols to Pred( ColCount ) do
    InvalidateCell( aCol, aRow );
end;

procedure TARSortGrid.SelectRow( aRow: LongInt; Select: Boolean ); //RCV03
var
  ListIndex: Integer;
begin
  ListIndex := fSelectedRows.IndexOf( Pointer( aRow ) );
  if ( ListIndex = -1 ) and (Select) then
     begin
        fSelectedRows.Add( Pointer( aRow ) );
        InvalidateRow( aRow );
     end
  else if ( ListIndex <> -1 ) and (NOT Select) then
     begin
		fSelectedRows.Delete( ListIndex );
        InvalidateRow( aRow );
     end;
end;

procedure TARSortGrid.SelectRows( aRow, bRow: LongInt; Select: Boolean ); //RCV03
var
  Index, StartRow, EndRow: LongInt;
begin
  if aRow > bRow then
     begin
        StartRow := bRow;
        EndRow   := aRow;
     end
  else
     begin
        StartRow := aRow;
        EndRow  :=  bRow;
     end;
  for Index := StartRow to EndRow do
     SelectRow( Index, Select );
end;


{******************************************************************************}
{** Protected Members for TARSortGrid                                          **}
{******************************************************************************}
procedure TARSortGrid.ListQuickSort( const aCol: LongInt; const SortOptions: TSortOptions );
var
   i:           Integer;
   Item:       pSortedListEntry;
   BufferGrid:  TStringGrid;
begin
   //Let everyone know we're sorting.
   fSorting := True;
   try
      //Get rid of any old entries in the sorted list.
      fSortedList.Reset;

      //Set the sort options for the list.
      //"Compare" can only look at GlobalSortOptions.
      GlobalSortOptions := SortOptions;
      SetSortDirection( SortOptions.SortDirection ); // Line modified/added by gnes@zero.it
      fSortColumn := aCol;

      //Insert the Row Number and Key (Str) into
	  for i := FixedRows to RowCount - 1 - gFooterSub do
      begin
          New( Item );
          Item^.RowNum := i;
          Item^.Str    := Cells[aCol, i];
          fSortedList.Add( Item );
      end;

      //Quick Sort the list by key string.
      //Then the row numbers will indicate where
      //each row should be placed.
      //E.g. If list item 0 contains a RowNum of 4 then
      //row 4 should be the first row (position 0).
      fSortedList.Sort( Compare );

      BufferGrid := nil;
      try
         //Now rearrange the rows of the grid in sorted order.
         //This is a fast but space inefficient way to do it.
         //First, create a buffer grid and size it correctly.
         BufferGrid := TStringGrid.Create( Self );

         BufferGrid.ColCount := ColCount;
		 BufferGrid.RowCount := RowCount;
         //Copy the rows to the buffer grid in sorted order.
         for i := 0 to fSortedList.Count - 1 do
         begin
             Item := fSortedList.GetItem( i );
             BufferGrid.Rows[i + FixedRows].Assign( Rows[Item^.RowNum] );
         end;
         //Now put the rows back into the original grid.
         for i := FixedRows to RowCount - 1 - gFooterSub do
             Rows[i].Assign( BufferGrid.Rows[i] );
      finally
         BufferGrid.Free;
      end;

      //Now put the selection back on the right row.
      for i := 0 to fSortedList.Count - 1 do
      begin
         Item := fSortedList.GetItem( i );
         if Item^.RowNum = Row then
         begin
            MoveTo( Col, i + FixedRows );
            Break;
         end;
	  end;
   finally
      //Make sure we get this turned off.
      fSorting   := False;
      fSortState := ssSorted;   // Now the grid is sorted // Line modified/added by gnes@zero.it
   end;
end;

//This function tries to determine the best sort style
//for a column.  If all the entries can be converted to
//numbers, a numeric sort is returned.  If they can all
//be converted to dates, a date sort is returned.  If time,
//then a time sort is returned,
//Otherwise, an alphabetic sort is returned.
function TARSortGrid.DetermineSortStyle( const aCol: LongInt ): TSortStyle;
var
   i: Integer;
   DoNumeric, DoDateTime, DoTime: Boolean;
   F: extended;
   dt:TDateTime;
   s: string;
begin
   DoNumeric  := True;
   DoDateTime := True;
   DoTime     := True;

   //Note: We only go through the rows once.
   //This code depends on the fact that no
   //entry can be both a date and number.
   for i := FixedRows to RowCount - 1 - gFooterSub do
   begin
      if NOT DoNumeric and NOT doDateTime and NOT doTime then
		 Break; //speed things up a little.
	  s := Cells[aCol, i];
	  if DoNumeric then
		DoNumeric := TryStrToFloat(s ,f);
 {	  begin
		 try
// IF YOU GET AN EXCEPTION HERE WHEN RUNNING FROM THE IDE,
// THEN YOU NEED TO TURN OFF "Break on Exception"
			StrToFloat( Cells[aCol, i] );
         except
            on EConvertError do
               DoNumeric := False;
         end;
	  end;  }

	  if DoTime then
		DoTime := TryStrToTime( s , dt );
{      begin
         try
// IF YOU GET AN EXCEPTION HERE WHEN RUNNING FROM THE IDE,
// THEN YOU NEED TO TURN OFF "Break on Exception"
			StrToTime( Cells[aCol, i] );
		 except
            on EConvertError do
               DoTime := False;
         end;
	  end; }

	  if DoDateTime then
		DoDateTime := TryStrToDateTime( s , dt );
{      begin
         try
// IF YOU GET AN EXCEPTION HERE WHEN RUNNING FROM THE IDE,
// THEN YOU NEED TO TURN OFF "Break on Exception"
			StrToDateTime( Cells[aCol, i] );
		 except
			on EConvertError do
			   DoDateTime := False;
         end;
	  end;      }
   end;

   if DoNumeric then
      Result := ssNumeric
   else if DoDateTime then
      Result := ssDateTime
   else if DoTime then
      Result := ssTime
   else
      Result := ssAlphabetic;
end;

procedure TARSortGrid.InitializeFormatOptions( const aCol, aRow: LongInt; var FmtOpts: TFormatOptions );
begin
   // Setup good defaults for FormatOptions.
   if aRow > RowCount - 1 - fFooterRows then //RCV02
   begin
      Canvas.Font         := fFooterFont;
      Canvas.Brush.Color  := fFooterColor;
      FmtOpts.Bevel.Style := fFooterBevelStyle;
   end else
      FmtOpts.Bevel.Style := fBevelStyle;

   FmtOpts.HideText      := False;
   FmtOpts.Font          := Canvas.Font;
   FmtOpts.Brush         := Canvas.Brush;
   FmtOpts.AlignmentHorz := AlignmentHorz;
   FmtOpts.AlignmentVert := AlignmentVert;

   // Set defaults for the bevel colors.
   case BevelStyle of
      cbRaised:
      begin
         FmtOpts.Bevel.UpperLeftColor  := clBtnHighlight;
         FmtOpts.Bevel.LowerRightColor := clBtnShadow;
      end;
      cbLowered:
      begin
         FmtOpts.Bevel.UpperLeftColor  := clBtnShadow;
         FmtOpts.Bevel.LowerRightColor := clBtnHighlight;
      end;
      else
         FmtOpts.Bevel.UpperLeftColor  := clWindow;
         FmtOpts.Bevel.LowerRightColor := clWindow;
   end;
end;

procedure TARSortGrid.DrawCell( aCol, aRow: LongInt; aRect: TRect; aState: TGridDrawState );
var
   xOffset, yOffset: Integer;
   FmtOpts:  TFormatOptions;
   NewState: TGridDrawState; //RCV03
begin
   InitializeFormatOptions( aCol, aRow, FmtOpts );

   NewState := aState; //RCV03
   if (fSelectedRows.IndexOf( Pointer( aRow ) ) > -1) then
      Include( NewState, gdSelected )
   else
      Exclude( NewState, gdSelected );
   if not (gdFixed in NewState) and (aRow <= RowCount - 1 - fFooterRows) and (goRangeSelect in Options) then
   begin
      if (gdSelected in NewState) then
      begin
         FmtOpts.Brush.Color := clHighlight;
         FmtOpts.Font.Color  := clHighlightText;
      end else
      begin
         FmtOpts.Brush.Color := clWindow;
         FmtOpts.Font.Color  := clWindowText;
      end;
   end;

   // Now do the OnGetCellFormat event if necessary.
   if Assigned( fOnGetCellFormat ) then
      fOnGetCellFormat( Self, aCol, aRow, aState, FmtOpts );

   if DefaultDrawing then
   begin
      // Calculate horizontal offset.
      case FmtOpts.AlignmentHorz of
         taRightJustify:
            xOffset := aRect.Right - aRect.Left - Canvas.TextWidth( Cells[aCol, aRow] )- 2;
         taCenter:
            xOffset := (aRect.Right - aRect.Left - Canvas.TextWidth( Cells[aCol, aRow] )) div 2;
         else
            xOffset := 2;
      end;

      // Calculate vertical offset.
      case FmtOpts.AlignmentVert of
          taBottomJustify:
             yOffset := aRect.Bottom - aRect.Top - Canvas.TextHeight( Cells[aCol, aRow] )- 3;
          taMiddle:
             yOffset := (aRect.Bottom - aRect.Top - Canvas.TextHeight( Cells[aCol, aRow] )) div 2;
          else
             yOffset := 2;
      end;

      // Leave room for a sort marker, if necessary.
      // FIX ME: if someone wants to use sgGlyph, then spacing needs to be rearranged.
      // FIX ME: a user might want to select whether symbol is shown before or after column label.
      if (SortSymbol <> sgNone) and (aRow = 0) then
      begin
          Inc(xOffset,GSortBM.Width + SortSpacingHor); //width of up/down bitmap and spacing
          if( aCol = 0 ) then inc( xOffset, 2 ); // I don't know why.  It just works better.
      end;

      // Now do the text drawing.
      if NOT FmtOpts.HideText then
         Canvas.TextRect( aRect, aRect.Left + xOffset, aRect.Top + yOffset, Cells[aCol, aRow] )
      else
         Canvas.TextRect( aRect, aRect.Left + xOffset, aRect.Top + yOffset, '' );

      // Draw a sort marker.
      // FIX ME: if someone wants to use sgGlyph, then spacing needs to be rearranged.
      // FIX ME: a user might want to select whether symbol is shown before or after column label.
      if (SortSymbol <> sgNone) and (aRow = 0) and (aCol = SortColumn) and (FixedRows > 0) then
      begin
         //w := Canvas.TextWidth( Cells[aCol, aRow] ); // Value is never used.
         yOffset := (RowHeights[0] div 2) - 4;
         DrawSortSymbol( aRect.Left + SortSpacingHor, aRect.Top + yOffset + SortSpacingVert );
      end;

      // Draw Bevel.
      if (FmtOpts.Bevel.Style <> cbNone) and (aCol >= FixedCols) and (aRow >= FixedRows) then
      begin
         //Draw from bottom-most lines out to mimic behaviour of
         //fixed cells when FixedXXXXLine is toggled.
         with ARect do
            begin
               if goFixedVertLine in Options then
               begin
                  Canvas.Pen.Color := FmtOpts.Bevel.LowerRightColor;
                  Canvas.PolyLine( [Point( Right - 1, Top ), Point( Right - 1, Bottom )] );
               end;
               if goFixedHorzLine in Options then
               begin
                  Canvas.Pen.Color := FmtOpts.Bevel.LowerRightColor;
                  Canvas.PolyLine( [Point(Left, Bottom - 1), Point(Right, Bottom - 1)] );
               end;
               if goFixedVertLine in Options then
               begin
                  Canvas.Pen.Color := FmtOpts.Bevel.UpperLeftColor;
                  Canvas.PolyLine( [Point( Left, Top ), Point( Left, Bottom )] );
               end;
               if goFixedHorzLine in Options then
               begin
                  Canvas.Pen.Color := FmtOpts.Bevel.UpperLeftColor;
                  Canvas.PolyLine( [Point( Left, Top ), Point( Right, Top )] );
            end;
         end;
      end;
      if Assigned( OnDrawCell ) then
         OnDrawCell( Self, aCol, aRow, aRect, NewState );
   end else
      inherited DrawCell( aCol, aRow, aRect, NewState );
end;

procedure TARSortGrid.DrawSortSymbol( x, y: Integer );
var
   MyCol: TColor;
begin
	if( ( Assigned( GSortBM ) ) and ( ssSorted = self.SortState ) ) then
   begin
      if SortSymbol = sgCustom then
         MyCol := GSortBM.Canvas.Pixels[0, 0]
      else
         MyCol := clAqua;
		Canvas.BrushCopy( Rect(x, y, x + GSortBM.Width, y + GSortBM.Height ), GSortBM, Rect( 0, 0, GSortBM.Width, GSortBM.Height ), MyCol );
	end;

  // FIX ME: else???
end;


procedure TARSortGrid.SetGSortSymbol;
var
   pos: Integer;
   RcStr: String;
begin
	GSortBM.Free;								// Delete the old bitmap if present.
	GSortBM := nil;
	GSortBMWidth  := 0;
   GSortBMHeight := 0;
	if SortSymbol <> sgNone then           // Do we use a bitmap?
   begin
		GSortBM := TBitmap.Create;          // Yes, create a new bitmap.
		if SortSymbol = sgCustom then
      begin
			// Copy the custom Bitmap to the Sort Bitmap.
         if SortDirection = sdAscending then
     	      GSortBM.Assign( FSortBMA )
         else
            GSortBM.Assign( FSortBMD );
      end
    else if SortSymbol = sgBendsenArrow then  // Use (more attractive) bitmaps from Bendsen's resource file
      begin
         if SortDirection = sdAscending then
           GSortBM.LoadFromResourceName(HInstance,'UP')
         else
           GSortBM.LoadFromResourceName(HInstance,'DOWN');
      end
    else // It's an internal bitmap symbol...
      begin
         pos := 0;
         if SortSymbol <> sgArrow then
            pos := 2;
         if SortDirection <> sdAscending then
            Inc( pos );
			RcStr := Copy( 'SORTUPSORTDNSORTAZSORTZA', pos * 6 + 1, 6 );
			// Load it from the resource.
			GSortBM.Handle := LoadBitmap( HInstance, pChar( RcStr ) );
     end;
     GSortBMWidth  := GSortBM.Width;
     GSortBMHeight := GSortBM.Height;
	end;
end;

procedure TARSortGrid.clickSort( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  MouseUp( button, shift, x, y );
end;

procedure TARSortGrid.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
   Point:         TPoint;
   aCol, aRow:    LongInt;
   SortOptions:   TSortOptions;
   CurrentCursor: TCursor;
begin
  (*
   dbcout( 'CLICK SORT!' );

   dbcout( boolToText( focused ) );
   dbcout( boolToText( SortOnClick ) );
   dbcout( boolToText( (fGridState = gsNormal) ) );
   dbcout( boolToText( (FixedRows >= 1) ) );
   dbcout( boolToText( (Shift = []) ) );
   *)
   
   //Make sure we're not sizing and have a header row.
   if Focused and SortOnClick and (fGridState = gsNormal) and (FixedRows >= 1) and (Shift = []) then
   begin
      Point.x := X;
      Point.y := Y;
      MouseToCell( Point.x, Point.y, aCol, aRow );
      
      //Make sure they clicked a fixed row.
      if (aRow >= 0) and (aRow <= (FixedRows - 1)) then
      begin
         SortOptions.SortStyle := ssAutomatic;

         // AR: I don't like right clicking to change sort direction
         (*
         if Button = mbRight then
            SortOptions.SortDirection := sdDescending
         else
            SortOptions.SortDirection := sdAscending;
         *)

         // AR: This is better....
         if( ssSorted = SortState ) then
          begin
             if( aCol = fSortColumn ) then
              begin
                 if( self.SortDirection = sdDescending ) then
                  SortOptions.SortDirection := sdAscending
                 else
                  SortOptions.SortDirection := sdDescending
                 ;
              end
             else
              SortOptions.SortDirection := {self.SortDirection //}sdAscending
             ;
          end
         else
          SortOptions.SortDirection := self.SortDirection
         ;

         { EWE: Set case sensitivity here }
         SortOptions.SortCaseSensitive := fCaseSensitive;

         SortOptions.SortCompare := nil;
         if Assigned( fOnClickSort ) then
            fOnClickSort( Self, aCol, aRow, SortOptions );
            CurrentCursor := Screen.Cursor;
         try
            Screen.Cursor := crHourglass;
            SortByColumn( aCol, SortOptions );
         finally
            Screen.Cursor := CurrentCursor;
         end;
      end;
   end;

   inherited MouseUp( Button, Shift, X, Y );
   fMouseIsDown := False;
end;

procedure TARSortGrid.ColWidthsChanged;
begin
   inherited ColWidthsChanged;
   if Assigned( fOnColumnWidthsChanged ) then
      fOnColumnWidthsChanged( Self );
   UpdateScrollPage;
end;

procedure TARSortGrid.RowHeightsChanged;
begin
   inherited RowHeightsChanged;
   if Assigned( fOnRowHeightsChanged ) then
      fOnRowHeightsChanged( Self );
   UpdateScrollPage;
end;

procedure TARSortGrid.SizeChanged( OldColCount, OldRowCount: LongInt );
begin
   inherited SizeChanged( OldColCount, OldRowCount );
   if Assigned( fOnSizeChanged ) then
      fOnSizeChanged( Self, OldColCount, OldRowCount );
   UpdateScrollPage;
end;

procedure TARSortGrid.UpdateScrollPage;
   function LMax( const A, B: LongInt ): LongInt;
   begin
      Result := B;
      if A > B then Result := A;
   end;
var
   SI: TScrollInfo;
begin
   {Make the scroll bar(s) proportional.}
   {To do this correctly, I should sum colwidths and rowheights,
   but I just approximate by basing the proportion on visible rows or cols
   divided by row or col count...}
   {Also, I can't really figure out Borland's scroll bar range and position
   scheme.  Thus, sometimes when you click on the end of the scroll bar, you
   still have to scroll farther with the arrows to actually get to the end
   of the grid.  If anyone knows how to fix this, PLEASE let me know...}
   if (ScrollBars = ssVertical) or (ScrollBars = ssBoth) then
   begin
      SI.cbSize := SizeOf( SI );
      SI.fMask:=SIF_PAGE or SIF_POS or SIF_RANGE;
      GetScrollInfo( Handle, SB_VERT, SI );
      SI.fMask := SIF_PAGE;
      //Issue 2546: The proportional scroll bar calculation causes this issue
      //if ProportionalScrollBars then
      //begin
         //SI.nPage := LMax(Round(((1.0*(VisibleRowCount+FixedRows))/RowCount)*(SI.nMax-SI.nMin+1)), 1)
      //end else
         SI.nPage := 0;
         SetScrollInfo( Handle, SB_VERT, SI, True );
   end;
   if (ScrollBars = ssHorizontal) or (ScrollBars = ssBoth) then
   begin
      SI.cbSize := SizeOf( SI );
      SI.fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
      GetScrollInfo( Handle, SB_HORZ, SI );
      SI.fMask := SIF_PAGE;
      if ProportionalScrollBars then
      begin
         SI.nPage := LMax(Round(((1.0*(VisibleColCount+FixedCols))/ColCount)*(SI.nMax-SI.nMin+1)), 1)
      end else
         SI.nPage := 0;
         SetScrollInfo( Handle, SB_HORZ, SI, True );
   end;
end;

procedure TARSortGrid.KeyDown( var Key: Word; Shift: TShiftState );
begin
   inherited KeyDown( Key, Shift );
   if ExtendedKeys and NOT EditorMode then
   begin
      if Shift = [ssCtrl] then
      begin
         case Key of
            VK_INSERT: InsertRow( Row );
            VK_DELETE: if RowCount > (FixedRows + 1) then
                          DeleteRow( Row );
         end;
      end
      else if Shift = [ssCtrl, ssShift] then
      begin
         case Key of
            VK_INSERT: InsertColumn( Col );
            VK_DELETE: if ColCount > (FixedCols + 1) then
                          DeleteColumn( Col );
         end;
      end;
   end;
end;

procedure TARSortGrid.SetEditText( aCol, aRow: LongInt; const Value: String );
begin
   try
      if Value <> Cells[aCol, aRow] then
         Modified := True;
   finally
      inherited SetEditText( aCol, aRow, Value );
   end;
end;

procedure TARSortGrid.Click;
begin
   try
      inherited Click;
   finally
      if fEntered then
         ValidateCell;
   end;
end;

procedure TARSortGrid.DoEnter;
begin
   try
      inherited DoEnter;
      fEntered := True;
   finally
      InitValidate;
   end;
end;

procedure TARSortGrid.DoExit;
begin
   try
      Click;
   finally
      inherited DoExit;
      fEntered := False;
   end;
end;

procedure TARSortGrid.InitValidate;
begin
   fOldCol := Col;
   fOldRow := Row;
   fOldCellText := Cells[ fOldCol, fOldRow ];
   fOldModifiedValue := Modified;
end;

procedure TARSortGrid.TopLeftChanged;
begin
   inherited TopLeftChanged;
   if fMouseIsDown then
   begin
      if TopRow > fLastTopRow then
      begin
         fLastMoveOn := TopRow + VisibleRowCount - 1;
         SelectRow( TopRow + VisibleRowCount - 1, True );
      end;
   end;
   fLastTopRow := TopRow;
end;

procedure TARSortGrid.MouseMove( Shift: TShiftState; X, Y: Integer ); //RCV03
var
  aCol, aRow: LongInt;
begin
  inherited MouseMove( Shift, X, Y );
  if fMouseIsDown then
    begin
      MouseToCell( X, Y, aCol, aRow );
      if ( aRow <> fLastMoveOn ) then
        begin
          if ( aRow >= fAnchor ) and ( aRow < fLastMoveOn ) then
            begin
              SelectRows( fLastMoveOn , aRow , False );
              if aRow = fAnchor then
                SelectRow( fAnchor , True );
            end
          else if ( aRow <= fAnchor ) and ( aRow > fLastMoveOn ) then
            begin
              SelectRows( fLastMoveOn, aRow , False );
              if aRow = fAnchor then
                SelectRow( fAnchor, True );
            end
          else if ( aRow < fAnchor ) and ( fLastMoveOn > fAnchor ) then
            begin
              SelectRows( fLastMoveOn, fAnchor + 1, False );
              SelectRows( fAnchor, aRow, True );
            end
          else if ( aRow > fAnchor ) and ( fLastMoveOn < fAnchor ) then
            begin
              SelectRows( fLastMoveOn, fAnchor - 1, False );
              SelectRows( fAnchor, aRow, True );
            end
          else
            SelectRows( aRow, fAnchor, True );
          fLastMoveOn := aRow;
        end;
    end;
end;

procedure TARSortGrid.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); //RCV03
var
   aRow, aCol: LongInt;
begin
   inherited MouseDown( Button, Shift, X, Y );

   if (Button = mbLeft) and (goRangeSelect in Options) then
   begin
      MouseToCell( X, Y, aCol, aRow );
      if aRow < FixedRows then
         Exit
      else
      begin
        fMouseIsDown := True;
        fLastMoveOn  := aRow;

        if ssCtrl in Shift then
          begin
             if aRow > -1 then
             begin
                ToggleRow( aRow );
                fAnchor := aRow;
             end;
          end
        else if ssShift in Shift then
        begin
           fSelectedRows.Clear;
           SelectRows( fAnchor, aRow, True );
        end
        else
          begin
             if fSelectedRows.Count > 0 then
                fSelectedRows.Clear;
             if aRow > -1 then
             begin
                fSelectedRows.Add( Pointer( aRow ) );
                fAnchor := aRow;
             end;
             Refresh;
          end
        ;
      end;
   end;
end;

{******************************************************************************}
{** Public Members for TPrintOptions                                         **}
{******************************************************************************}
constructor TPrintOptions.Create; //RCV04
begin
   inherited Create;
   PageFooter  := 'date|time|page';
   DateFormat  := 'd-mmm-yyyy';
   TimeFormat  := 'h:nn';
   fCopies      := 1;
   fLeftPadding := 2;
   fPreviewPage := 1;
end;

{******************************************************************************}
{** Public Members for TARSortGrid                                             **}
{******************************************************************************}
constructor TARSortGrid.Create( AOwner: TComponent );
var
  component: TComponent;
begin
   inherited Create( AOwner );

   OriginalWindowProc := self.WindowProc;
   self.WindowProc := newWindowProc;

   fMyForm                  := nil;
   fSortEnabled             := true;
   setIsReadOnly( true );
   fSortedList              := TSortedList.Create;
   fCaseSensitive           := False;  { dflt to no case sensitivity }
   fAlignmentHorz           := taLeftJustify;
   fAlignmentVert           := taTopJustify;
   fBevelStyle              := cbNone;
   fProportionalScrollBars  := True;
   fExtendedKeys            := False;
   fSorting                 := False;
   fModified                := False;
   fEntered                 := False;
   fSortState               := ssUnsorted;     // Line modified/added by gnes@zero.it
   fSortBMA                 := TBitmap.Create; //RCV02
   fSortBMD                 := TBitmap.Create; //RCV02
   GSortBM                  := nil; //RCV02
   fSortSymbol              := sgNone; //RCV02
   fSortSpacingHor          := 2; //RCV02
   fSortSpacingVert         := 0; //RCV02
   fSortColumn              := 1; //RCV02
   fSortDirection           := sdAscending; //RCV02
   fSortStyle               := ssAutomatic;
   fAutoSizeCols            := True;
   fSortOnClick             := True;  //RCV02
   fSortFooter              := False; //RCV02
   fFooterRows              := 0; //RCV02
   gFooterSub               := 0; //RCV02
   fFooterColor             := clAqua; //RCV02
   fFooterFont              := TFont.Create; //RCV02
   fFooterBevelStyle        := cbNone; //RCV02
   gASE                     := False; //RCV02
   fSelectedRows            := TList.Create; //RCV03
   fSelectedRows.Add( Pointer( FixedRows ) ); //RCV03
   fAnchor                  := FixedRows; //RCV03
   fMouseIsDown             := False; //RCV03
   fLastTopRow              := TopRow; //RCV03
   fOldRowCol               := TOldRowColValueList.Create; //RCV04
   {$IfDef VERD4+}
   fPrintOptions            := TPrintOptions.Create; //RCV04
   fPrintOptions.HeaderSize := Font.Size + 2; //RCV04
   fPrintOptions.FooterSize := fFooterFont.Size - 1; //RCV04
   {$EndIf}
   fOldFont                 := TFont.Create;  //RCV04
   fOldBrush                := TBrush.Create; //RCV04
   fOldFont.Assign( Canvas.Font );   //RCV04
   fOldBrush.Assign( Canvas.Brush ); //RCV04

   fSearchOptions := TSearchOptions.Create();
   fSearchOptions.Grid := Self;

   component := AOwner;
   while nil <> component do
   begin
    if( component is TForm ) then
      begin
        fMyForm := component as TForm;
        break;
      end
    else
      component := component.GetParentComponent
    ;
   end;

   InitValidate;
end;

destructor TARSortGrid.Destroy;
begin
   fOldBrush.Free; //RCV04
   fOldFont.Free;  //RCV04
   {$IfDef VERD4+}
   fPrintOptions.Free; //RCV04
   {$EndIf}
   fOldRowCol.Reset; //RCV04
   fOldRowCol.Free; //RCV04
   fSelectedRows.Free; //RCV03
   fFooterFont.Free; //RCV02
   fSortBMA.Free; //RCV02
   fSortBMD.Free; //RCV02
   gSortBM.Free;  //RCV02	Delete the old bitmap if present.
   fSortedList.Reset;
   fSortedList.Free;

   fSearchOptions.Free();

   inherited Destroy;
end;

procedure TARSortGrid.ValidateCell;
var
   Value: String;
   Valid: Boolean;
begin
   if fOldCellText <> Cells[fOldCol, fOldRow] then
   begin
      Value := Cells[fOldCol, fOldRow];
      Valid := True;
      if Assigned( fOnCellValidate ) then
         fOnCellValidate( Self, fOldCol, fOldRow, Value, Valid );
      //Since Value is also a VAR parameter, we always
      //use it if it was changed in OnCellValidate.
      if NOT Valid then
      begin
         if Value <> Cells[fOldCol, fOldRow] then
            Cells[fOldCol, fOldRow] := Value
         else
             Cells[fOldCol, fOldRow] := fOldCellText;
         Modified := fOldModifiedValue;
      end else
         if Value <> Cells[fOldCol, fOldRow] then
            Cells[fOldCol, fOldRow] := Value;
   end;
   InitValidate;
end;

//AutoSizes the aCol column.
procedure TARSortGrid.AutoSizeCol( const aCol: LongInt );
var
   MaxWidth, TextW, i: Integer;
   FmtOpts:            TFormatOptions;
begin
   //Resize the column to display the largest value.
   MaxWidth := 0;
   Canvas.Font := Font;
   for i := 0 to RowCount - 1 do
   begin
      InitializeFormatOptions( aCol, i, FmtOpts );
      if Assigned( fOnGetCellFormat ) then
         fOnGetCellFormat( Self, Col, i, GetCellDrawState( aCol, i ), FmtOpts );
      Canvas.Font := FmtOpts.Font;
      TextW := Canvas.TextWidth( Cells[aCol, i] );
      if TextW > MaxWidth then
         MaxWidth := TextW;
      if( 0 = i ) then
        begin
          if Assigned( GSortBM ) then
            if( maxWidth < maxWidth + GSortBM.Width + 10 ) then
              maxWidth := maxWidth + GSortBM.Width + 10;
        end
      ;
   end;
   ColWidths[aCol] := MaxWidth + Canvas.TextWidth( 'x' );
end;

//AutoSizes ALL the variable columns and optionally the fixed columns.
procedure TARSortGrid.AutoSizeColumns( const DoFixedCols: Boolean; const Padding: Integer );
var
   i: Integer;
begin
   if DoFixedCols then
      for i := 0 to FixedCols - 1 do
      begin
         AutoSizeCol(i);
         if Padding <> 0 then
            ColWidths[i] := ColWidths[i] + Padding;
      end;
   for i := FixedCols to ColCount - 1 do
   begin
      AutoSizeCol( i );
      if Padding <> 0 then
         ColWidths[i] := ColWidths[i] + Padding;
   end;
end;

procedure TARSortGrid.AutoSizeColumns();
begin
  autoSizeColumns( true, 2 );
end;


procedure TARSortGrid.SortByColumn( const aCol: longint );
var
  sortOptions: TSortOptions;
begin
  // Don't sort while in edit mode or if sort is disabled.
  if( not( EditorMode ) and SortEnabled ) then
    begin
      //dbcout( 'Sort will occur by column ' + intToStr( aCol ) );
      sortOptions.sortStyle := self.SortStyle;
      sortOptions.SortDirection := self.SortDirection;
      sortOptions.SortCaseSensitive := self.CaseSensitive;
      sortOptions.SortCompare := nil;

      sortByColumn( aCol, sortOptions );
    end
  ;
end;

//Sorts the variable rows using Column aCol as a key
procedure TARSortGrid.SortByColumn( const aCol: LongInt; SortOptions: TSortOptions );
begin
   (*
   dbcout( 'EditorMode: ' + boolToText( EditorMode ) );
   dbcout( 'SortEnabled: ' + boolToText( SortEnabled ) );
   *)

   //Don't sort while in edit mode or if sort is disabled.
   if( not( EditorMode ) and SortEnabled ) then
   begin
      setSortColumn( aCol );

      //Call the OnBeginSort event.
      if Assigned( fOnBeginSort ) then
         fOnBeginSort( Self, aCol, SortOptions );

      //If there's only one row we don't need to do anything.
      if RowCount > (FixedRows + 1) then
      begin
         //Now we do the Automatic sorting determination.
         if SortOptions.SortStyle = ssAutomatic then
            SortOptions.SortStyle := DetermineSortStyle( aCol );

         //Quick Sort column ACol.
         ListQuickSort( aCol, SortOptions );

         // Clear the selection
         fSelectedRows.Clear;
         refresh();

         // Reset the find options
         GStartRow := 0;
         GStartCol := 0;
      end;

      //Call the OnEndSort event.
      if Assigned( fOnEndSort ) then
         fOnEndSort( Self, aCol );
   end;
end;

procedure TARSortGrid.InsertRow( aRow: LongInt );
begin
   RowCount := RowCount + 1;
   fOldRowCol.MoveUp( aRow, 1 );  // RCV04
   MoveRow( RowCount - 1, aRow );
   Rows[aRow].Clear;
   Row := aRow;
   fSortState := ssUnsorted; // Line modified/added by gnes@zero.it
   //setSortColumn( -1 ); // Unsorted!
   if Assigned( fOnRowInsert ) then
      fOnRowInsert( Self, aRow );
end;


function TARSortGrid.AppendRow(): longint;
begin
  result := rowCount;
  RowCount := RowCount + 1;
end;


function TARSortGrid.InsertRows( aRow, rCount: Integer ): Boolean; //RCV02
var
   i: Integer;
begin
	if CheckRange( aRow, aRow, True ) then
   begin
      for i := 0 to rCount - 1 do
      begin
		   RowCount := RowCount + 1;
         fOldRowCol.MoveUp( aRow + i, 1 );  // RCV04
         MoveRow( RowCount - 1, aRow );
		   Rows[aRow].Clear();
		   Row := aRow;
		   if Assigned( OnRowInsert ) then
            OnRowInsert( Self, aRow );
	   end;
      Result := True;
   end else
      Result := False;
end;

procedure TARSortGrid.InsertColumn( aCol: LongInt );
begin
   ColCount := ColCount + 1;
   fOldRowCol.MoveUp( aCol, 2 );  // RCV04
   MoveColumn( ColCount - 1, aCol );
   Cols[aCol].Clear;
   Col := aCol;
   if Assigned( fOnColumnInsert ) then
      fOnColumnInsert( Self, aCol );
end;

function TARSortGrid.InsertCols( aCol, cCount: Integer ): Boolean; //RCV02
var
   i: Integer;
begin
	if CheckRange( aCol, aCol, False ) then
   begin
      for i := 0 to cCount - 1 do
      begin
         ColCount := ColCount + 1;
         fOldRowCol.MoveUp( aCol + i, 2 );  // RCV04
		   MoveColumn( ColCount - 1, aCol );
		   Cols[aCol].Clear();
		   Col := aCol;
		   if Assigned( OnColumnInsert ) then
            OnColumnInsert( Self, aCol );
	   end;
   	Result := True;
   end else
      Result := False;
end;

function TARSortGrid.CheckRange( startcr, endcr: Integer; IsRow: Boolean): Boolean; //RCV02
var
   aCount: Integer;
begin
   if IsRow = True then
      aCount := RowCount
   else
      aCount := ColCount;

	if (startcr >= 0) and (startcr < aCount) and (endcr >= startcr) and (endcr < aCount) then
      Result := True
   else
	   Result := False;
end;

//Clears the grid.
function TARSortGrid.Clear: Boolean;
var
   filterWasUsed: boolean;
begin
  // Don't forget to clear the rows below the filter!
  filterWasUsed := searchOptions.Filtered;
  if( filterWasUsed ) then
    begin
      if FilterCount < 0 then
        begin
          RowCount := RowCount+(-1*FilterCount);
          //DeleteRow(1);
        end
      else
        begin
          RowCount := RowCount+FilterCount;
          FilterCount := 0;
        end
      ;
    end
  ;
  Result := ClearRows( 0, RowCount - 1 );
  if( filterWasUsed ) then searchOptions.Filtered := true;
end;

function TARSortGrid.ClearFrom( FromRow: LongInt ): Boolean; //RCV02
begin
   Result := ClearRows( FromRow, RowCount - 1 );
end;

function  TARSortGrid.ClearRow( aRow: Integer ): Boolean;
begin
   Result := ClearRows( aRow, aRow );
end;

function TARSortGrid.ClearRows( sRow, eRow: Integer ): Boolean; //RCV02
var
   i: Integer;
begin
   if CheckRange( sRow, eRow, True ) then
   begin
      for i := sRow to eRow do
         Rows[i].Clear();
	   Result := True;
      Modified       := False;
      fSortDirection := sdAscending; // Line modified/added by gnes@zero.it
      fSortState     := ssUnsorted;  // Line modified/added by gnes@zero.it
   end else
      Result := False;
end;

function TARSortGrid.ClearCols( sCol, eCol: Integer ): Boolean; //RCV02
var
   i: Integer;
begin
	if CheckRange( sCol, eCol, False ) then
   begin
      i := sCol;
      while i <= eCol do
      begin
         Cols[i].Clear();
         Inc( i, 2 );
      end;
	   Result := True;
   end else
      Result := False;
end;

{ If goAlwaysShowEditor is enabled then RemoveRow
	 and MoveRow leave the caret past the last row or
	 in one of the fixed rows.  So its turned off before
	 the delete and then back on afterward.				 }
procedure TARSortGrid.SetResetASE( SetASE: Boolean );
begin
	if SetASE then
   begin
		if goAlwaysShowEditor in Options then
      begin
			Options := Options - [goAlwaysShowEditor];
			GASE := True;
		end;
	end else
   begin
      if GASE then
      begin
	      Options := Options + [goAlwaysShowEditor];
		   GASE := False;
	   end;
   end;
end;

procedure TARSortGrid.DeleteRow( aRow: LongInt );
var
   i: LongInt;
begin
   if Selected[ aRow ] then
   begin
      SelectRow( aRow, False );

      for i := 0 to SelectedCount - 1 do
        begin
         if LongInt( fSelectedRows.Items[ i ] ) > aRow then
            fSelectedRows.Items[ i ] := Pointer( LongInt( fSelectedRows.Items[ i ] ) - 1 );
        end
      ;
   end;

   Rows[ aRow ].Clear;
   {If goAlwaysShowEditor is enabled then DeleteRow
    and MoveRow leave the caret past the last row or
    in one of the fixed rows.  So I turn it off before
    the delete and then back on after to get it
    working correctly.}
  	SetResetASE( True );
   fOldRowCol.RemRC( aRow, 1 );  // RCV04
   fOldRowCol.MoveDown( aRow, 1 );  // RCV04

   if( FixedRows + 1 < RowCount  ) then
      inherited DeleteRow( aRow )
   else
    begin
      if( self.SearchOptions.Filtered ) then FilterCount := -1 * FilterCount;
    end
   ;

  	SetResetASE( False );
   if Assigned( fOnRowDelete ) then
      fOnRowDelete( Self, aRow );
   if fFooterRows > RowCount - FixedRows then //RCV02
      FooterRows := RowCount - FixedRows;

   // If filtering, everything hidden by the filter also needs to be moved up.
   if( self.SearchOptions.Filtered ) then
   begin
     for i := 1 to FilterCount do
        MoveRow(self.RowCount + i, self.RowCount + i - 1);
   end;
end;

function TARSortGrid.DeleteRows( sRow, eRow: integer ): Boolean;
begin
  result := removeRows( sRow, eRow );
end;

function TARSortGrid.RemoveRows( sRow, eRow: Integer ): Boolean; //RCV02
var
   r: Integer;
begin
	if CheckRange( sRow, eRow, True ) then
   begin
      for r := eRow downto sRow do
      begin
         //dbcout( 'Removing row ' + intToStr( r ) );
         Rows[r].Clear();
    	  	SetResetASE( True );

          DeleteRow( r );

    		SetResetASE( False );
      end;
      Result := True;
   end else
      Result := False;
end;

procedure TARSortGrid.DeleteColumn( aCol: LongInt );
begin
   Cols[ aCol ].Clear;
   //See DeleteRow for comments...
  	SetResetASE( True );
   fOldRowCol.RemRC( aCol, 2 );  // RCV04
   fOldRowCol.MoveDown( aCol, 2 );  // RCV04
   inherited DeleteColumn( aCol );
  	SetResetASE( False );
   if Assigned( fOnColumnDelete ) then
      fOnColumnDelete( Self, aCol );
end;

function TARSortGrid.RemoveCols( sCol, eCol: Integer ): Boolean; //RCV02
var
   c: Integer;
begin
	if CheckRange( sCol, eCol, False ) then
   begin
      for c := eCol downto sCol do
      begin
		   Cols[ c ].Clear();
		   SetResetASE( True );
		   DeleteColumn( c );
		   SetResetASE( False );
      end;
	   Result := True;
   end else
      Result := False;
end;

procedure TARSortGrid.MoveRow( FromIndex, ToIndex: LongInt );
begin
   //See DeleteRow for comments...
   SetResetASE( True );
   inherited MoveRow( FromIndex, ToIndex );
   SetResetASE( False );
   fSortState := ssUnsorted; // Line modified/added by gnes@zero.it
   //setSortColumn( -1 ); // Unsorted!
end;

procedure TARSortGrid.MoveColumn( FromIndex, ToIndex: LongInt );
begin
   //See DeleteRow for comments...
   SetResetASE( True );
   inherited MoveColumn( FromIndex, ToIndex );
   SetResetASE( False );
end;

//The logic gets around a weird case where you swap with the last row.
procedure TARSortGrid.SwapRows( aRow1, aRow2: LongInt );
begin
   if aRow1 < aRow2 then
   begin
      MoveRow( aRow2, aRow1 );
      MoveRow( aRow1 + 1, aRow2 );
   end
   else if aRow2 < aRow1 then
   begin
      MoveRow( aRow1, aRow2 );
      MoveRow( aRow2 + 1, aRow1 );
   end;

   InvalidateRow( aRow1 );
   InvalidateRow( aRow2 );
end;

//The logic gets around a weird case where you swap with the last column.
procedure TARSortGrid.SwapColumns( aCol1, aCol2: LongInt );
begin
   if aCol1 < aCol2 then
   begin
      MoveColumn( aCol2, aCol1 );
      MoveColumn( aCol1 + 1, aCol2 );
   end
   else if aCol2 < aCol1 then
   begin
      MoveColumn( aCol1, ACol2 );
      MoveColumn( aCol2 + 1, ACol1 );
   end;
end;

//Moves the selected cell to (aCol, aRow) and makes it visible.
procedure TARSortGrid.MoveTo( aCol, aRow: LongInt );
begin
   if aCol < FixedCols then aCol := FixedCols;
   if aRow < FixedRows then aRow := FixedRows;
   if SelectCell( aCol, aRow ) then
   begin
      Col := aCol;
      Row := aRow;
      MoveColRow( aCol, aRow, True, True );
   end;
end;

//Finds a string in the grid.
//It searches by column and returns the first instance it finds.
function TARSortGrid.IsCell( const Value: String; var aCol, aRow: LongInt ): Boolean;
var
   i, Place: LongInt;
begin
   Result := False;
   for i := 0 to ColCount - 1 do
   begin
      Place := Cols[i].IndexOf( Value );
      if Place >= 0 then
      begin
         aRow   := Place;
         aCol   := i;
         Result := True;
         Break;
      end;
   end;
end;

procedure TARSortGrid.LoadFromFile( const FileName: String; const Delimiter: Char; sortWhenLoaded: boolean = false );
var
   r:             LongInt;
   Lines, Fields: TStringList;
begin
   Lines  := TStringList.Create;
   Fields := TStringList.Create;
   try
      Clear;

      Lines.LoadFromFile( FileName );
      RowCount := Lines.Count;
      ColCount := FixedCols + 1;
      for r := 0 to Lines.Count - 1 do
      begin
         TokenizeGridString( Lines[r], Delimiter, Fields );
         if Fields.Count > ColCount then
            ColCount := Fields.Count;
         Rows[r].Assign( Fields );
      end;

      if( autoSizeCols ) then autoSizeColumns( true, 0 );

      if( sortWhenLoaded ) then
        begin
          //Sort properly
          SortDirection := sdAscending;
          SortByColumn( 0 );
        end
      ;

      // Clear the selection
      fSelectedRows.Clear;
      refresh();

      // Reset the find options
      GStartRow := 0;
      GStartCol := 0;

   finally
      Fields.Free;
      Lines.Free;
   end;
end;

function TARSortGrid.delimitedText( const Delimiter: Char ): string;
var
   r, c:   LongInt;
   BufStr: String;
   Lines:  TStringList;
   s2: string;
begin
   Lines := TStringList.Create;
   try
      Lines.Clear;
      for r := 0 to RowCount - 1 do
      begin
         BufStr := '';
         for c := 0 to ColCount - 1 do
         begin
            s2 := Cells[c, r];
            if( not( isNan( usStrToFloat( s2, NaN ) ) ) ) then
              s2 := ansiReplaceStr( s2, SysUtils.DecimalSeparator, csvDecPt )
            ;
            BufStr := BufStr + s2;
            if c <> (ColCount - 1) then
               BufStr := BufStr + Delimiter;
         end;
         Lines.Add( BufStr );
      end;
      result := lines.text;
   finally
      Lines.Free;
   end;
end;

function TARSortGrid.csvText(): string;
begin
  result := delimitedText( csvListSep );
end;

procedure TARSortGrid.SaveToFile( const FileName: String; const Delimiter: Char );
var
   r, c:   LongInt;
   BufStr: String;
   Lines:  TStringList;
begin
   Lines := TStringList.Create;
   try
      Lines.Clear;
      for r := 0 to RowCount - 1 do
      begin
         BufStr := '';
         for c := 0 to ColCount - 1 do
         begin
            BufStr := BufStr + Cells[c, r];
            if c <> (ColCount - 1) then
               BufStr := BufStr + Delimiter;
         end;
         Lines.Add( BufStr );
      end;
      Lines.SaveToFile( FileName );
   finally
      Lines.Free;
   end;
end;

function TARSortGrid.CanUndoSort: Boolean;
begin
   //We can only undo a sort if we still have exactly the
   //same number of rows that we did when we sorted.
   Result := (fSortedList.Count = (RowCount - FixedRows));
   if Result = False then
      fSortedList.Reset;
end;

procedure TARSortGrid.UndoSort;
var
   BufferGrid: TStringGrid;
   Item:       pSortedListEntry;
   i:          Integer;
begin
   if CanUndoSort then
   begin
      BufferGrid := nil;
      try
         BufferGrid := TStringGrid.Create( Self );
         BufferGrid.ColCount := ColCount;
         BufferGrid.RowCount := RowCount;
         //Copy the rows to the buffer grid in the current order.
         for i := FixedRows to RowCount - 1 - gFooterSub do
             BufferGrid.Rows[i].Assign( Rows[i] );
         //Now put the rows back into the original grid in the old order.
         for i := 0 to fSortedList.Count - 1 do
         begin
             Item := fSortedList.GetItem( i );
             Rows[Item^.RowNum].Assign( BufferGrid.Rows[i + FixedRows] );
         end;
      finally
         BufferGrid.Free;
      end;

      //Now put the selection back on the right row.
      Item := fSortedList.GetItem( Row - FixedRows );
      MoveTo( Col, Item^.RowNum );
      //Now reset the list.
      fSortedList.Reset;
   end;
   fSortState := ssUnsorted;    // Line modified/added by gnes@zero.it
   //setSortColumn( -1 ); // Unsorted!
end;

function TARSortGrid.FindFirst(): boolean;
var
  sCol, sRow: longint;
  eCol, eRow: longint;
  aStr: string;
begin
  sCol := 0;
  sRow := 1;
  eCol := self.ColCount - 1;
  eRow := self.RowCount - 1;

  aStr := self.SearchOptions.SearchText;

  Result := findFirst( aStr, sCol, sRow, eCol, eRow );
end;

function TARSortGrid.FindFirst( const aStr: string ): boolean;
begin
  self.SearchOptions.SearchText := aStr;
  result := findFirst();
end;

// Finds the first (sub)string in the grid.
// It searches by row and by column starting from sRow and sCol to eCol and finally eRow.
// Comparisons are made folowing the SortCaseSensitive property.
// If found, returning the cell coordinates in sCol and sRow and return True.
function TARSortGrid.FindFirst( const aStr: String; var sCol, sRow: LongInt; eCol, eRow: LongInt ): Boolean; //RCV02
var
   searchStr: string;
   pStr: String;
   r, c: Integer;
   matchFound: Boolean;
begin
   Result := False;
	if CheckRange( sCol, eCol, false ) and CheckRange( sRow, eRow, true ) then
   begin;
      self.SearchOptions.SearchText := aStr;
	   GEndCol		:= eCol;
	   GEndRow		:= eRow;
      for r := sRow to eRow do
      begin
         for c := sCol to eCol do
         begin
            if FCaseSensitive then
              begin
               pStr := trim( Cells[c, r] );
               searchStr := trim( aStr );
              end
            else
              begin
               pStr := UpperCase( trim( Cells[c, r] ) );
               searchStr := upperCase( trim( aStr ) );
              end;

            if( SearchOptions.PartialMatch ) then
              matchFound := ( {$IfDef VER90}Pos{$Else}AnsiPos{$EndIf}( searchStr, pStr ) > 0 )
            else
              matchFound := ( pStr = SearchStr )
            ;

            if( matchFound ) then
            begin
               sCol := c;
               GStartCol := c;
               sRow := r;
               GStartRow := r;

               if( goRangeSelect in options ) then
                begin
                  fSelectedRows.Clear;
                  selectRow( r, true );
                  refresh();
                end
               ;
               Result := True;
               Exit;
            end;
         end;
      end
   end;
end;

function TARSortGrid.FindNext(): Boolean;
var
  aCol, aRow: longint;
begin
  Result := findNext( aCol, aRow );
end;

function TARSortGrid.FindNext( var aCol, aRow: LongInt ): Boolean; //RCV02
var
   searchStr: string;
   pStr: String;
   r, c: Integer;
   matchFound: Boolean;
begin
   Result := False;
	for r := GStartRow to GEndRow do
   begin
		for c := GStartCol + 1 to GEndCol do
      begin
         if FCaseSensitive then
          begin
            pStr := trim( Cells[c, r] );
            searchStr := trim( self.SearchOptions.SearchText );
          end
         else
          begin
            pStr := UpperCase( trim( Cells[c, r] ) );
            searchStr := upperCase( trim( self.SearchOptions.SearchText ) );
          end;

      if( SearchOptions.PartialMatch ) then
        matchFound := ( {$IfDef VER90}Pos{$Else}AnsiPos{$EndIf}( searchStr, pStr ) > 0 )
      else
        matchFound := ( pStr = SearchStr )
      ;

      if( matchFound ) then
         begin
				aCol := c;
				aRow := r;
            Result := True;
            GStartCol := c;
            GStartRow := r;

            if( goRangeSelect in options ) then
            begin
              fSelectedRows.Clear;
              selectRow( r, true );
              refresh();
            end;

            Exit;
			end;
		end;
      GStartCol := -1;
	end;
   GStartRow := RowCount; // Keep returning False
end;

function TARSortGrid.GetCellDrawState( const aCol, aRow: LongInt ): TGridDrawState;
   function PointInGridRect( Col, Row: LongInt; const Rect: TGridRect ): Boolean;
   begin
     Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top) and (Row <= Rect.Bottom);
   end;
var
   DrawState: TGridDrawState;
begin
   DrawState := [];
   if (aRow < FixedRows) and (aCol < FixedCols) then Include( DrawState, gdFixed );
   if Focused and (aRow = Row) and (aCol = Col) then Include( DrawState, gdFocused );
   if PointInGridRect( aCol, aCol, Selection ) then Include( DrawState, gdSelected );
   Result := DrawState;
end;

function TARSortGrid.SelectCell( aCol, aRow: LongInt ): Boolean;
begin
   Result := inherited SelectCell( aCol, aRow );
end;

procedure TARSortGrid.KeyPress( var Key: Char );
begin
   //I have to do this here because KeyDown doesn't get called
   //when the enter key is pressed in the inplace editor.
   if Key = #13 then
      ValidateCell;
   inherited KeyPress( Key );
end;

procedure TARSortGrid.ClearSelection; //RCV03
begin
   fSelectedRows.Clear;
   fSelectedRows.Add( Pointer( Row ) );
   Refresh;
end;

// IsRC = 1...Rows IsRC = 2...Columns
procedure TARSortGrid.ShowRC( StartRC, EndRC: LongInt; IsRC: Integer ); // RCV04 priv
var
   rc, v: Integer;
begin
   for rc := StartRC to EndRC do
   begin
      if ( ((IsRc = 1) and (RowHeights[ rc ] = 0)) or ((IsRC = 2) and (ColWidths[ rc ] = 0)) ) then
      begin
         v := fOldRowCol.GetValue( rc, IsRC );
         if v <> -1 then
         begin
            if IsRC = 1 then
               RowHeights[ rc ] := v
            else
               ColWidths[ rc ] := v;
            fOldRowCol.RemRC( rc, IsRC );
         end;
      end;
   end;
end;

procedure TARSortGrid.HideRC( StartRC, EndRC: LongInt; IsRC: Integer ); // RCV04 priv
var
   rc: Integer;
begin
   for rc := StartRC to EndRC do
   begin
      if IsRC = 1 then
      begin
         fOldRowCol.SetValue( rc, RowHeights[ rc ], IsRC );
         RowHeights[ rc ] := 0;
      end else
      begin
         fOldRowCol.SetValue( rc, ColWidths[ rc ], IsRC );
         ColWidths[ rc ] := 0;
      end;
   end;
end;

procedure TARSortGrid.ShowRows( StartRow, EndRow: LongInt ); // RCV04
begin
   ShowRC( StartRow, EndRow, 1 );
end;

procedure TARSortGrid.HideRows( StartRow, EndRow: LongInt ); // RCV04
begin
   HideRC( StartRow, EndRow, 1 );
end;

procedure TARSortGrid.AutoSizeRows( StartRow, EndRow: LongInt ); // RCV04
begin
   AutoSizeRowsInt( StartRow, EndRow, asAll );
end;

procedure TARSortGrid.AutoSizeVisibleRows( StartRow, EndRow: LongInt ); // RCV04
begin
   AutoSizeRowsInt( StartRow, EndRow, asVisible );
end;

procedure TARSortGrid.AutoSizeHiddenRows( StartRow, EndRow: LongInt ); // RCV04
begin
   AutoSizeRowsInt( StartRow, EndRow, asHidden );
end;

// TAutoSize: asAll, asVisible, asHidden
procedure TARSortGrid.AutoSizeRowsInt( StartRow, EndRow: LongInt; How: TAutoSize ); // RCV04 priv
var
   r:    Integer;
   Auto: Boolean;
begin
   for r := StartRow to EndRow do
   begin
      Auto := False;
      if How = asAll then Auto := True;
      if (How = asVisible) and (RowHeights[ r ] <> 0) then Auto := True;
      if (How = asHidden) and (RowHeights[ r ] = 0) then Auto := True;
      if Auto = True then AutoSizeRow( r );
   end;
end;

procedure TARSortGrid.AutoSizeRow( aRow: LongInt ); // RCV04
var
   c, maxh, h, v: Integer;
   R: TRect;  s: String;
begin
   maxh := DefaultRowHeight;
   for c := 0 to ColCount - 1 do
   begin
      s := Cells[ c, aRow ];
      R := CellRect( c, aRow );
      DrawText( Canvas.Handle, pChar( s ), -1, R, DT_CALCRECT or DT_WORDBREAK );
      h := R.Bottom - R.Top + 1;
      if h < maxh then maxh := h;
   end;
   v := fOldRowCol.GetValue( aRow, 1 );
   if v <> -1 then
   begin
      fOldRowCol.RemRC( aRow, 1 );
      fOldRowCol.SetValue( aRow, maxh, 1 );
   end else RowHeights[ aRow ] := maxh;
end;

procedure TARSortGrid.ShowCols( StartCol, EndCol: LongInt ); // RCV04
begin
   ShowRC( StartCol, EndCol, 2 );
end;

procedure TARSortGrid.HideCols( StartCol, EndCol: LongInt ); // RCV04
begin
   HideRC( StartCol, EndCol, 2 );
end;

{$IfDef VERD4+}
procedure TARSortGrid.Print; // RCV04
begin
   AutoSizeHiddenRows( 0, RowCount - 1 );
   if Printer.Printers.Count = 0 then
   begin
      MessageDlg( 'No Printer is installed', mtError, [mbOK], 0 );
      Exit;
   end;
   Printer.Title  := PrintOptions.fJobTitle;
   Printer.Copies := PrintOptions.fCopies;
   Printer.Orientation := PrintOptions.Orientation;
   PrintOptions.ToRow := RowCount - 1;
   Printer.BeginDoc;
   //DrawToCanvas( Printer.Canvas, pmPrint, PrintOptions.FromRow, PrintOptions.ToRow );
   DrawToCanvas( Printer.Canvas, pmPrint, 1, RowCount - 1 );
   Printer.EndDoc;
end;

procedure TARSortGrid.PrintPreview; // RCV04
var
   Preview: TARSortGridPreviewForm;
begin
   fPageCount := 0;
   Preview := TARSortGridPreviewForm.Create( Application );
   Preview.Grid := Self;
   fPrintImage := Preview.PrintImage;
   DrawToCanvas( fPrintImage.Canvas, pmPreview, 1, RowCount - 1 );
   Preview.PreviewImage.Picture.Bitmap.Assign( fPrintImage );
   Preview.ShowModal;
   Preview.Free;
end;

procedure TARSortGrid.UpdatePreview( aCanvas: TCanvas ); // RCV04
begin
   fPageCount := 0;
   DrawToCanvas( aCanvas, pmPreview, 1, RowCount - 1 );
end;

function TARSortGrid.PageCount: Integer; // RCV04
begin
   fPageCount := 0;
   DrawToCanvas( nil, pmPageCount, 1, RowCount - 1 );
   Result := fPageCount;
end;

procedure TARSortGrid.DrawToCanvas( aCanvas: TCanvas; Mode: TPrintMode; FromRow, ToRow: LongInt ); // RCV04
var
   PageWidth, PageHeight, PageRow, PageCol, I, iRow, FromCol, ToCol, X, Y: Integer;
   DoPaint, HasLogo:  Boolean;
   Hheader, Hfooter:  Integer;
   LogoPic, LogoPics: TBitmap;

   function ScaleX( I: Integer ): Integer;
   begin
      if Mode = pmPreview then
         Result := I
      else
         Result := Round( I *(GetDeviceCaps( Printer.Handle, LOGPIXELSX ) / Screen.PixelsPerInch ) );
   end;

   function ScaleY( I:Integer ): Integer;
   begin
      if Mode = pmPreview then
         Result := I
      else
         Result := Round( I *(GetDeviceCaps( Printer.Handle, LOGPIXELSY ) / Screen.PixelsPerInch ) );
   end;

   procedure DrawCells( iRow: Integer );
   var
      iCol, I: Integer;
      R:       TRect;
      drs:     String;
      aState:  TGridDrawState;
      FmtOpts: TFormatOptions;
      xOffset: Cardinal;
      yOffset: Cardinal;
   begin
      if DoPaint then
      begin
         Canvas.Font.Assign( fOldFont );
         Canvas.Brush.Assign( fOldBrush );
      end;
      for iCol := FromCol to ToCol do
      begin
         if ColWidths[ iCol ] <> 0 then
         begin
            InitializeFormatOptions( iCol, iRow, FmtOpts );
            if DoPaint then
            begin
               if (iRow >= FixedRows) and (iRow <= RowCount - 1 - fFooterRows) then
                  Canvas.Brush.Color := Color;
               if (iRow < FixedRows) or ((iCol < FixedCols) and (iRow <= RowCount - 1 - fFooterRows)) then
                  Canvas.Brush.Color := FixedColor;
            end;
            // Now do the OnGetCellFormat event if necessary.
            if Assigned( fOnGetCellFormat ) then
               fOnGetCellFormat( Self, iCol, iRow, aState, FmtOpts );

            case FmtOpts.AlignmentHorz of
               taRightJustify:
                  xOffset := DT_RIGHT;
               taCenter:
                  xOffset := DT_CENTER;
               else
                  xOffset := DT_LEFT;
            end;

            case FmtOpts.AlignmentVert of
               taBottomJustify:
                  yOffset := DT_BOTTOM;
               taMiddle:
                  yOffset := DT_VCENTER;
               else
                  yOffset := DT_TOP;
            end;
            if DoPaint then
            begin
               aCanvas.Brush.Assign( FmtOpts.Brush );
               aCanvas.Font.Assign( FmtOpts.Font );
            end;

            // X Offset
            X := ScaleX( PrintOptions.Marginleft );
            for I := FromCol to iCol - 1 do
               Inc( X, ScaleX( ColWidths[ I ]+ 1) );
            // Text Rect
            R := Rect( X, Y, X + ScaleX( ColWidths[ iCol ] ), Y + ScaleY( RowHeights[ iRow ] ) );
            // Draw on the Canvas
            if DoPaint then
            begin
               aCanvas.Brush.Color := FmtOpts.Brush.Color;
               aCanvas.FillRect( Rect( r.Left, r.Top, r.Right + ScaleX( 2 ), r.Bottom + ScaleY( 1 ) ) );
               if PrintOptions.BorderStyle = bsSingle then
               begin
                  aCanvas.Brush.Style := bsClear;
                  aCanvas.Rectangle( r.Left, r.Top, r.Right + ScaleX( 2 ), r.Bottom + ScaleY( 1 ) );
               end;
               drs := Cells[ iCol, iRow ];
               R.Left := R.Left + ScaleX( PrintOptions.LeftPadding );
               if ( FWordWrap and (iCol < FixedCols ) and (iRow < FixedRows) ) then
                  DrawText( aCanvas.Handle, pChar( drs ), -1, R, DT_SINGLELINE or xOffset or yOffset )
               else
                  DrawText( aCanvas.Handle, pChar( drs ), -1, R, DT_WORDBREAK or xOffset );
            end;
         end;
      end;
      Inc( Y, ScaleY( RowHeights[ iRow ] ) );
   end;

   procedure DrawTitle; // Draw Header and Footer
   var
      S, fstr: String;
      flist:   TStringList;
      i:       Integer;
      tmpfont: TFont; // I have no idea why you can't use gettextwidth when acanvas = printer.canvas, it returns wrong value
   begin
      tmpfont := nil;
      if DoPaint then
      begin
        aCanvas.Font.Size := fPrintOptions.HeaderSize;
        tmpfont := Font;
        Canvas.Font := aCanvas.Font;
      end;
      // Title
      Y := ScaleY( PrintOptions.MarginTop );
      S := PrintOptions.PageTitle;
      HHeader := Canvas.TextHeight( S );
      if HasLogo then if LogoPic.Height > HHeader then HHeader := LogoPic.Height;
      if DoPaint then
      begin
         if HasLogo then
            aCanvas.Draw( ScaleX( PrintOptions.MarginLeft ), Y, LogoPics );
         aCanvas.TextOut( (PageWidth div 2) - (ScaleX( Canvas.TextWidth( S ) div 2 )), Y, S );
      end;
      Y := Y + ScaleY( HHeader );
      // Page nr
      S := 'Page ' + IntToStr( PageRow );
      if (ToCol < ColCount - 1) or (PageCol > 1) then
         S := S + '-' + IntToStr( PageCol );
      fstr := PrintOptions.PageFooter ;
      HFooter := Canvas.TextHeight( fstr );
      if fstr <> '' then if DoPaint then
      begin
         aCanvas.Font.Size := fPrintOptions.FooterSize ;
         Canvas.font := aCanvas.Font;
         HFooter := Canvas.TextHeight( fstr );
         fList := TStringList.Create;
         fList.Text := StringReplace( fstr, '|' , #$0D#$0A, [rfreplaceall] );
         while flist.count < 3 do
            fList.Append( '' );
         for i := 0 to 2 do
         begin
            fList[ i ] := StringReplace( fList[ i ], 'date', formatdatetime( PrintOptions.Dateformat, now ), [] );
            fList[ i ] := StringReplace( fList[ i ], 'time', formatdatetime( PrintOptions.Timeformat, now ), [] );
            fList[ i ] := StringReplace( fList[ i ], 'page', s, [] );
         end;
         // Paint left footer
         if fList[ 0 ] <> '' then
            aCanvas.TextOut( ScaleX( Integer( PrintOptions.MarginLeft ) + Canvas.TextWidth( fList[ 0 ] ) ), PageHeight - ScaleY( Integer( PrintOptions.MarginBottom )+ Canvas.TextHeight( fList[ 0 ] )), fList[ 0 ] );
         // Paint center Footer
         if fList[ 1 ] <> '' then
            aCanvas.TextOut( (PageWidth div 2)-(ScaleX( Canvas.TextWidth( fList[ 1 ] ) ) div 2), PageHeight - ScaleY( Integer( PrintOptions.MarginBottom )+ Canvas.TextHeight(fList[ 1 ] ) ), fList[ 1 ] );
         // Paint right Footer
         if fList[ 2 ] <> '' then
            aCanvas.TextOut( PageWidth - ScaleX( Integer( PrintOptions.MarginRight ) + Canvas.TextWidth( fList[ 2 ])+ 10 ), PageHeight - ScaleY( Integer( PrintOptions.MarginBottom )+ Canvas.TextHeight( fList[ 2 ])), fList[ 2 ] );
          fList.Free;
      end;

      if DoPaint then
      begin
         aCanvas.Font.Size := Font.Size;
         Canvas.Font := tmpfont;
      end;
      Y := Y + ScaleY( PrintOptions.PageTitleMargin );
      DrawCells( 0 );
   end;


begin
   Canvas.Font.Assign( fOldFont );
   Canvas.Brush.Assign( fOldBrush );
   // Page size
   PageWidth  := Printer.PageWidth;
   PageHeight := Printer.PageHeight;
   if Mode = pmPreview then
   begin
      PageWidth  := PageWidth div ((GetDeviceCaps( Printer.Handle, LOGPIXELSX ) div Screen.PixelsPerInch) );
      PageHeight := PageHeight div ((GetDeviceCaps( Printer.Handle, LOGPIXELSY ) div Screen.PixelsPerInch) );
      fPrintImage.Width   := PageWidth;
      fPrintImage.Height  := PageHeight;
      aCanvas.Brush.Color := Color;
      aCanvas.FillRect( Rect( 0, 0, PageWidth, PageHeight ) );
   end;
   HasLogo := False;
   if PrintOptions.Logo <> '' then if FileExists( PrintOptions.Logo ) then
   begin
      LogoPic := TBitmap.Create;
      Logopic.LoadFromFile( PrintOptions.Logo );
      Haslogo := True;
      Logopics := TBitmap.Create;
      Logopics.Width := ScaleX( Logopic.Width );
      Logopics.Height := ScaleY( Logopic.Height );
      Logopic.PixelFormat := pf24bit;
      Logopics.PixelFormat := pf24bit;
      SmoothResize( LogoPic, LogoPics );
   end;

   if Mode <> pmPageCount then
   begin
      aCanvas.Font := Font;
      aCanvas.Font.Color := clBlack;
   end;
   PageCol :=  0;
   FromCol := -1;
   ToCol   := -1;
   // Scan cols
   repeat
      // Scan missing cols
      if FromCol = ToCol then
         Inc( FromCol )
      else
         FromCol := ToCol + 1;
      Inc( ToCol );
      // Get Cols with width that fits page
      X := PrintOptions.MarginLeft;
      for I := FromCol to ColCount - 1 do
      begin
         Inc( X, ScaleX( ColWidths[ I ]+ 1 ) );
         if X <= (PageWidth - Integer( PrintOptions.MarginRight )) then
            ToCol := I;
      end;
      PageRow := 1;
      Inc( PageCol );
      // Mode = PageCount
      Inc( fPageCount );
      // Preview mode
      DoPaint := (((Mode = pmPreview) and (fPageCount = PrintOptions.PreviewPage)) or (Mode = pmPrint));
      // Header & Footer
      DrawTitle;
      // Contents
      iRow := FromRow;
      repeat
         if (Y + ScaleY( RowHeights[ iRow ] ) ) <= (PageHeight - ScaleY( Integer( PrintOptions.MarginBottom ) + 20 + HFooter ) ) then
         begin // Draw contents to canvas
            if RowHeights[ iRow ] <> 0 then
               DrawCells( iRow );
            Inc( iRow );
         end else // New page
         begin
            if (DoPaint = True) and (Mode = pmPreview) then
               Exit;
            if Mode = pmPrint then
               Printer.NewPage;
            Inc( fPageCount ); // Pagecount
            DoPaint := (((Mode = pmPreview) and (fPageCount = PrintOptions.PreviewPage)) or (Mode = pmPrint));
            Inc( PageRow );
            DrawTitle;
         end;
         if (iRow = ToRow + 1) and (ToCol < ColCount - 1) and (Y <= PageHeight - ScaleY( 20 )) then
         begin
            if (DoPaint = True) and (Mode = pmPreview) then
               Exit;
            if Mode = pmPrint then
               Printer.NewPage;
            DrawTitle;
         end;
      until iRow = ToRow + 1;
   until ToCol = ColCount - 1;
   if HasLogo then
   begin
      LogoPic.Free;
      LogoPics.Free;
   end;
end;

procedure TARSortGrid.SmoothResize( var Src, Dst: TBitmap ); // RCV04
var
   x, y, xP, yP,
   yP2, xP2:        Integer;
   Read, Read2:    pByteArray;
   t, z, z2, iz2:   Integer;
   pc:             pBytearray;
   w1, w2, w3, w4:  Integer;
   Col1r, col1g, col1b, Col2r, Col2g, Col2b: Byte;
begin
  xP2 := ((src.Width- 1)shl 15)div Dst.Width;
  yP2 := ((src.Height- 1)shl 15)div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height- 1 do
  begin
    xP := 0;
    Read := src.ScanLine[ yP shr 15 ];
    if yP shr 16 < src.Height- 1 then
      Read2 := src.ScanLine [ yP shr 15 + 1 ]
    else
      Read2 := src.ScanLine [ yP shr 15 ];
    pc := Dst.scanline[ y ];
    z2 := yP and $7FFF;
    iz2 := $8000 - z2;
    for x := 0 to Dst.Width- 1 do
    begin
      t := xP shr 15;
      Col1r := Read[ t * 3 ];
      Col1g := Read[ t * 3 + 1 ];
      Col1b := Read[ t * 3 + 2 ];
      Col2r := Read2[ t * 3 ];
      Col2g := Read2[ t * 3 + 1 ];
      Col2b := Read2[ t * 3 + 2 ];
      z := xP and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc[ x * 3 + 2 ]:= (Col1b * w1 + Read[ (t + 1)* 3 + 2]* w2 + Col2b * w3 + Read2[ (t + 1)* 3 + 2]* w4) shr 15;
      pc[ x * 3 + 1 ] := (Col1g * w1 + Read[(t + 1)* 3 + 1]* w2 + Col2g * w3 + Read2[ (t + 1)* 3 + 1 ]* w4) shr 15;
      pc[ x * 3 ] := (Col1r * w1 + Read2[ (t + 1)* 3 ]* w2 + Col2r * w3 + Read2[ (t + 1)* 3 ]* w4) shr 15;
      Inc( xP, xP2 );
    end;
    Inc( yP, yP2 );
  end;
end;
{$EndIf}


procedure TARSortGrid.setSortEnabled( val: boolean );
begin
  //dbcout( '=== Setting sortEnabled to ' + boolToText( val ) );
  self.fSortEnabled := val;
end;

//-----------------------------------------------------------
// AR 4/22/09.  Based on code from
// http://www.swissdelphicenter.ch/torry/showcode.php?id=349
//-----------------------------------------------------------
procedure TARSortGrid.setCursorPosition( const col, row: integer; pos: integer );
  begin
    // Are the column and row in bounds?
    if( ( 0 > col ) or ( 0 > row ) ) then
      exit
    ;

    if( ( self.ColCount < col - 1 ) or ( self.RowCount < row - 1 ) ) then
      exit
    ;

    self.Col := Col;
    self.Row := Row;

    // Adjust pos as needed.
    if( pos < 0 ) then
      pos := 0
    ;
    if( pos > length( self.Cells[ col, row ] ) ) then
      pos := length( self.Cells[ col, row ] )
    ;
    self.InplaceEditor.SelStart := pos;
  end
;


function TARSortGrid.getCursorPosition(): integer;
  begin
    result := self.InplaceEditor.SelStart;
  end
;


procedure TARSortGrid.lockCell();
  begin
    lockControl( self.InplaceEditor );
  end
;


procedure TARSortGrid.unlockCell();
  begin
    unlockControl( self.InplaceEditor );
  end
;


procedure TARSortGrid.selectCellContents();
  begin
    self.InplaceEditor.SelectAll();
  end
;
//-----------------------------------------------------------


procedure Register;
begin
   RegisterComponents( 'APHIControls', [TARSortGrid] );
end;

End.

