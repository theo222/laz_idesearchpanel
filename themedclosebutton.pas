unit themedclosebutton;

//Large parts of this code are copied from Anchordocking to achieve the same look.

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Types, Forms, Themes, Dialogs;

type

  { TThemedCloseButton }

  TThemedCloseButton = class(TCustomControl)
  private
    fDetails: TThemedElementDetails;
    fPPI: integer;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure SetPreferredSize;
    property PPI:integer read fPPI write fPPI;
  end;

var
  PreferredButtonWidth: integer = -1;
  PreferredButtonHeight: integer = -1;

const
  HardcodedButtonSize: integer = 13;

implementation

{ TThemedCloseButton }

procedure SizeCorrector(var current, recomend: integer);
begin
  if recomend < 0 then
  begin
    if current > 0 then
      recomend := current
    else
      current := HardcodedButtonSize;
  end
  else
  begin
    if current > recomend then
      current := recomend
    else
    begin
      if current > 0 then
        recomend := current
      else
        current := recomend;
    end;
  end;
end;

procedure ButtonSizeCorrector(var w, h: integer);
begin
  SizeCorrector(w, PreferredButtonWidth);
  SizeCorrector(h, PreferredButtonHeight);
end;

constructor TThemedCloseButton.Create(AOwner: TComponent);
var
  Size: TSize;
begin
  inherited Create(AOwner);
  fPPI:=Screen.PixelsPerInch;
  fDetails := ThemeServices.GetElementDetails(
    {$IFDEF LCLWIN32}
    twCloseButtonNormal
    {$ELSE}
    twSmallCloseButtonNormal
    {$ENDIF}
    );
  //Size := ThemeServices.GetDetailSizeForPPI(fDetails, fPPI);
  Size := ThemeServices.GetDetailSize(fDetails);
  Width := Size.cx;
  Height := Size.cy;
end;

procedure TThemedCloseButton.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: boolean);
begin
  //with ThemeServices.GetDetailSizeForPPI(fDetails, fPPI) do
  with ThemeServices.GetDetailSize(fDetails) do
  begin
    PreferredWidth := cx;
    PreferredHeight := cy;
    ButtonSizeCorrector(PreferredWidth, PreferredHeight);
    {$IF defined(LCLGtk2) or defined(Carbon)}
    inc(PreferredWidth,2);
    inc(PreferredHeight,2);
    {$ENDIF}
    PreferredWidth := ScaleDesignToForm(PreferredWidth);
    PreferredHeight := ScaleDesignToForm(PreferredHeight);
  end;
end;

procedure TThemedCloseButton.Paint;
begin
  inherited Paint;
  ThemeServices.DrawElement(Canvas.Handle, fDetails, Rect(0, 0, Width, Height), nil);
end;

procedure TThemedCloseButton.SetPreferredSize;
var
  w, h: integer;
begin
  CalculatePreferredSize(w, h, True);
  Width := w;
  Height := h;
end;

end.
