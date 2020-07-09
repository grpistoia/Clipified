unit UEvaluatorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Math,
  ExtCtrls, Menus, fpexprpars, Clipbrd, Buttons;

type

  { TEvaluatorForm }

  TEvaluatorForm = class(TForm)
    OkButton: TBitBtn;
    EditX: TEdit;
    EditResult: TEdit;
    EditY: TEdit;
    LabelA: TLabel;
    LabelResult: TLabel;
    LabelB: TLabel;
    MemoExpression: TMemo;
    PopupFormulasDynamic: TMenuItem;
    CancelButton: TBitBtn;
    PopupExpressions: TPopupMenu;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure OkButtonClick(Sender: TObject);
    procedure EditXChange(Sender: TObject);
    procedure EditYChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoExpressionChange(Sender: TObject);
    procedure OnPopupExpressionClick(Sender: TObject);
    procedure PopupExpressionsClose(Sender: TObject);
    procedure PopupExpressionsPopup(Sender: TObject);
  private
    { private declarations }
    procedure Calculate;
    function  GetExpression(): string;
    procedure SetExpression(NewExpression: string);
  public
    { public declarations }
    property Expression: string read GetExpression write SetExpression;
  end;

implementation

{$R *.lfm}

type
    { To hold the Expressions }
    TStandardExpressionData = record
        Name   : string;
        Expression: string;
    end;

const

    // put them in a list..
    AllExpressions : array[1..16] of TStandardExpressionData = (
        // Percentage
        (Name: 'Percentage';          Expression: 'x * y / 100'),
        // %Increase
        (Name: 'Percentage Increase'; Expression: '(x - y) / y * 100'),
        // Simple interest
        (Name: 'Simple Interest';     Expression: 'x + (x * y / 100)'),
        // Compound interest
        (Name: 'Compound Interest';   Expression: 'x * power(1 + y / 100, 12)'),
        // Other functions for reference..
        (Name: 'Round';               Expression: 'round(x)'),
        (Name: 'Trunc';               Expression: 'trunc(x)'),
        (Name: 'Abs';                 Expression: 'abs(x)'),
        (Name: 'Exp';                 Expression: 'exp(x)'),
        (Name: 'Ln';                  Expression: 'ln(x)'),
        (Name: 'Log';                 Expression: 'log(x)'),
        (Name: 'Cos';                 Expression: 'cos(x)'),
        (Name: 'Sin';                 Expression: 'sin(x)'),
        (Name: 'x^y';                 Expression: 'power(x, y)'),
        (Name: 'x^2';                 Expression: 'sqr(x)'),
        (Name: '2√x';                 Expression: 'sqrt(x)'),
        // For reference..
        (Name: '(x - y)^2';           Expression: 'sqr(x) - 2*x*y + sqr(y)')
    );

{ TEvaluatorForm }

procedure TEvaluatorForm.FormCreate(Sender: TObject);
var iterator : Integer;
    tempData : TStandardExpressionData;
    tempMenu : TMenuItem;
begin
    // Move it
    Self.Top := Application.MainForm.Top + 50;
    Self.Left := Application.MainForm.Left;
    // Delete the temp one..
    PopupExpressions.Items.Delete(0);
    // Create them on the fly...
    for iterator := 1 to Length(AllExpressions) do begin
        // The item to map...
        tempData := AllExpressions[iterator];
        // Create a menu on the fly..
        tempMenu := TMenuItem.Create(Self);
        tempMenu.Caption := tempData.Name;
        tempMenu.Tag     := iterator;
        tempMenu.OnClick := @OnPopupExpressionClick;
        // add the item..
        PopupExpressions.Items.Add(tempMenu);
    end;
    // Select the first one by default...
    MemoExpression.Lines.Text := AllExpressions[1].Expression;
end;

function  TEvaluatorForm.GetExpression(): string;
begin
    // Kept in one place
    result := MemoExpression.Lines.Text;
end;

procedure TEvaluatorForm.SetExpression(NewExpression: string);
begin
    // Keep in one place..
    MemoExpression.Lines.Clear;
    MemoExpression.Lines.Text := NewExpression;
end;

procedure TEvaluatorForm.PopupExpressionsPopup(Sender: TObject);
var iterator : Integer;
    tempData : TStandardExpressionData;
    tempMenu : TMenuItem;
begin
    // Otherwise in Windows the menu disappear
    Self.FormStyle := fsNormal;

    // Check on the fly..
    for iterator := 0 to PopupExpressions.Items.Count - 1 do begin
        // Which one.. (in case I change order or.. whatever)..
        tempMenu := PopupExpressions.Items[iterator];
        // The item to map...
        tempData := AllExpressions[tempMenu.Tag];
        // Show off.
        tempMenu.Checked := (Trim(MemoExpression.Text) = tempData.Expression);
    end;
end;

procedure TEvaluatorForm.PopupExpressionsClose(Sender: TObject);
begin
    // Make this Windows on top again
    Self.FormStyle := fsSystemStayOnTop;
end;

procedure TEvaluatorForm.OnPopupExpressionClick(Sender: TObject);
var tempData : TStandardExpressionData;
begin
    tempData := AllExpressions[TMenuItem(Sender).Tag];
    MemoExpression.Lines.Text := tempData.Expression;
end;

procedure TEvaluatorForm.FormShow(Sender: TObject);
begin
    // produce a result
    Calculate();
end;

procedure TEvaluatorForm.EditXChange(Sender: TObject);
begin
    Calculate();
end;

procedure TEvaluatorForm.OkButtonClick(Sender: TObject);
var result : Extended;
begin
    // Has something.. copy..
    result := 0.0;
    // Returns zero if fails.. so I ignore that number
    TextToFloat(PChar(EditResult.Text), result);
    // when it has something.. copy it..
    if (result <> 0.0) then Clipboard.AsText := EditResult.Text;
end;

procedure TEvaluatorForm.FormKeyPress(Sender: TObject; var Key: char);
begin
    case Key of
        #13: begin
                // bye
                OkButtonClick(Sender);
             end;
        #27: begin
                // bye
                ModalResult := mrCancel;
             end;
    end;
end;

procedure TEvaluatorForm.EditYChange(Sender: TObject);
begin
    Calculate();
end;

procedure TEvaluatorForm.MemoExpressionChange(Sender: TObject);
begin
    Calculate();
end;

procedure TEvaluatorForm.Calculate;
var parser: TFPExpressionParser;
    resultValue: Double;
    valueX : Extended;
    valueY : Extended;
    cachedText : string;
begin
    try
        // processing..
        cachedText := MemoExpression.Lines.Text;
        // Improvements...
        cachedText := Lowercase(cachedText);

        // parser bit..
        parser := TFPExpressionParser.Create(Self);
        try

            try
                // is mathematiall.
                parser.BuiltIns   := [bcMath];

                // set up variables..
                valueX := 0.0;
                TextToFloat(PChar(EditX.Text), valueX);
                parser.Identifiers.AddFloatVariable('x', valueX);

                // set up variables..
                valueY := 0.0;
                TextToFloat(PChar(EditY.Text), valueY);
                parser.Identifiers.AddFloatVariable('y', valueY);

                // get it now.. hopefully..
                // the expression must be forced to decimal with this trick..
                parser.Expression := '0.0 + (' + cachedText + ')'; // If I dont use this calculates weird..

                // Evaluate...
                resultValue := parser.Evaluate.ResFloat;

                // Notified
                EditResult.Text := FloatToStr(resultValue);

            except
                // Show the problem..
                EditResult.Text := 'Syntax error';

            end;

        finally
            FreeAndNil(parser);

        end;

    except
        EditResult.Text := 'Error';

    end;

end;

procedure BuiltPower(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
    Result.resFloat := Power(ArgToFloat(Args[0]), ArgToFloat(Args[1]));
end;

initialization

    // Shame this was missing..
    BuiltinIdentifiers.AddFunction(bcMath, 'power', 'F', 'FF', @BuiltPower);

end.

