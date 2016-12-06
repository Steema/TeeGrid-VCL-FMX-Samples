//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit_Basic.h"
#include "Tee.Painter.hpp"
#include "VCLTee.Editor.Grid.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "VCLTee.Control"
#pragma link "VCLTee.Grid"
#pragma link "VCLTee.Editor.Grid"
#pragma link "Tee.Grid.Data.Strings"
#pragma resource "*.dfm"
TFormTeeGridCppBasic *FormTeeGridCppBasic;
//---------------------------------------------------------------------------
__fastcall TFormTeeGridCppBasic::TFormTeeGridCppBasic(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TFormTeeGridCppBasic::FormCreate(TObject *Sender)
{
  Data = new TStringsData(10,20);

  TeeGrid1->Data = Data;

  for (int i=0; i < Data->Columns; i++) {
    Data->Headers[i] = IntToStr(i);

    TColumn *col = TeeGrid1->Columns->Items[i];

    col->TextAlignment=Custom;
    col->TextAlign->Horizontal=THorizontalAlign::Center;
  }

  for (int col=0; col < Data->Columns; col++) {
    for (int row=0; row < Data->Rows; row++) {
      Data->Cells[col][row] = "Hello";
    }
  }

//  TeeGrid1->Rows->Height->Automatic = true;
}
//---------------------------------------------------------------------------

void __fastcall TFormTeeGridCppBasic::Button1Click(TObject *Sender)
{
  TTeeGridEditor::Edit(this,TeeGrid1);
}
//---------------------------------------------------------------------------

