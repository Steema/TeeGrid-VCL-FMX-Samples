//---------------------------------------------------------------------------

#ifndef Unit_BasicH
#define Unit_BasicH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "VCLTee.Control.hpp"
#include "VCLTee.Grid.hpp"
#include "Tee.GridData.Strings.hpp"
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TFormTeeGridCppBasic : public TForm
{
__published:	// IDE-managed Components
	TTeeGrid *TeeGrid1;
	TPanel *Panel1;
	TButton *Button1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
private:	// User declarations
        TStringsData *Data;
public:		// User declarations
	__fastcall TFormTeeGridCppBasic(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormTeeGridCppBasic *FormTeeGridCppBasic;
//---------------------------------------------------------------------------
#endif
