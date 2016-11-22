{$IFDEF FPC} // 3.0.0 includes TPointF etc at Types.pp ??
{$mode delphi}
{$DEFINE NOUITYPES}
{$DEFINE HASFORMATSETTINGS}

{$ELSE}

{$IF CompilerVersion>23}
{$LEGACYIFEND ON}
{$IFEND}

{$IF CompilerVersion>20}
 {$DEFINE HASTOUCHPROPERTY}
 {$DEFINE HASONGESTURE}
{$IFEND}

{$IF CompilerVersion<=22}
{$DEFINE NOUITYPES}
{$IFEND}

{$IF CompilerVersion>21}
{$DEFINE HASFORMATSETTINGS}
{$IFEND}

{$ENDIF}
