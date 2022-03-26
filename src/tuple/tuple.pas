unit Tuple;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, nonetype;

type

  { TPair }

  generic TPair<TFirst, TSecond> = record
  public type
    TSpecializedPair = specialize TPair<TFirst, TSecond>;
  public
    First: TFirst;
    Second: TSecond;
  private
    class operator Initialize(var pair: TSpecializedPair);
  public
    constructor Create(constref AFirst: TFirst; constref ASecond: TSecond);

    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType);
  end;

  { TTriple }

  generic TTriple<TFirst, TSecond, TThird> = record
  public type
    TSpecializedTriple = specialize TTriple<TFirst, TSecond, TThird>;
  public
    First: TFirst;
    Second: TSecond;
    Third: TThird;
  private
    class operator Initialize(var triple: TSpecializedTriple);
  public
    constructor Create(constref AFirst: TFirst; constref ASecond: TSecond;
                       constref AThird: TThird);

    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType);
  end;

  { TQuadruple }

  generic TQuadruple<TFirst, TSecond, TThird, TFourth> = record
  public type
    TSpecializedQuadruple = specialize TQuadruple<TFirst, TSecond, TThird, TFourth>;
  public
    First: TFirst;
    Second: TSecond;
    Third: TThird;
    Fourth: TFourth;
  private
    class operator Initialize(var quadruple: TSpecializedQuadruple);
  public
    constructor Create(constref AFirst: TFirst; constref ASecond: TSecond;
                       constref AThird: TThird; constref AFourth: TFourth);

    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird; out AFourth: TFourth);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird; out AFourth: TFourth);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird; out AFourth: TFourth);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird; out AFourth: TFourth);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType; out AFourth: TFourth);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType; out AFourth: TFourth);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType; out AFourth: TFourth);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType; out AFourth: TFourth);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird; const AFourth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird; const AFourth: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird; const AFourth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird; const AFourth: TNoneType);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType; const AFourth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType; const AFourth: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType; const AFourth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType; const AFourth: TNoneType);
  end;

  { TQuintuple }

  generic TQuintuple<TFirst, TSecond, TThird, TFourth, TFifth> = record
  public type
    TSpecializedQuintuple = specialize TQuintuple<TFirst, TSecond, TThird,
                                                  TFourth, TFifth>;
  public
    First: TFirst;
    Second: TSecond;
    Third: TThird;
    Fourth: TFourth;
    Fifth: TFifth;
  private
    class operator Initialize(var quintuple: TQuintuple);
  public
    constructor Create(constref AFirst: TFirst; constref ASecond: TSecond;
                       constref AThird: TThird; constref AFourth: TFourth;
                       constref AFifth: TFifth);

    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType; out AFourth: TFourth;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     out AFifth: TFifth);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType; out AFourth: TFourth;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     out AThird: TThird; const AFourth: TNoneType;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     out AThird: TThird; const AFourth: TNoneType;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     out AThird: TThird; const AFourth: TNoneType;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     out AThird: TThird; const AFourth: TNoneType;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; out ASecond: TSecond;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     const AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; out ASecond: TSecond;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     const AFifth: TNoneType);
    procedure Unpack(out AFirst: TFirst; const ASecond: TNoneType;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     out AFifth: TNoneType);
    procedure Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
                     const AThird: TNoneType; const AFourth: TNoneType;
                     const AFifth: TNoneType);
  end;

generic function Pair<TFirst, TSecond>(constref AFirst: TFirst; constref ASecond: TSecond):
  specialize TPair<TFirst, TSecond>; inline;
generic function Triple<TFirst, TSecond, TThird>(
  constref AFirst: TFirst; constref ASecond: TSecond; constref AThird: TThird):
  specialize TTriple<TFirst, TSecond, TThird>; inline;
generic function Quadruple<TFirst, TSecond, TThird, TFourth>(
  constref AFirst: TFirst; constref ASecond: TSecond; constref AThird: TThird;
  constref AFourth: TFourth): specialize TQuadruple<TFirst, TSecond, TThird, TFourth>; inline;
generic function Quintuple<TFirst, TSecond, TThird, TFourth, TFifth>(
  constref AFirst: TFirst; constref ASecond: TSecond; constref AThird: TThird;
  constref AFourth: TFourth; constref AFifth: TFifth):
  specialize TQuintuple<TFirst, TSecond, TThird, TFourth, TFifth>; inline;

implementation

generic function Pair<TFirst, TSecond>(constref AFirst: TFirst; constref ASecond: TSecond):
  specialize TPair<TFirst, TSecond>;
begin
  Result := specialize TPair<TFirst, TSecond>.Create(AFirst, ASecond);
end;

generic function Triple<TFirst, TSecond, TThird>(
  constref AFirst: TFirst; constref ASecond: TSecond; constref AThird: TThird):
  specialize TTriple<TFirst, TSecond, TThird>;
begin
  Result := specialize TTriple<TFirst, TSecond, TThird>.Create(AFirst, ASecond, AThird);
end;

generic function Quadruple<TFirst, TSecond, TThird, TFourth>(
  constref AFirst: TFirst; constref ASecond: TSecond; constref AThird: TThird;
  constref AFourth: TFourth): specialize TQuadruple<TFirst, TSecond, TThird, TFourth>;
begin
  Result := specialize TQuadruple<TFirst, TSecond, TThird, TFourth>.Create(
    AFirst, ASecond, AThird, AFourth);
end;

generic function Quintuple<TFirst, TSecond, TThird, TFourth, TFifth>(
  constref AFirst: TFirst; constref ASecond: TSecond; constref AThird: TThird;
  constref AFourth: TFourth; constref AFifth: TFifth):
  specialize TQuintuple<TFirst, TSecond, TThird, TFourth, TFifth>;
begin
  Result := specialize TQuintuple<TFirst, TSecond, TThird, TFourth, TFifth>.Create(
    AFirst, ASecond, AThird, AFourth, AFifth);
end;

{ TQuintuple }

class operator TQuintuple.Initialize(var quintuple: TQuintuple);
begin
  quintuple.First := Default(TFirst);
  quintuple.Second := Default(TSecond);
  quintuple.Third := Default(TThird);
  quintuple.Fourth := Default(TFourth);
  quintuple.Fifth := Default(TFifth);
end;

constructor TQuintuple.Create(constref AFirst: TFirst; constref
  ASecond: TSecond; constref AThird: TThird; constref AFourth: TFourth;
  constref AFifth: TFifth);
begin
  First := AFirst;
  Second := ASecond;
  Third := AThird;
  Fourth := AFourth;
  Fifth := AFifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird; out AFourth: TFourth; out AFifth: TFifth);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird; out AFourth: TFourth; out AFifth: TFifth);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird; out AFourth: TFourth; out AFifth: TFifth);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  out AThird: TThird; out AFourth: TFourth; out AFifth: TFifth);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType; out AFourth: TFourth; out AFifth: TFifth);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType; out AFourth: TFourth; out AFifth: TFifth);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType; out AFourth: TFourth; out AFifth: TFifth);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType; out AFourth: TFourth; out AFifth: TFifth);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird; const AFourth: TNoneType; out AFifth: TFifth);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird; const AFourth: TNoneType; out AFifth: TFifth);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird; const AFourth: TNoneType; out AFifth: TFifth);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  out AThird: TThird; const AFourth: TNoneType; out AFifth: TFifth);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType; const AFourth: TNoneType; out AFifth: TFifth);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType; const AFourth: TNoneType; out AFifth: TFifth);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType; const AFourth: TNoneType; out AFifth: TFifth);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType; const AFourth: TNoneType; out AFifth: TFifth);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird; out AFourth: TFourth; const AFifth: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird; out AFourth: TFourth; const AFifth: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird; out AFourth: TFourth; const AFifth: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  out AThird: TThird; out AFourth: TFourth; const AFifth: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType; out AFourth: TFourth; const AFifth: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType; out AFourth: TFourth; const AFifth: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType; out AFourth: TFourth; const AFifth: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType; out AFourth: TFourth; const AFifth: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  out AThird: TThird; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

procedure TQuintuple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType; const AFourth: TNoneType; out AFifth: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  AFifth := Fifth;
end;

procedure TQuintuple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType; const AFourth: TNoneType; const AFifth: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
  //AFifth := Fifth;
end;

{ TQuadruple }

class operator TQuadruple.Initialize(var quadruple: TSpecializedQuadruple);
begin
  quadruple.First := Default(TFirst);
  quadruple.Second := Default(TSecond);
  quadruple.Third := Default(TThird);
  quadruple.Fourth := Default(TFourth);
end;

constructor TQuadruple.Create(constref AFirst: TFirst; constref
  ASecond: TSecond; constref AThird: TThird; constref AFourth: TFourth);
begin
  First := AFirst;
  Second := ASecond;
  Third := AThird;
  Fourth := AFourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird; out AFourth: TFourth);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird; out AFourth: TFourth);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird; out AFourth: TFourth);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  out AThird: TThird; out AFourth: TFourth);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType; out AFourth: TFourth);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType; out AFourth: TFourth);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType; out AFourth: TFourth);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType; out AFourth: TFourth);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird; const AFourth: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird; const AFourth: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird; const AFourth: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  out AThird: TThird; const AFourth: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType; const AFourth: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType; const AFourth: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType; const AFourth: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
end;

procedure TQuadruple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType; const AFourth: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
  //AFourth := Fourth;
end;

{ TTriple }

class operator TTriple.Initialize(var triple: TSpecializedTriple);
begin
  triple.First := Default(TFirst);
  triple.Second := Default(TSecond);
  triple.Third := Default(TThird);
end;

constructor TTriple.Create(constref AFirst: TFirst; constref ASecond: TSecond;
  constref AThird: TThird);
begin
  First := AFirst;
  Second := ASecond;
  Third := AThird;
end;

procedure TTriple.Unpack(out AFirst: TFirst; out ASecond: TSecond; out
  AThird: TThird);
begin
  AFirst := First;
  ASecond := Second;
  AThird := Third;
end;

procedure TTriple.Unpack(const AFirst: TNoneType; out ASecond: TSecond; out
  AThird: TThird);
begin
  //AFirst := First;
  ASecond := Second;
  AThird := Third;
end;

procedure TTriple.Unpack(out AFirst: TFirst; const ASecond: TNoneType; out
  AThird: TThird);
begin
  AFirst := First;
  //ASecond := Second;
  AThird := Third;
end;

procedure TTriple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType; out
  AThird: TThird);
begin
  //AFirst := First;
  //ASecond := Second;
  AThird := Third;
end;

procedure TTriple.Unpack(out AFirst: TFirst; out ASecond: TSecond;
  const AThird: TNoneType);
begin
  AFirst := First;
  ASecond := Second;
  //AThird := Third;
end;

procedure TTriple.Unpack(const AFirst: TNoneType; out ASecond: TSecond;
  const AThird: TNoneType);
begin
  //AFirst := First;
  ASecond := Second;
  //AThird := Third;
end;

procedure TTriple.Unpack(out AFirst: TFirst; const ASecond: TNoneType;
  const AThird: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
  //AThird := Third;
end;

procedure TTriple.Unpack(const AFirst: TNoneType; const ASecond: TNoneType;
  const AThird: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
  //AThird := Third;
end;

{ TPair }

class operator TPair.Initialize(var pair: TSpecializedPair);
begin
  pair.First := Default(TFirst);
  pair.Second := Default(TSecond);
end;

constructor TPair.Create(constref AFirst: TFirst; constref ASecond: TSecond);
begin
  First := AFirst;
  Second := ASecond;
end;

procedure TPair.Unpack(out AFirst: TFirst; out ASecond: TSecond);
begin
  AFirst := First;
  ASecond := Second;
end;

procedure TPair.Unpack(const AFirst: TNoneType; out ASecond: TSecond);
begin
  //AFirst := First;
  ASecond := Second;
end;

procedure TPair.Unpack(out AFirst: TFirst; const ASecond: TNoneType);
begin
  AFirst := First;
  //ASecond := Second;
end;

procedure TPair.Unpack(const AFirst: TNoneType; const ASecond: TNoneType);
begin
  //AFirst := First;
  //ASecond := Second;
end;

end.

