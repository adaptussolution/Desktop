
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Some basic classes for RichView.                }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}


unit RVClasses;

interface

uses Classes, Graphics;

{$I RV_Defs.inc}

type
  {
    List of TObjects. Automatically frees objects on deletion
  }
  TRVList = class (TList)
    public
      procedure Clear; {$IFDEF RICHVIEWDEF4}override;{$ENDIF}
      procedure Delete(Index: Integer);
      procedure DeleteAsPointer(Index: Integer);
      destructor Destroy; override;
  end;
  {
    List of integers
  }
  TRVIntegerList = class(TList)
    private
      function Get(Index: Integer): Integer;
      procedure Put(Index: Integer; const Value: Integer);
    public
      constructor CreateEx(Count, Value: Integer);
      constructor CreateCopy(Source:TRVIntegerList);
      procedure Sort;
      procedure InitWith(Value, Count: Integer);
      procedure Fill(Value: Integer);
      procedure Add(Value: Integer);
      function AddUnique(Value: Integer): Integer;
      procedure Insert(Index, Value: Integer);
      procedure Assign(Source:TRVIntegerList);
      property Items[Index: Integer]: Integer read Get write Put; default;
  end;
  {
    List of TColors
  }
  TRVColorList = class (TRVIntegerList)
    public
      procedure AddUnique(Value: Integer);    
  end;

implementation

function SortIntegers(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(Item1)-Integer(Item2);
end;

{================================= TRVList ====================================}
{ Removes all items from the list. Frees all objects.                          }
procedure TRVList.Clear;
var i: Integer;
begin
   for i := 0 to Count-1 do
     TObject(Items[i]).Free;
   inherited Clear;
end;
{------------------------------------------------------------------------------}
{ Deletes the item with Index. Frees the item's object.                        }
procedure TRVList.Delete(Index: Integer);
begin
  TObject(Items[Index]).Free;
  inherited Delete(Index);
end;
{------------------------------------------------------------------------------}
{ Deletes the item Index without freeing its object.                           }
procedure TRVList.DeleteAsPointer(Index: Integer);
begin
  inherited Delete(Index);
end;
{------------------------------------------------------------------------------}
{ Destructor.                                                                  }
destructor TRVList.Destroy;
begin
  Clear;
  inherited Destroy;
end;
{============================== TRVIntegerList ================================}
{ Adds a new value                                                             }
procedure TRVIntegerList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));
end;
{------------------------------------------------------------------------------}
{ Inserts a new value                                                          }                                                         
procedure TRVIntegerList.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Pointer(Value));
end;
{------------------------------------------------------------------------------}
{ Accessing Items[Index]                                                       }
function TRVIntegerList.Get(Index: Integer): Integer;
begin
  Result := Integer(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
{ Assigning Items[Index]                                                       }
procedure TRVIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Put(Index, Pointer(Value));
end;
{------------------------------------------------------------------------------}
{ Creates a new list with Count items having Value                             }
constructor TRVIntegerList.CreateEx(Count, Value: Integer);
begin
  inherited Create;
  Capacity := Count;
  while Count>0 do begin
    Add(Value);
    dec(Count);
  end;
end;
{------------------------------------------------------------------------------}
{ Creates a copy of Source                                                     }
constructor TRVIntegerList.CreateCopy(Source: TRVIntegerList);
begin
  inherited Create;
  Assign(Source);
end;
{------------------------------------------------------------------------------}
{ Adds Value if it does not exist in the list. In any case, returns its index  }
function TRVIntegerList.AddUnique(Value: Integer): Integer;
begin
  Result := IndexOf(Pointer(Value));
  if Result=-1 then begin
    inherited Add(Pointer(Value));
    Result := Count-1;
  end;
end;
{------------------------------------------------------------------------------}
{ Makes Self a copy of Source.                                                 }
procedure TRVIntegerList.Assign(Source: TRVIntegerList);
var i: Integer;
begin
  Clear;
  Capacity := Source.Count;
  for i := 0 to Source.Count-1 do
    Add(Source.Items[i]);
end;
{------------------------------------------------------------------------------}
{ Adds Count items equal to Value                                              }
procedure TRVIntegerList.InitWith(Value, Count: Integer);
var i: Integer;
begin
  Clear;
  Capacity := Count;
  for i := 0 to Count-1 do
    Add(Value);
end;
{------------------------------------------------------------------------------}
{ Assigns Value to all items                                                   } 
procedure TRVIntegerList.Fill(Value: Integer);
var i: Integer;
begin
  for i := 0 to Count-1 do
    Items[i] := Value;
end;
{------------------------------------------------------------------------------}
{ Ascending sort                                                               }
procedure TRVIntegerList.Sort;
begin
  inherited Sort(SortIntegers);
end;
{================================= TRVColorList ===============================}
{ The same as in TRVIntegerList, but does not allow adding clNone.             }
procedure TRVColorList.AddUnique(Value: Integer);
begin
  if Value<>clNone then
    inherited AddUnique(Value);
end;


end.
