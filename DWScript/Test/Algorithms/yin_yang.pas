//
// New Script
//

type
   TColorFuncX = function (x : Integer) : Integer;

type
   TSquareBoard = class
      Scale : Integer;
      Pix : array of array of Integer;

      constructor Create(aScale : Integer);
      begin
         Scale := aScale;
         Pix := new Integer[aScale*12+1, aScale*12+1];
      end;

      method DoPrint;
      begin
         var i, j : Integer;
         for i:=0 to Pix.High do begin
            for j:=0 to Pix.High do begin
               case Pix[j, i] of
                  1 : Print('.');
                  2 : Print('#');
               else
                  Print(' ');
               end;
            end;
            PrintLn('');
         end;
      end;

      method DrawCircle(cx, cy, cr : Integer; color : TColorFuncX);
      begin
         var rr := Sqr(cr*Scale);
         var x, y : Integer;
         for x := 0 to Pix.High do begin
            for y := 0 to Pix.High do begin
               if Sqr(x-cx*Scale)+Sqr(y-cy*Scale)<=rr then
                  Pix[x, y] := color(x);
            end;
         end;
      end;

      method ColorHalf(x : Integer) : Integer;
      begin
         if (x<6*Scale) then
            Result:=1
         else Result:=2;
      end;

      method ColorYin(x : Integer) : Integer;
      begin
         Result:=2;
      end;

      method ColorYang(x : Integer) : Integer;
      begin
         Result:=1;
      end;

      method YinYang;
      begin
         DrawCircle(6, 6, 6, ColorHalf);
         DrawCircle(6, 3, 3, ColorYang);
         DrawCircle(6, 9, 3, ColorYin);
         DrawCircle(6, 9, 1, ColorYang);
         DrawCircle(6, 3, 1, ColorYin);
      end;

   end;

var sq := new TSquareBoard(2);
sq.YinYang;
sq.DoPrint;

sq := new TSquareBoard(1);
sq.YinYang;
sq.DoPrint;

