type
  TNotifyEvent = Procedure(sender: TObject);

type
  TMyObject = Class(TObject)
  private
    FEvent: TNotifyEvent;
    Procedure setEvent(Value: TNotifyEvent);
  public
    Property OnClick: TNotifyEvent read FEvent write setEvent;
    procedure Trigger;
  End;

Procedure TMyObject.setEvent(Value: TNotifyEvent);
Begin
  FEvent := Value;
end;

procedure TMyObject.Trigger;
begin
  if Assigned(FEvent) then
    FEvent(Self);
end;

Procedure HandleEvent(sender: TObject);
Begin
  PrintLn('event triggered');
  PrintLn(sender.ClassName);
end;

var
  mTemp: TMyObject;
mTemp := TMyObject.Create;
mTemp.Trigger;
mTemp.OnClick := HandleEvent;
// <--Throws compiler error "More arguments expected"
mTemp.Trigger;
