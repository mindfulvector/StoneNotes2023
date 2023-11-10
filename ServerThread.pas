unit ServerThread;

interface

uses
  System.SysUtils,
  IdContext,
  WebSocketServer,
  System.Classes;

type
  TServerThread = class(TThread)
  private
    FServer: TWebSocketServer;
    procedure HandleConnect(AContext: TIdContext);
    procedure HandleDisconnect(AContext: TIdContext);
    procedure HandleExecute(AContext: TIdContext);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartServer;
    procedure StopServer;
  end;

implementation

uses
  Logger, MainForm;

constructor TServerThread.Create;
begin
  inherited Create(True); // Suspended by default
  FServer := TWebSocketServer.Create;
  FServer.DefaultPort := 64768;
  FServer.OnExecute := Self.HandleExecute;
  FServer.OnConnect := Self.HandleConnect;
  FServer.OnDisconnect := Self.HandleDisconnect;
end;

destructor TServerThread.Destroy;
begin
  FServer.Active := false;
  FServer.DisposeOf;

  inherited;
end;

procedure TServerThread.HandleConnect(AContext: TIdContext);
begin
  Synchronize(
    procedure
    begin
      Log('Client connected');
    end
  );
end;

procedure TServerThread.HandleDisconnect(AContext: TIdContext);
begin
  Synchronize(
    procedure
    begin
      Log('Client disconnected');
    end
  );
end;

procedure TServerThread.HandleExecute(AContext: TIdContext);
var
  io: TWebSocketIOHandlerHelper;
  msg: string;
  Response: string;
begin
  io := TWebSocketIOHandlerHelper(AContext.Connection.IOHandler);
  io.CheckForDataOnSource(10);
  msg := io.ReadString;
  if msg = '' then
    Exit;

  Synchronize(
    procedure
    begin
      Log(msg);
    end
  );

  TThread.Queue(nil,
    procedure
    begin
      Response := frmStoneNotes.ProcessJSONMessage(msg);
    end
  );

  io.WriteString(Response);
end;

procedure TServerThread.Execute;
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('Server thread started');
    end
  );
  while not Terminated do
  begin
    Sleep(100);
  end;
    TThread.Queue(nil,
    procedure
    begin
      Log('Server thread exit');
    end
  );
end;

procedure TServerThread.StartServer;
begin
  TThread.Queue(nil,
    procedure
    begin
      Log('Starting server');
    end
  );
  FServer.Active := true;
  Start; // Starts the thread
end;

procedure TServerThread.StopServer;
begin
  Terminate;
  FServer.Active := false;
end;

end.

